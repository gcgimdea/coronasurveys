# Copyright 2020 (c) Cognizant Digital Business, Evolutionary AI. All rights reserved. Issued under the Apache 2.0 License.

#
# Example script for training neat-based prescriptors
# Uses neat-python: pip install neat-python
#
import os
from copy import deepcopy

import numpy as np
import pandas as pd
from pathlib import Path

# Suppress noisy Tensorflow debug logging
os.environ['TF_CPP_MIN_LOG_LEVEL'] = '3'

# noinspection PyPep8Naming
import keras.backend as K
import numpy as np
import pandas as pd
from keras.callbacks import EarlyStopping
from keras.constraints import Constraint
from keras.layers import Dense
from keras.layers import Input
from keras.layers import LSTM
from keras.layers import Lambda
from keras.models import Model

import sys

ups = '/..' * 2
root_path = os.path.dirname(os.path.realpath(__file__)) + ups
sys.path.append(root_path)

from .utils import PRED_CASES_COL, prepare_historical_df, CASES_COL, IP_COLS, \
    IP_MAX_VALUES, add_geo_id, get_predictions, HIST_DATA_FILE_PATH

# Cutoff date for training data
from .cost_generator import generate_costs

# Path where this script lives
ROOT_DIR = Path(os.path.dirname(os.path.abspath(__file__)))

CUTOFF_DATE = '2020-07-31'

# Range of days the prescriptors will be evaluated on.
# To save time during training, this range may be significantly
# shorter than the maximum days a prescriptor can be evaluated on.
EVAL_START_DATE = '2020-08-01'
EVAL_END_DATE = '2020-08-31'

# Number of days the prescriptors will look at in the past.
# Larger values here may make convergence slower, but give
# prescriptors more context. The number of inputs of each neat
# network will be NB_LOOKBACK_DAYS * (IP_COLS + 1) + IP_COLS.
# The '1' is for previous case data, and the final IP_COLS
# is for IP cost information.
NB_LOOKBACK_DAYS = 21

# Number of countries to use for training. Again, lower numbers
# here will make training faster, since there will be fewer
# input variables, but could potentially miss out on useful info.
NB_EVAL_COUNTRIES = 1

WINDOW_SIZE = 14

# Load historical data with basic preprocessing
print("Loading historical data...")
df = prepare_historical_df()

# Restrict it to dates before the training cutoff
cutoff_date = pd.to_datetime(CUTOFF_DATE, format='%Y-%m-%d')
df = df[df['Date'] <= cutoff_date]

# As a heuristic, use the top NB_EVAL_COUNTRIES w.r.t. ConfirmedCases
# so far as the geos for evaluation.
eval_geos = list(df.groupby('GeoID').max()['ConfirmedCases'].sort_values(
    ascending=False).head(NB_EVAL_COUNTRIES).index)
print("Nets will be evaluated on the following geos:", eval_geos)

# Pull out historical data for all geos
past_cases = {}
past_ips = {}
geo_id = None
for geo in eval_geos:
    geo_id = geo
    geo_df = df[df['GeoID'] == geo]
    past_cases[geo] = np.maximum(0, np.array(geo_df['ConfirmedCases']))
    past_ips[geo] = np.array(geo_df[IP_COLS])

# Gather values for scaling network output
ip_max_values_arr = np.array([IP_MAX_VALUES[ip] for ip in IP_COLS])

cost_df = generate_costs(distribution='uniform')
cost_df = add_geo_id(cost_df)
geo_costs = {}
for geo in eval_geos:
    costs = cost_df[cost_df['GeoID'] == geo]
    cost_arr = np.array(costs[IP_COLS])[0]
    geo_costs[geo.replace('__', '')] = cost_arr

# Do any additional setup that is constant across evaluations
eval_start_date = pd.to_datetime(EVAL_START_DATE, format='%Y-%m-%d')
eval_end_date = pd.to_datetime(EVAL_END_DATE, format='%Y-%m-%d')
NB_DAYS_PREDICT = (eval_end_date - eval_start_date).days


class Positive(Constraint):
    def __call__(self, w):
        return K.abs(w)


# Functions to be used for lambda layers in model
def _combine_r_and_d(x):
    r, d, c = x
    return d

predictor = None


def predict_future_cases(model, prescribed_ips, initial_context_input):
    nb_days = 14

    initial_context_input = np.array(initial_context_input)
    action_input = np.array(prescribed_ips)

    context_input = np.expand_dims(np.copy(initial_context_input), axis=2)

    new_pred = None

    for i in range(nb_days):

        if new_pred is not None:
            context_input[:, :-1] = context_input[:, 1:]
            context_input[:, -1] = new_pred
            action_input[:, :-1] = action_input[:, 1:]
            action_input[:, -1, :] = prescribed_ips[:, -1, :]

        new_pred = model([context_input, action_input])

    return new_pred

context = None
import tensorflow as tf
def loss_fn(inputs=None, y_pred_=None, testing=False):
    global predictor, context

    n_cases = 0
    if inputs.shape[0] is not None:
        context = inputs[:, :, 0]
        actions = inputs[:, :-1, 1:13]

        # print(inputs.shape)

        prescribed_ips = y_pred_ * ip_max_values_arr
        _prescribed_ips = prescribed_ips
        # if not isinstance(_prescribed_ips, np.ndarray):
        #    _prescribed_ips = _prescribed_ips.numpy()

        # for i in range(_prescribed_ips.shape[-1]):
        #    _prescribed_ips[:, i] = np.minimum(_prescribed_ips[:, i], ip_max_values_arr[i])
        _prescribed_ips = K.expand_dims(np.round(prescribed_ips), axis=1)
        _prescribed_ips = K.concatenate([actions, _prescribed_ips], axis=1)

        n_cases = predict_future_cases(predictor, _prescribed_ips, context)

        # n_cases = predictor([context, _prescribed_ips])  # * np.max(context) * 10

    def loss_fn_(y_true, y_pred):
        global predictor, context
        # print(y_pred, y_true)
        costs = geo_costs[geo_id.replace("__", '')] # K.round(y_pred * ip_max_values_arr)

        # loss = K.sqrt(K.sum(K.sum((n_cases + costs * prescribed_ips)**2, axis=1)))
        loss = K.sum(K.sum((n_cases + costs * prescribed_ips), axis=1))
        return loss
        """
        if len(prescribed_ips.shape) > 1:
            loss = K.sum((costs * prescribed_ips) ** 2, axis=1)
        else:
            loss = K.sum((costs * prescribed_ips) ** 2)
        return loss
        """
    return loss_fn_


# Construct model
def _construct_model(nb_context, nb_action, nb_costs, lstm_size=32, nb_lookback_days=NB_LOOKBACK_DAYS):
    # Create action encoder
    # Every aspect is monotonic and nonnegative except final bias
    _input = Input(shape=(nb_lookback_days, nb_action + nb_context + nb_costs),
                   name='action_input')
    # print("Input shape:", (nb_lookback_days, nb_action + nb_context + nb_costs))
    x = LSTM(units=lstm_size,
             kernel_constraint=Positive(),
             recurrent_constraint=Positive(),
             bias_constraint=Positive(),
             return_sequences=False,
             name='action_lstm')(_input)
    """
    x = LSTM(units=lstm_size,
             kernel_constraint=Positive(),
             recurrent_constraint=Positive(),
             bias_constraint=Positive(),
             return_sequences=False,
             name='action_lstm')(x)
    x = LSTM(units=lstm_size,
             kernel_constraint=Positive(),
             recurrent_constraint=Positive(),
             bias_constraint=Positive(),
             return_sequences=False,
             name='action_lstm')(x)
    x = LSTM(units=lstm_size,
             kernel_constraint=Positive(),
             recurrent_constraint=Positive(),
             bias_constraint=Positive(),
             return_sequences=False,
             name='action_lstm')(x)
    """
    action_output = Dense(units=12,
                          activation='sigmoid',
                          kernel_constraint=Positive(),
                          name='action_dense')(x)

    # Create prediction model
    model = Model(inputs=_input,
                  outputs=action_output)
    model.compile(loss=loss_fn(_input, action_output), optimizer='adam')

    # Create training model, which includes loss to measure
    # variance of action_output predictions
    training_model = Model(inputs=_input,
                           outputs=action_output)
    training_model.compile(loss=loss_fn(_input, action_output),
                           optimizer='adam')

    return model, training_model


# Train model
def _train_model(training_model, X_context, X_action, X_costs, epochs=1, verbose=0):
    early_stopping = EarlyStopping(patience=20,
                                   restore_best_weights=True)
    # print(X_context.shape, X_action.shape, X_costs.shape)
    inputs = np.concatenate([X_context, X_action, X_costs], axis=2)
    l = len(inputs)
    split=.9
    training_idx = np.random.randint(inputs.shape[0], size=int(split*l))
    val_idx = np.random.randint(inputs.shape[0], size=l - int(split*l))
    training, val = inputs[training_idx], inputs[val_idx]

    # print(inputs.shape)
    # sys.exit(0)
    # print(inputs.shape)
    """
    history = training_model.fit(inputs, np.zeros((X_context.shape[0], X_costs.shape[-1])),
                                 epochs=epochs,
                                 batch_size=32,
                                 validation_split=0.1,
                                 callbacks=[early_stopping],
                                 verbose=verbose)
    """
    optimizer = tf.optimizers.Adam(learning_rate=1e-2)
    history = {
        'train_loss': [],
        'val_loss': []
    }
    for epoch in range(epochs):
        print("\nStart of epoch %d" % (epoch,))

        # Iterate over the batches of the dataset.
        for step in range(0, len(training), 32):
            x_batch = training[step:step+32]
            # print("x batch train", x_batch.shape, "step", step)

            # Open a GradientTape to record the operations run
            # during the forward pass, which enables auto-differentiation.
            with tf.GradientTape() as tape:

                # Run the forward pass of the layer.
                # The operations that the layer applies
                # to its inputs are going to be recorded
                # on the GradientTape.
                preds = training_model(x_batch, training=True)  # Logits for this minibatch

                # Compute the loss value for this minibatch.
                loss_value = loss_fn(x_batch, preds)([], preds)
                history['train_loss'].append(loss_value)

            # Use the gradient tape to automatically retrieve
            # the gradients of the trainable variables with respect to the loss.
            grads = tape.gradient(loss_value, training_model.trainable_weights)

            # Run one step of gradient descent by updating
            # the value of the variables to minimize the loss.
            optimizer.apply_gradients(zip(grads, training_model.trainable_weights))
            # Log every 200 batches.
            if step % 32 == 0:
                print(f"{step + len(x_batch)}/{len(training)} | "
                      f"Training loss (for one batch) at step {step}: {float(loss_value):.4f}")

    # Run a validation loop at the end of each epoch.
    for step in range(0, len(val), 32):
        x_batch = val[step:step+32]
        preds = training_model(x_batch, training=False)  # Logits for this minibatch

        # Compute the loss value for this minibatch.
        loss_value = loss_fn(x_batch, preds)([], preds)
        history['val_loss'].append(loss_value)

    return history

model_path = f"{root_path}/prescribe1/saved_models"

def save_model(model):
    if not os.path.exists(model_path):
        os.mkdir(model_path)
    path = f"{model_path}/AL_model.h5"
    model.save(path)

def load_model_weights(trained_model):
    path = f"{model_path}/AL_model.h5"
    if os.path.exists(path):
        trained_model.load_weights(path)
        return trained_model
    print("Could not load model at:", path)
    return False

def _prepare_dataframe(data_url: str) -> pd.DataFrame:
    """
    Loads the Oxford dataset, cleans it up and prepares the necessary columns. Depending on options, also
    loads the Johns Hopkins dataset and merges that in.
    :param data_url: the url containing the original data
    :return: a Pandas DataFrame with the historical data
    """
    # Original df from Oxford
    df = _load_original_data(data_url)

    # Drop countries with no population data
    df.dropna(subset=['population'], inplace=True)

    #  Keep only needed columns
    columns = CASES_COL + IP_COLS
    df = df[columns]

    df = df[df['Date'] < eval_start_date]

    # Fill in missing values
    _fill_missing_values(df)

    # Compute number of new cases and deaths each day
    df['NewCases'] = df.groupby('GeoID').ConfirmedCases.diff().fillna(0)
    df['NewDeaths'] = df.groupby('GeoID').ConfirmedDeaths.diff().fillna(0)

    # Replace negative values (which do not make sense for these columns) with 0
    df['NewCases'] = df['NewCases'].clip(lower=0)
    df['NewDeaths'] = df['NewDeaths'].clip(lower=0)

    # Compute smoothed versions of new cases and deaths each day
    df['SmoothNewCases'] = df.groupby('GeoID')['NewCases'].rolling(
        WINDOW_SIZE, center=False).mean().fillna(0).reset_index(0, drop=True)
    df['SmoothNewDeaths'] = df.groupby('GeoID')['NewDeaths'].rolling(
        WINDOW_SIZE, center=False).mean().fillna(0).reset_index(0, drop=True)

    # Compute percent change in new cases and deaths each day
    df['CaseRatio'] = df.groupby('GeoID').SmoothNewCases.pct_change(
    ).fillna(0).replace(np.inf, 0) + 1
    df['DeathRatio'] = df.groupby('GeoID').SmoothNewDeaths.pct_change(
    ).fillna(0).replace(np.inf, 0) + 1

    # Add column for proportion of population infected
    df['ProportionInfected'] = df['ConfirmedCases'] / df['population']

    # Create column of value to predict
    df['PredictionRatio'] = df['CaseRatio'] / (1 - df['ProportionInfected'])

    return df


def _load_original_data(data_url):
    latest_df = pd.read_csv(data_url,
                            parse_dates=['Date'],
                            encoding="ISO-8859-1",
                            dtype={"RegionName": str,
                                   "RegionCode": str},
                            error_bad_lines=False)
    # GeoID is CountryName / RegionName
    # np.where usage: if A then B else C
    latest_df["GeoID"] = np.where(latest_df["RegionName"].isnull(),
                                  latest_df["CountryName"],
                                  latest_df["CountryName"] + ' / ' + latest_df["RegionName"])
    return latest_df


def _fill_missing_values(df):
    """
    # Fill missing values by interpolation, ffill, and filling NaNs
    :param df: Dataframe to be filled
    """
    df.update(df.groupby('GeoID').ConfirmedCases.apply(
        lambda group: group.interpolate(limit_area='inside')))
    # Drop country / regions for which no number of cases is available
    df.dropna(subset=['ConfirmedCases'], inplace=True)
    df.update(df.groupby('GeoID').ConfirmedDeaths.apply(
        lambda group: group.interpolate(limit_area='inside')))
    # Drop country / regions for which no number of deaths is available
    df.dropna(subset=['ConfirmedDeaths'], inplace=True)
    for npi_column in IP_COLS:
        df.update(df.groupby('GeoID')[npi_column].ffill().fillna(0))


def _create_country_samples(df: pd.DataFrame, geos: list) -> dict:
    """
    For each country, creates numpy arrays for Keras
    :param df: a Pandas DataFrame with historical data for countries (the "Oxford" dataset)
    :param geos: a list of geo names
    :return: a dictionary of train and test sets, for each specified country
    """
    context_column = 'NewCases'
    action_columns = IP_COLS
    outcome_column = 'NewCases'
    country_samples = {}
    for g in geos:
        cdf = df[df.GeoID == g]
        cdf = cdf[cdf.ConfirmedCases.notnull()]
        context_data = np.array(cdf[context_column])
        action_data = np.array(cdf[action_columns])
        costs_data = np.tile(geo_costs[g], (NB_LOOKBACK_DAYS, 1))
        outcome_data = np.array(cdf[outcome_column])

        context_samples = []
        action_samples = []
        costs_samples = []
        outcome_samples = []
        nb_total_days = outcome_data.shape[0]

        for d in range(NB_LOOKBACK_DAYS, nb_total_days):
            context_samples.append(context_data[d - NB_LOOKBACK_DAYS:d])
            action_samples.append(action_data[d - NB_LOOKBACK_DAYS:d])
            costs_samples.append(costs_data)
            outcome_samples.append(outcome_data[d])
        if len(outcome_samples) > 0:
            X_context = np.expand_dims(np.stack(context_samples, axis=0), axis=2)
            X_action = np.stack(action_samples, axis=0)
            X_costs = np.stack(costs_samples, axis=0)
            country_samples[g] = {
                'X_context': X_context,
                'X_action': X_action,
                'X_costs': X_costs,
                'X_train_context': X_context[:-NB_DAYS_PREDICT],
                'X_train_action': X_action[:-NB_DAYS_PREDICT],
                'X_train_costs': X_costs[:-NB_DAYS_PREDICT],
                'X_test_context': X_context[-NB_DAYS_PREDICT-1:],
                'X_test_action': X_action[-NB_DAYS_PREDICT-1:],
                'X_test_costs': X_costs[-NB_DAYS_PREDICT-1:]
            }
    return country_samples


NUM_TRIALS = 2
LSTM_SIZE = 32
EPOCHS = 50


# Function that evaluates the fitness of each prescriptor model
def trainer(_predictor=None, force_train=False):
    global predictor
    predictor = _predictor
    print("Creating numpy arrays for Keras for each country...")
    df = _prepare_dataframe(HIST_DATA_FILE_PATH)

    geos = _most_affected_geos(df, NB_EVAL_COUNTRIES, NB_LOOKBACK_DAYS)
    country_samples = _create_country_samples(df, geos)
    print("Numpy arrays created")

    # Aggregate data for training
    all_X_context_list = [country_samples[c]['X_train_context']
                          for c in country_samples]
    all_X_action_list = [country_samples[c]['X_train_action']
                         for c in country_samples]
    all_X_cost_list = [country_samples[c]['X_train_costs']
                       for c in country_samples]
    X_context = np.concatenate(all_X_context_list)
    X_action = np.concatenate(all_X_action_list)
    X_costs = np.concatenate(all_X_cost_list)

    _, trained_model = _construct_model(nb_context=X_context.shape[-1],
                                             nb_action=X_action.shape[-1],
                                             nb_costs=X_costs.shape[-1],
                                             lstm_size=LSTM_SIZE,
                                             nb_lookback_days=NB_LOOKBACK_DAYS)

    if not force_train:
        trained_model = load_model_weights(trained_model)
        if trained_model is not False:
            print("Loaded pretrained model")
            return trained_model, df

    # Aggregate data for testing only on top countries
    test_all_X_context_list = [country_samples[g]['X_test_context']
                               for g in geos]
    test_all_X_action_list = [country_samples[g]['X_test_action']
                              for g in geos]
    test_all_X_cost_list = [country_samples[g]['X_test_costs']
                            for g in geos]

    test_X_context = np.concatenate(test_all_X_context_list)
    test_X_action = np.concatenate(test_all_X_action_list)
    test_X_costs = np.concatenate(test_all_X_cost_list)

    # Run full training several times to find best model
    # and gather data for setting acceptance threshold
    models = []
    train_losses = []
    val_losses = []
    test_losses = []
    for t in range(NUM_TRIALS):
        print('Trial', t)
        X_context, X_action, X_costs = _permute_data(X_context, X_action, X_costs, seed=t)
        model, training_model = _construct_model(nb_context=X_context.shape[-1],
                                                 nb_action=X_action.shape[-1],
                                                 nb_costs=X_costs.shape[-1],
                                                 lstm_size=LSTM_SIZE,
                                                 nb_lookback_days=NB_LOOKBACK_DAYS)
        history = _train_model(training_model, X_context, X_action, X_costs, epochs=EPOCHS, verbose=0)
        top_epoch = np.argmin(history['val_loss'])
        train_loss = history['train_loss'][top_epoch]
        val_loss = history['val_loss'][top_epoch]
        #print(test_X_context.shape, test_X_action.shape, test_X_costs.shape)
        input_test = np.concatenate([test_X_context, test_X_action, test_X_costs], axis=2)
        test_prescriptions = training_model(input_test)
        test_loss = loss_fn(input_test, test_prescriptions, testing=True)([], test_prescriptions)
        train_losses.append(train_loss)
        val_losses.append(val_loss)
        test_losses.append(test_loss)
        models.append(training_model)
        print('Train Loss:', train_loss)
        print('Val Loss:', val_loss)
        print('Test Loss:', test_loss)

    # Gather test info
    country_inputss = []
    country_predss = []
    country_casess = []
    for model in models:
        country_inputs, country_preds, country_cases = _lstm_get_test_rollouts(model,
                                                               df,
                                                               geos,
                                                               country_samples)
        country_inputss.append(country_inputs)
        country_predss.append(country_preds)
        country_casess.append(country_cases)

    # Compute cases mae
    test_case_maes = []
    for m in range(len(models)):
        total_loss = 0
        for g in geos:
            inputs_ = country_inputss[m][g][-NB_DAYS_PREDICT:]
            pred_cases = country_predss[m][g][-NB_DAYS_PREDICT:]
            total_loss += loss_fn(inputs_, pred_cases, True)([], pred_cases)
        test_case_maes.append(total_loss)

    # Select best model
    best_model = models[np.argmin(test_case_maes)]
    save_model(best_model)
    print("Training completed, model saved")
    return best_model, df

def predict(model, df, start_date, end_date):
    g = geo_id.replace('__', '')
    cdf = df[df.GeoID == g]
    cdf = cdf[cdf.ConfirmedCases.notnull()]

    context_column = 'NewCases'
    action_columns = IP_COLS

    context_data = np.array(cdf[context_column])
    action_data = np.array(cdf[action_columns])
    costs_data = np.tile(geo_costs[g], (NB_LOOKBACK_DAYS, 1))

    context_samples = context_data[- NB_LOOKBACK_DAYS:].reshape((NB_LOOKBACK_DAYS, 1))
    action_samples = action_data[- NB_LOOKBACK_DAYS:]
    costs_samples = costs_data

    eval_start_date = pd.to_datetime(start_date, format='%Y-%m-%d')
    eval_end_date = pd.to_datetime(end_date, format='%Y-%m-%d')
    nb_test_days = (eval_end_date - eval_start_date).days + 1

    nb_actions = action_data.shape[-1]

    inputs_, prescriptions, preds = _lstm_roll_out_predictions(df, model,
                                                      context_samples,
                                                      action_samples,
                                                      costs_samples,
                                                      None,
                                                      nb_test_days)

    return prescriptions, preds

from datetime import datetime, timedelta

# Functions for computing test metrics
def _lstm_roll_out_predictions(df, model, initial_context_input, initial_action_input, initial_costs_input,
                               future_action_sequence_, nb_test_days = NB_DAYS_PREDICT + 1):
    # predict cases using predictor
    # nb_test_days = NB_DAYS_PREDICT + 1
    pred_output = np.zeros(nb_test_days)
    initial_context_input = np.array(initial_context_input)
    initial_action_input = np.array(initial_action_input)
    initial_costs_input = np.array(initial_costs_input)
    prescriptions_output = np.zeros((nb_test_days, initial_costs_input.shape[-1]))

    context_input = np.expand_dims(np.copy(initial_context_input), axis=0)
    action_input = np.expand_dims(np.copy(initial_action_input), axis=0)
    costs_input = np.expand_dims(np.copy(initial_costs_input), axis=0)

    count_ = 0

    country_code = 'ALB'

    # Set up dictionary to keep track of prescription
    df_dict = {'CountryName': [], 'RegionName': [], 'Date': [], 'PrescriptionIndex': []}
    for ip_col in IP_COLS:
        df_dict[ip_col] = []

    preds = None
    prescribed_ips = None

    inputs_ = []

    for d, date in enumerate(pd.date_range(eval_start_date, eval_end_date)):
        date_str = date.strftime("%Y-%m-%d")
        count_ += 1

        if count_ > len(prescriptions_output):
            break

        # print('*'*100)
        # print(action_input.shape, future_action_sequence.shape)
        if prescribed_ips is not None:
            context_input[:, :-1] = context_input[:, 1:]
            context_input[:, -1] = preds['PredictedDailyNewCases'][d-1]
            action_input[:, :-1] = action_input[:, 1:]
            action_input[:, -1] = prescribed_ips

        if count_ == 1:
            inputs = np.concatenate([context_input, action_input, costs_input], axis=2)
            prescriptions = model.predict(inputs)
            prescribed_ips = (prescriptions * ip_max_values_arr).round().flatten()
            # print(ip_max_values_arr)
            # print(prescribed_ips)
            prescribed_ips = [min(ip_max_values_arr[i], prescribed_ips[i]) for i in range(prescribed_ips.shape[-1])]

            for i in range(WINDOW_SIZE):
                inputs_.append(inputs)
                df_dict['CountryName'].append(geo_id)
                df_dict['RegionName'].append("")
                df_dict['Date'].append(date_str)
                df_dict['PrescriptionIndex'].append(1)
                for ip_col, prescribed_ip in zip(IP_COLS, prescribed_ips):
                    df_dict[ip_col].append(prescribed_ip)
                date_str = (datetime.strptime(date_str, '%Y-%m-%d') + timedelta(days=1)).strftime('%Y-%m-%d')

            df_dict_ = pd.DataFrame(df_dict)
            preds = get_predictions(EVAL_START_DATE, EVAL_END_DATE, df_dict_, countries=[geo_id.replace("__", "")])

        prescriptions_output[d] = prescribed_ips
        pred_output[d] = preds['PredictedDailyNewCases'][d]

        if count_ == WINDOW_SIZE:
            count_ = 0
    inputs_ = np.array(inputs_).squeeze()
    return inputs_, prescriptions_output, pred_output


def _lstm_get_test_rollouts(model, df, top_geos, country_samples):
    country_inputs = {}
    country_preds = {}
    country_cases = {}
    for g in top_geos:

        initial_context_input = country_samples[g]['X_test_context'][0]
        initial_action_input = country_samples[g]['X_test_action'][0]
        initial_costs_input = country_samples[g]['X_test_costs'][0]

        nb_test_days = NB_DAYS_PREDICT + 1
        nb_actions = initial_action_input.shape[-1]

        future_action_sequence = np.zeros((nb_test_days, nb_actions))
        future_action_sequence[:nb_test_days] = country_samples[g]['X_test_action'][:, -1, :]
        current_action = country_samples[g]['X_test_action'][:, -1, :][-1]
        future_action_sequence[14:] = current_action
        inputs, prescriptions, preds = _lstm_roll_out_predictions(df, model,
                                           initial_context_input,
                                           initial_action_input,
                                           initial_costs_input,
                                           future_action_sequence)
        country_inputs[g] = inputs
        country_preds[g] = prescriptions
        country_cases[g] = preds

    return country_inputs, country_preds, country_cases


def _most_affected_geos(df, nb_geos, min_historical_days):
    """
    Returns the list of most affected countries, in terms of confirmed deaths.
    :param df: the data frame containing the historical data
    :param nb_geos: the number of geos to return
    :param min_historical_days: the minimum days of historical data the countries must have
    :return: a list of country names of size nb_countries if there were enough, and otherwise a list of all the
    country names that have at least min_look_back_days data points.
    """
    # By default use most affected geos with enough history
    gdf = df.groupby('GeoID')['ConfirmedDeaths'].agg(['max', 'count']).sort_values(by='max', ascending=False)
    filtered_gdf = gdf[gdf["count"] > min_historical_days]
    geos = list(filtered_gdf.head(nb_geos).index)
    return geos


# Shuffling data prior to train/val split
def _permute_data(X_context, X_action, X_costs, seed=301):
    # np.random.seed(seed)
    p = np.random.permutation(X_action.shape[0])
    X_context = X_context[p]
    X_action = X_action[p]

    return X_context, X_action, X_costs

    # Set up dictionary to keep track of prescription
    df_dict = {'CountryName': [], 'RegionName': [], 'Date': []}
    for ip_col in IP_COLS:
        df_dict[ip_col] = []

    # Set initial data
    eval_past_cases = deepcopy(past_cases)
    eval_past_ips = deepcopy(past_ips)

    # Compute prescribed stringency incrementally
    stringency = 0.

    # Make prescriptions one day at a time, feeding resulting
    # predictions from the predictor back into the prescriptor.
    for date in pd.date_range(eval_start_date, eval_end_date):
        date_str = date.strftime("%Y-%m-%d")

        # Prescribe for each geo
        for geo in eval_geos:

            # Prepare input data. Here we use log to place cases
            # on a reasonable scale; many other approaches are possible.
            X_cases = np.log(eval_past_cases[geo][-NB_LOOKBACK_DAYS:] + 1)
            X_ips = eval_past_ips[geo][-NB_LOOKBACK_DAYS:]
            X_costs = geo_costs[geo]
            X = np.concatenate([X_cases.flatten(),
                                X_ips.flatten(),
                                X_costs])

            # Get prescription
            prescribed_ips = net.activate(X)

            # Map prescription to integer outputs
            prescribed_ips = (prescribed_ips * ip_max_values_arr).round()

            # Add it to prescription dictionary
            country_name, region_name = geo.split('__')
            if region_name == 'nan':
                region_name = np.nan
            df_dict['CountryName'].append(country_name)
            df_dict['RegionName'].append(region_name)
            df_dict['Date'].append(date_str)
            for ip_col, prescribed_ip in zip(IP_COLS, prescribed_ips):
                df_dict[ip_col].append(prescribed_ip)

            # Update stringency. This calculation could include division by
            # the number of IPs and/or number of geos, but that would have
            # no effect on the ordering of candidate solutions.
            stringency += np.sum(geo_costs[geo] * prescribed_ips)

        # Create dataframe from prescriptions.
        pres_df = pd.DataFrame(df_dict)

        # Make prediction given prescription for all countries
        pred_df = get_predictions(EVAL_START_DATE, date_str, pres_df)

        # Update past data with new day of prescriptions and predictions
        pres_df['GeoID'] = pres_df['CountryName'] + '__' + pres_df['RegionName'].astype(str)
        pred_df['RegionName'] = pred_df['RegionName'].fillna("")
        pred_df['GeoID'] = pred_df['CountryName'] + '__' + pred_df['RegionName'].astype(str)
        new_pres_df = pres_df[pres_df['Date'] == date_str]
        new_pred_df = pred_df[pred_df['Date'] == date_str]
        for geo in eval_geos:
            geo_pres = new_pres_df[new_pres_df['GeoID'] == geo]
            geo_pred = new_pred_df[new_pred_df['GeoID'] == geo]

            # Append array of prescriptions
            pres_arr = np.array([geo_pres[ip_col].values[0] for ip_col in IP_COLS]).reshape(1, -1)
            eval_past_ips[geo] = np.concatenate([eval_past_ips[geo], pres_arr])

            # Append predicted cases
            eval_past_cases[geo] = np.append(eval_past_cases[geo],
                                             geo_pred[PRED_CASES_COL].values[0])

    # Compute fitness. There are many possibilities for computing fitness and ranking
    # candidates. Here we choose to minimize the product of ip stringency and predicted
    # cases. This product captures the area of the 2D objective space that dominates
    # the candidate. We minimize it by including a negation. To place the fitness on
    # a reasonable scale, we take means over all geos and days. Note that this fitness
    # function can lead directly to the degenerate solution of all ips 0, i.e.,
    # stringency zero. To achieve more interesting behavior, a different fitness
    # function may be required.
    new_cases = pred_df[PRED_CASES_COL].mean().mean()
    genome.fitness = -(new_cases * stringency)

    print('Evaluated Genome', genome_id)
    print('New cases:', new_cases)
    print('Stringency:', stringency)
    print('Fitness:', genome.fitness)
