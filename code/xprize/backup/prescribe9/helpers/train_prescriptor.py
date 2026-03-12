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
from datetime import timedelta

ups = '/..' * 2
root_path = os.path.dirname(os.path.realpath(__file__)) + ups
prescriptor_dir = os.path.dirname(os.path.realpath(__file__)) + '/..'
sys.path.append(root_path)

from .utils import CASES_COL, IP_COLS, IP_MAX_VALUES, add_geo_id, NB_LOOKBACK_DAYS, POPULATION_FILE

from .cost_generator import generate_costs

ROOT_DIR = Path(os.path.dirname(os.path.abspath(__file__)))

CUTOFF_DATE = '2020-07-31'
EVAL_START_DATE = '2020-08-01'
EVAL_END_DATE = '2020-08-31'

eval_start_date = pd.to_datetime(EVAL_START_DATE, format='%Y-%m-%d')
eval_end_date = pd.to_datetime(EVAL_END_DATE, format='%Y-%m-%d')
NB_DAYS_PREDICT = (eval_end_date - eval_start_date).days

WINDOW_SIZE = 15

NUM_TRIALS = 2
LSTM_SIZE = 32
EPOCHS = 100

ip_max_values_arr = np.array([IP_MAX_VALUES[ip] for ip in IP_COLS])


class Positive(Constraint):
    def __call__(self, w):
        return K.abs(w)


# Functions to be used for lambda layers in model
def _combine_r_and_d(x):
    r, d, c = x
    return d

predictor = None
df = None
geo_id = None


def predict_future_cases(model, prescribed_ips, initial_context_input):
    global df, geo_id
    nb_days = WINDOW_SIZE  # (lag)

    initial_context_input = np.array(initial_context_input)
    action_input = np.array(prescribed_ips)

    context_input = np.expand_dims(np.copy(initial_context_input), axis=2)

    cdf = df[df.GeoID == geo_id]
    cdf = cdf[cdf.ConfirmedCases.notnull()]
    # Gather info to convert to total cases
    prev_confirmed_cases = np.array(cdf.ConfirmedCases)
    prev_new_cases = np.array(cdf.NewCases)
    initial_total_cases = prev_confirmed_cases[-1]
    pop_size = np.array(cdf.population)[-1]

    preds = np.array([])
    new_pred = None

    for i in range(nb_days):

        new_pred = model([context_input, action_input])

        new_pred_ = _convert_ratios_to_total_cases(np.asarray(new_pred), WINDOW_SIZE,
            prev_new_cases, initial_total_cases, pop_size)
        initial_total_cases += np.sum(np.array(new_pred_))

        context_input[:, :-1] = context_input[:, 1:]
        prev_new_cases = np.concatenate([prev_new_cases, new_pred_], axis=0)
        new_pred_ = np.array(new_pred_).reshape(-1, 1)
        context_input[:, -1] = new_pred_
        # action_input[:, :-1] = action_input[:, 1:]
        # action_input[:, -1, :] = prescribed_ips[:, -1, :]

        preds = new_pred_ if len(preds) == 0 else np.concatenate([preds, new_pred_], axis=1)

    return new_pred, preds

context = None
cases = None
prescribed_ips = None
n_cases = None
import tensorflow as tf

def loss_fn(inputs=None, y_pred_=None, testing=False):
    global predictor, context, cases, prescribed_ips, n_cases

    n_cases = 0
    cases = 0
    if inputs.shape[0] is not None:
        context = inputs[:, :, 0]
        actions = inputs[:, :-1, 1:13]

        # print(inputs.shape)

        cases = y_pred_[:, 0]
        cases = K.expand_dims(cases, axis=1)
        y_pred_ = y_pred_[:, 1:]
        prescribed_ips = y_pred_ * ip_max_values_arr
        _prescribed_ips = prescribed_ips
        if not isinstance(_prescribed_ips, np.ndarray):
            _prescribed_ips = _prescribed_ips.numpy()

        for i in range(_prescribed_ips.shape[-1]):
            _prescribed_ips[:, i] = np.minimum(_prescribed_ips[:, i], ip_max_values_arr[i])
        _prescribed_ips = K.expand_dims(np.round(_prescribed_ips), axis=1)
        _prescribed_ips = K.concatenate([actions, _prescribed_ips], axis=1)

        _, n_cases = predict_future_cases(predictor, _prescribed_ips, context)
        n_cases = n_cases[0]
        # prescribed_ips = _prescribed_ips

        # n_cases = predictor([context, _prescribed_ips])  # * np.max(context) * 10

    def loss_fn_(y_true, y_pred):
        global predictor, context, cases, n_cases, prescribed_ips

        cases = tf.cast(cases, dtype=prescribed_ips.dtype)
        n_cases = tf.cast(n_cases, dtype=prescribed_ips.dtype)
        n_cases = np.asarray(n_cases)
        prescribed_ips = tf.cast(prescribed_ips, dtype=n_cases.dtype)
        predictor_loss = K.mean(((cases - n_cases)) ** 2) * 10
        ratio = 1  # 50. / 100000.  # ./1440  # 1440. / 2880913
        cases_loss = K.mean(cases * ratio)
        stringency = K.sum((prescribed_ips) / 24, axis=1)
        # print(cases.dtype, stringency.dtype)
        # agg_loss = K.sum((cases * ratio) + stringency, axis=1)
        agg_loss = K.mean(stringency)  # K.sum(prescribed_ips, axis=1)  # K.mean(stringency)

        loss = agg_loss  # .1 * K.mean(agg_loss) * 1e-3
        # reg_loss = (1 + K.sum(n_cases)) / (agg_loss + 1e-5)
        # reg_loss = 32 * 1 * K.minimum(1000, reg_loss)

        reg_loss = (1 + K.sum(cases)) / (agg_loss*24 + 1e-5)
        if reg_loss >= 1000:
            loss = 0

        # print(prescribed_ips)
        # print(agg_loss, reg_loss, predictor_loss, cases_loss)
        # loss += reg_loss
        # print(loss, predictor_loss, cases_loss)
        # print(loss)
        loss += predictor_loss
        # print(K.sum((cases - n_cases) ** 2))
        # loss += cases_loss
        # print(K.sum(cases))

        return loss
    return loss_fn_

def loss_fn2(inputs=None, y_pred_=None, testing=False):
    global predictor, context, cases, prescribed_ips, n_cases

    n_cases = 0
    cases = 0
    if inputs.shape[0] is not None:
        context = inputs[:, :, 0]
        actions = inputs[:, :-1, 1:13]

        # print(inputs.shape)

        cases = y_pred_[:, 0]
        cases = K.expand_dims(cases, axis=1)
        y_pred_ = y_pred_[:, 1:]
        prescribed_ips = y_pred_ * ip_max_values_arr
        _prescribed_ips = prescribed_ips
        if not isinstance(_prescribed_ips, np.ndarray):
            _prescribed_ips = _prescribed_ips.numpy()

        for i in range(_prescribed_ips.shape[-1]):
            _prescribed_ips[:, i] = np.minimum(_prescribed_ips[:, i], ip_max_values_arr[i])
        _prescribed_ips = K.expand_dims(np.round(_prescribed_ips), axis=1)
        _prescribed_ips = K.concatenate([actions, _prescribed_ips], axis=1)

        _, n_cases = predict_future_cases(predictor, _prescribed_ips, context)
        n_cases = n_cases[0]
        # prescribed_ips = _prescribed_ips

        # n_cases = predictor([context, _prescribed_ips])  # * np.max(context) * 10

    def loss_fn_(y_true, y_pred):
        global predictor, context, cases, n_cases, prescribed_ips

        # cases = y_pred[:, 0]
        # cases = tf.cast(cases, dtype=n_cases.dtype)
        
        n_cases = tf.cast(n_cases, dtype=n_cases.dtype)
        n_cases = np.asarray(n_cases)        

        stringency = K.sum((prescribed_ips) / 24, axis=1)
        agg_loss = K.mean(stringency)  # K.sum(prescribed_ips, axis=1)  # K.mean(stringency)
        agg_loss = tf.cast(agg_loss, dtype=cases.dtype)
        reg_loss = (1 + (K.sum(cases) * (50/100000))) / (agg_loss*24 + 1e-5)
        loss = 5 * reg_loss
        if loss >= 1000:
            loss = K.sum((prescribed_ips - ip_max_values_arr)**2)
            loss = 1. * loss

        l1_reg_term = K.sum(K.abs(prescribed_ips))
        if l1_reg_term > 0:
            l1_reg_term = tf.cast(l1_reg_term, dtype=agg_loss.dtype)
            loss += .0 * l1_reg_term

        return loss
    return loss_fn_

def _convert_ratio_to_new_cases(ratio,
                                window_size,
                                prev_new_cases_list,
                                prev_pct_infected):
    return (ratio * (1 - prev_pct_infected) - 1) * \
           (window_size * np.mean(prev_new_cases_list[-window_size:])) \
           + prev_new_cases_list[-window_size]

def _convert_ratios_to_total_cases(ratios,
                                   window_size,
                                   prev_new_cases,
                                   initial_total_cases,
                                   pop_size):
    new_new_cases = []
    prev_new_cases_list = list(prev_new_cases)
    curr_total_cases = initial_total_cases
    for ratio in ratios:
        new_cases = _convert_ratio_to_new_cases(ratio,
                                                window_size,
                                                prev_new_cases_list,
                                                curr_total_cases / pop_size)
        # new_cases can't be negative!
        new_cases = max(0, new_cases[0])
        # Which means total cases can't go down
        curr_total_cases += new_cases
        # Update prev_new_cases_list for next iteration of the loop
        prev_new_cases_list.append(new_cases)
        new_new_cases.append(new_cases)
    return new_new_cases


class Prescriptor:

    def __init__(self, df=None, data_path=None, path_to_cost_file=None, _predictor=None,
                 start_date=None, end_date=None):
        self.df = df
        self.models_path = f"{prescriptor_dir}/saved_models"

        global CUTOFF_DATE, EVAL_START_DATE, EVAL_END_DATE, eval_start_date, eval_end_date, NB_DAYS_PREDICT
        EVAL_START_DATE = start_date
        EVAL_END_DATE = end_date
        eval_start_date = pd.to_datetime(EVAL_START_DATE, format='%Y-%m-%d')
        eval_end_date = pd.to_datetime(EVAL_END_DATE, format='%Y-%m-%d')
        NB_DAYS_PREDICT = (eval_end_date - eval_start_date).days
        CUTOFF_DATE = (eval_start_date - timedelta(days=1)).strftime('%Y-%m-%d')

        if (df is None) and (data_path is not None):
            self.df = self._prepare_dataframe(data_path)

        self.countries = None
        self.path_to_cost_file = path_to_cost_file
        self._predictor = _predictor
        global predictor
        predictor = _predictor
        self.geo_costs = None
        self.cost_df = None
        if path_to_cost_file is not None:
            cost_df = pd.read_csv(path_to_cost_file,
                                    encoding="ISO-8859-1",
                                    dtype={"RegionName": str,
                                           "RegionCode": str},
                                    error_bad_lines=False)
        else:
            cost_df = generate_costs(distribution='uniform')
        self.cost_df = add_geo_id(cost_df)

    def set_countries(self, countries=None):
        self.countries = self.df.GeoID.unique().tolist() if countries is None else countries
        geo_costs = {}
        cost_df = add_geo_id(self.cost_df)
        for geo in self.countries:
            costs = cost_df[cost_df['GeoID'] == geo]
            if len(costs) > 0:
                cost_arr = np.array(costs[IP_COLS])[0]
                geo_costs[geo] = cost_arr
        self.geo_costs = geo_costs

    # Construct model
    def _construct_model(self, nb_context, nb_action, nb_costs, lstm_size=32, nb_lookback_days=NB_LOOKBACK_DAYS):
        _input = Input(shape=(nb_lookback_days, nb_action + nb_context + nb_costs),
                       name='action_input')
        x = LSTM(units=lstm_size,
                 return_sequences=False,
                 name='action_lstm')(_input)
        x = Dense(units=512,
                              activation='relu',
                              # kernel_constraint=Positive(),
                              name='before_action_dense')(x)
        action_output = Dense(units=13,
                              activation='relu',
                              kernel_constraint=Positive(),
                              name='action_dense')(x)

        model = Model(inputs=_input,
                      outputs=action_output)
        model.compile(loss=loss_fn(_input, action_output), optimizer='adam')

        training_model = Model(inputs=_input,
                               outputs=action_output)
        training_model.compile(loss=loss_fn(_input, action_output),
                               optimizer='adam')

        return model, training_model

    # Train model
    def _train_model(self, training_model, X_context, X_action, X_costs, epochs=1, verbose=0):
        inputs = np.concatenate([X_context, X_action, X_costs], axis=2)
        l = len(inputs)
        split=.9
        training_idx = np.random.randint(inputs.shape[0], size=int(split*l))
        val_idx = np.random.randint(inputs.shape[0], size=l - int(split*l))
        training, val = inputs[training_idx], inputs[val_idx]

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
                with tf.GradientTape(persistent=True) as tape:
                    preds = training_model(x_batch, training=True)
                    loss_value = loss_fn(x_batch, preds)([], preds)
                    loss_value2 = loss_fn2(x_batch, preds)([], preds)
                    history['train_loss'].append(loss_value)

                grads = tape.gradient(loss_value, training_model.trainable_weights)
                grads2 = tape.gradient(loss_value2, training_model.trainable_weights)

                optimizer.apply_gradients(zip(grads, training_model.trainable_weights))
                optimizer.apply_gradients(zip(grads2, training_model.trainable_weights))

                del tape

                if step % 32 == 0:
                    print(f"{step + len(x_batch)}/{len(training)} | "
                          f"Training loss (for one batch) at step {step}: {float(loss_value):.4f}")

        for step in range(0, len(val), 32):
            x_batch = val[step:step+32]
            preds = training_model(x_batch, training=False)

            loss_value = loss_fn(x_batch, preds)([], preds)
            history['val_loss'].append(loss_value)

        return history

    def save_model(self, model, geo_id):
        if not os.path.exists(self.models_path):
            os.mkdir(self.models_path)
        path = f"{self.models_path}/{geo_id}.h5"
        model.save(path)

    def load_model_weights(self, trained_model, geo_id):
        path = f"{self.models_path}/{geo_id}.h5"
        if os.path.exists(path):
            trained_model.load_weights(path)
            return trained_model
        print("Could not load model at:", path)
        return False

    def _prepare_dataframe(self, data_url: str) -> pd.DataFrame:
        """
        Loads the Oxford dataset, cleans it up and prepares the necessary columns. Depending on options, also
        loads the Johns Hopkins dataset and merges that in.
        :param data_url: the url containing the original data
        :return: a Pandas DataFrame with the historical data
        """
        # Original df from Oxford
        df = self._load_original_data(data_url)

        population_df = pd.read_csv(POPULATION_FILE,
                              encoding="ISO-8859-1",
                              dtype={"RegionName": str,
                                     "RegionCode": str},
                              error_bad_lines=False)
        population_df = population_df[['CountryName', 'RegionName', 'population']]
        df = df.merge(population_df, how='outer', on=['CountryName', 'RegionName'])

        # Drop countries with no population data
        df.dropna(subset=['population'], inplace=True)

        #  Keep only needed columns
        columns = CASES_COL + IP_COLS
        df = df[columns]

        df = df[df['Date'] < eval_start_date]

        # Fill in missing values
        self._fill_missing_values(df)

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

        # Fill any missing case values by interpolation and setting NaNs to 0
        df.update(df.groupby('GeoID').NewCases.apply(
            lambda group: group.interpolate()).fillna(0))

        # Fill any missing IPs by assuming they are the same as previous day
        for ip_col in IP_MAX_VALUES:
            df.update(df.groupby('GeoID')[ip_col].ffill().fillna(0))

        return df

    def _load_original_data(self, data_url):
        latest_df = pd.read_csv(data_url,
                                parse_dates=['Date'],
                                encoding="ISO-8859-1",
                                dtype={"RegionName": str,
                                       "RegionCode": str},
                                error_bad_lines=False)
        # GeoID is CountryName / RegionName
        # np.where usage: if A then B else C
        latest_df = add_geo_id(latest_df)
        return latest_df

    def _fill_missing_values(self, df):
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

    def _create_country_samples(self, df: pd.DataFrame, geos: list) -> dict:
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
            costs_data = np.tile(self.geo_costs[g], (NB_LOOKBACK_DAYS, 1))
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

    # Function that evaluates the fitness of each prescriptor model
    def trainer(self, _geo_id, force_train=False):
        global predictor, df, geo_id

        # print("Creating numpy arrays for Keras for each country...")
        df = self.df
        geo_id = _geo_id
        _geo_id = _geo_id.replace(' / ', '_')

        geos = self.countries
        country_samples = self._create_country_samples(df, geos)
        # print("Numpy arrays created")

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

        _, trained_model = self._construct_model(nb_context=X_context.shape[-1],
                                                 nb_action=X_action.shape[-1],
                                                 nb_costs=X_costs.shape[-1],
                                                 lstm_size=LSTM_SIZE,
                                                 nb_lookback_days=NB_LOOKBACK_DAYS)

        if not force_train:
            trained_model = self.load_model_weights(trained_model, _geo_id)
            if trained_model is not False:
                # print("Loaded pretrained model", _geo_id)
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
            print('\nTrial', t)
            X_context, X_action, X_costs = self._permute_data(X_context, X_action, X_costs, seed=t)
            model, training_model = self._construct_model(nb_context=X_context.shape[-1],
                                                     nb_action=X_action.shape[-1],
                                                     nb_costs=X_costs.shape[-1],
                                                     lstm_size=LSTM_SIZE,
                                                     nb_lookback_days=NB_LOOKBACK_DAYS)
            history = self._train_model(training_model, X_context, X_action, X_costs, epochs=EPOCHS, verbose=0)
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
            country_inputs, country_preds, country_cases = self._lstm_get_test_rollouts(model,
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
        self.save_model(best_model, _geo_id)
        print("Training completed, model saved")
        return best_model, df

    def predict(self, model, start_date, end_date, geo_id):
        global EVAL_START_DATE, EVAL_END_DATE, eval_start_date, eval_end_date, NB_DAYS_PREDICT
        EVAL_START_DATE = start_date
        EVAL_END_DATE = end_date
        eval_start_date = pd.to_datetime(EVAL_START_DATE, format='%Y-%m-%d')
        eval_end_date = pd.to_datetime(EVAL_END_DATE, format='%Y-%m-%d')
        NB_DAYS_PREDICT = (eval_end_date - eval_start_date).days

        g = geo_id
        cdf = self.df[self.df.GeoID == g]
        cdf = cdf[cdf.ConfirmedCases.notnull()]

        context_column = 'NewCases'
        action_columns = IP_COLS

        context_data = np.array(cdf[context_column])
        action_data = np.array(cdf[action_columns])
        costs_data = np.tile(self.geo_costs[g], (NB_LOOKBACK_DAYS, 1))

        context_samples = context_data[- NB_LOOKBACK_DAYS:].reshape((NB_LOOKBACK_DAYS, 1))
        action_samples = action_data[- NB_LOOKBACK_DAYS:]
        costs_samples = costs_data

        eval_start_date = pd.to_datetime(start_date, format='%Y-%m-%d')
        eval_end_date = pd.to_datetime(end_date, format='%Y-%m-%d')

        inputs_, prescriptions, pred_new_cases = self._lstm_roll_out_predictions(self.df, model, geo_id,
                                                          context_samples,
                                                          action_samples,
                                                          costs_samples,
                                                          None)

        return prescriptions, pred_new_cases

    # Functions for computing test metrics
    def _lstm_roll_out_predictions(self, df, model, geo_id, initial_context_input, initial_action_input, initial_costs_input,
                                   future_action_sequence_):
        # predict cases using predictor
        global predictor
        nb_test_days = NB_DAYS_PREDICT + 1

        pred_output = np.zeros(nb_test_days)
        initial_context_input = np.array(initial_context_input)
        initial_action_input = np.array(initial_action_input)
        initial_costs_input = np.array(initial_costs_input)
        prescriptions_output = np.zeros((nb_test_days, 1 + initial_costs_input.shape[-1]))

        context_input = np.expand_dims(np.copy(initial_context_input), axis=0)
        action_input = np.expand_dims(np.copy(initial_action_input), axis=0)
        costs_input = np.expand_dims(np.copy(initial_costs_input), axis=0)

        count_ = 0

        # Set up dictionary to keep track of prescription
        df_dict = {'CountryName': [], 'RegionName': [], 'Date': [], 'PrescriptionIndex': []}
        for ip_col in IP_COLS:
            df_dict[ip_col] = []

        preds = None
        prescribed_ips = None

        inputs_ = []

        for d, date in enumerate(pd.date_range(eval_start_date, eval_end_date)):
            count_ += 1

            if d >= len(prescriptions_output):
                break

            if count_ == 1:
                inputs = np.concatenate([context_input, action_input, costs_input], axis=2)
                prescriptions = model.predict(inputs)
                prescribed_ips = np.round(prescriptions[:, 1:] * ip_max_values_arr).flatten()
                # print(ip_max_values_arr)
                prescribed_ips = [min(ip_max_values_arr[i], prescribed_ips[i]) for i in range(prescribed_ips.shape[-1])]

                _, n_cases = predict_future_cases(predictor, action_input, context_input)
                n_cases = n_cases.squeeze()

                for i in range(WINDOW_SIZE):
                    inputs_.append(inputs)

            prescriptions_output[d] = [prescriptions[0, 0]] + prescribed_ips
            pred_output[d] = n_cases[d % WINDOW_SIZE]

            context_input[:, :-1] = context_input[:, 1:]
            context_input[:, -1] = pred_output[d]
            action_input[:, :-1] = action_input[:, 1:]
            action_input[:, -1] = prescribed_ips

            if count_ == WINDOW_SIZE:
                count_ = 0
        inputs_ = np.array(inputs_).squeeze()
        return inputs_, prescriptions_output, pred_output

    def _lstm_get_test_rollouts(self, model, df, top_geos, country_samples):
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
            inputs, prescriptions, preds = self._lstm_roll_out_predictions(df, model, g,
                                               initial_context_input,
                                               initial_action_input,
                                               initial_costs_input,
                                               future_action_sequence)
            country_inputs[g] = inputs
            country_preds[g] = prescriptions
            country_cases[g] = preds

        return country_inputs, country_preds, country_cases

    def _most_affected_geos(self, df, nb_geos, min_historical_days):
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
    def _permute_data(self, X_context, X_action, X_costs, seed=301):
        # np.random.seed(seed)
        p = np.random.permutation(X_action.shape[0])
        X_context = X_context[p]
        X_action = X_action[p]

        return X_context, X_action, X_costs
