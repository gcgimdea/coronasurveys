Applying_Other_Methods_CI_Symp: Both of them compute, for 2020 and 2021, each of the models defined in the literature 100 times, with 100 division of train and test where we are able to create the confidence interval. Then we save the metrics F1, sensitivity, specificity and precision for each loop in a dataframe.
Applying_Other_Methods_CI_Symp_Zoabi: This was made to correct a certain error in the creation of the Zoabi metrics paper and just save this method.
Computing_CI_mean:Compute the mean of the 100 iterations means, high_CI and low_CI and then print a latex form of the table.
Low_High_TPR_tables: Create the tables for high and low TPR.
Creating_DF_to_plot_Sens_Spec_Prec: Create tables to plot for Sens vs Prec and Sens vs Spec.
plotting_Sensitivity_precision_specificity: Generate the plots for Sens vs Prec and Sens vs Spec.