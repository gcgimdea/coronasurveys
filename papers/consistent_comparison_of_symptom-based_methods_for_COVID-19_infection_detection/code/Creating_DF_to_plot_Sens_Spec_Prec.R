Df_other = read.csv("./IMDEA/comparing_our_model/Plots/2021_results/df_Other_Final_CI_BR.csv")
Df_ours = read.csv("./IMDEA/comparing_our_model/Plots/2021_results/df_Our_Final_CI_BR.csv")
Df_ours_select = read.csv("./IMDEA/comparing_our_model/Plots/2021_results/df_Our_Final_selection_CI_BR.csv")
Df_ours_sensitivity_specificty <- Df_ours[c(3,4,7,8,11,12,15,16),]
Df_ours_select_sensitivity_specificty <- Df_ours_select[c(3,4,7,8,11,12,15,16,19,20,23,24,27,28),]
Df_to_plot =cbind(
  c(Df_other$mean_Sensitivity,Df_ours_sensitivity_specificty[c(2,4,6,8),2],Df_ours_select_sensitivity_specificty[c(2,4,6,8,10,12,14),2]),
  c(Df_other$mean_Specificity,Df_ours_sensitivity_specificty[c(1,3,5,7),2],Df_ours_select_sensitivity_specificty[c(1,3,4,7,9,11,13),2]))

colnames(Df_to_plot) <- c("Sensitivity","Specificity")
rownames(Df_to_plot) <- c(Df_other$X,"Lasso","Lg","GBC","RFC","Lg_select","GBC_select","RFC_select","Lg_SHAP","GBC_SHAP","RFC_SHAP","RFC_best")

Df_to_plot
