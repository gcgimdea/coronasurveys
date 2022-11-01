
RemoveOutliersBeforeDummification <- function(df) {
  cutoff_coeff <- 1.5
  
  # Filtering with the open responses using upper wisker of boxplot
  # B2: For how many days have you had at least one of these symptoms?
  # any_symptom <- ((df$B1_1 == 1) | (df$B1_2 == 1) | (df$B1_3 == 1) | (df$B1_4 == 1) | (df$B1_5 == 1) |
  #                   (df$B1_6 == 1) | (df$B1_7 == 1) | (df$B1_8 == 1) | (df$B1_9 == 1) | (df$B1_10 == 1) |
  #                   (df$B1_11 == 1) | (df$B1_12 == 1))
  # vals <- df$B2[which(any_symptom & !is.na(df$B2))]
  # cutoff_B2 <- boxplot.stats(vals, coef=cutoff_coeff)$stats[5]
  # # B4: How many people do you know with these symptoms?
  # vals <- df$B4[which((df$B3==1) & !is.na(df$B4))]
  # cutoff_B4 <- boxplot.stats(vals, coef=cutoff_coeff)$stats[5]
  # # E5: How many people slept in the place where you stayed last night?
  # vals <- df$E5[which(!is.na(df$E5))]
  # cutoff_E5 <- boxplot.stats(vals, coef=cutoff_coeff)$stats[5]
  # # E6: How many years of education have you completed? (not available before Version 5)
  # vals <- df$E6[which(!is.na(df$E6))]
  # cutoff_E6 <- boxplot.stats(vals, coef=1.5)$stats[5]
  # # E7: How many rooms are used for sleeping in the place where you are staying?
  # vals <- df$E7[which(!is.na(df$E7))]
  # cutoff_E7 <- boxplot.stats(vals, coef=1.5)$stats[5]
  
  cutoff_B2 <- 100
  cutoff_B4 <- 100  # Upper bound on number of cases in the local community (B4)
  cutoff_E5 <- 100
  cutoff_E6 <- 100
  cutoff_E7 <- 100
  
  
  cat("* Before removing outliers:", "dim:", dim(df), unique(df$ISO2), "\n")
  
  df <- df[which(is.na(df$B2) | (df$B2 <= cutoff_B2)),]
  cat("* B2: Symptoms duration cutoff:", cutoff_B2, "dim:", dim(df), unique(df$ISO2), "\n")
  df <- df[which(is.na(df$B4) | (df$B4 <= cutoff_B4)),]
  cat("* B4: Contacts with CLI cutoff:", cutoff_B4, "dim:", dim(df), unique(df$ISO2), "\n")
  df <- df[which(is.na(df$E5) | (df$E5 <= cutoff_E5)),]
  cat("* E5: People sleeping together cutoff:", cutoff_E5, "dim:", dim(df), unique(df$ISO2), "\n")
  df <- df[which(is.na(df$E6) | (df$E6 <= cutoff_E6)),]
  cat("* E6: Years of education cutoff:", cutoff_E6, "dim:", dim(df), unique(df$ISO2), "\n")
  df <- df[which(is.na(df$E7) | (df$E7 <= cutoff_E7)),]
  cat("* E7: Rooms are used for sleeping:", cutoff_E7, "dim:", dim(df), unique(df$ISO2), "\n")
  
  # # Removing responses declaring having all symptoms (B1_13 and B1_14 not availabe in all dates)
  all_symptoms <- ((df$B1_1 == 1) & (df$B1_2 == 1) & (df$B1_3 == 1) & (df$B1_4 == 1) & (df$B1_5 == 1) &
                     (df$B1_6 == 1) & (df$B1_7 == 1) & (df$B1_8 == 1) & (df$B1_9 == 1) & (df$B1_10 == 1) &
                     # (df$B1_11 == 1) & 
                     (df$B1_12 == 1)) # & (df$B1_13 == 1) & (df$B1_14 == 1))
  df <- df[which(!all_symptoms), ]
  
  return(df)
}

RemoveBinaryOutliers <- function(Dftry1, selectedColumnsForOutliers){
  cutoff_coeff <- 1.5
  
  #The input for this Dftry1 should be the output of the dummies function
  dfOutlierDetect<-list()
  for (i in selectedColumnsForOutliers) {
    for (j in 0:2) {
      attr <- paste0(i,".",j)
      if (attr %in% colnames(Dftry1)) {
        dfOutlierDetect[[attr]] <- Dftry1[[attr]]
      } 
      # else {
      #   cat(attr, "not column\n")
      # }
    }
  }
  dfOutlierDetect <- as.data.frame(dfOutlierDetect)
  #Taking the mean of each column
  dfOutlierDetectMean <- colMeans(dfOutlierDetect)
  #Calculating the distance to the mean for each response 
  if (nrow(dfOutlierDetect)>0) {
    for (i in (1:ncol(dfOutlierDetect))) {
      dfOutlierDetect[,i] <- abs(dfOutlierDetect[,i] - dfOutlierDetectMean[i])
    }
    # Assign the mean distance as a column, and select just the ones lowers than the upper whisker
    Dftry1$Distance <- rowMeans(dfOutlierDetect)
    cutoff <- boxplot.stats(Dftry1$Distance, coef=cutoff_coeff)$stats[5]
    Dftry1<-Dftry1[Dftry1$Distance <= cutoff,]
    Dftry1$Distance <- NULL
  }
  return(Dftry1)
}

# arw for outlier detection 
arw_custom <- function(x, m0, c0, alpha, pcrit) {
  # Adaptive reweighted estimator for multivariate location and scatter
  # with hard-rejection weights and delta = chi2inv(1-d,p)
  #
  # Input arguments
  #   x:  Dataset (n x p)
  #   m0: Initial location estimator (1 x p)
  #   c0: Initial scatter estimator (p x p)
  #   alpha:  Maximum thresholding proportion
  #       (optional scalar, default: alpha = 0.025)
  #   pcrit: critical value for outlier probability
  #       (optional scalar, default values from simulations)
  #
  # Output arguments:
  #   m:  Adaptive location estimator (p x 1)
  #   c:  Adaptive scatter estimator (p x p)
  #   cn: Adaptive threshold (scalar)
  #   w:  Weight vector (n x 1)
  dm <- dim(x)
  n <- dm[1]
  p <- dm[2]
  # Critical value for outlier probability based on simulations for alpha=0.025
  if (missing(pcrit)) {
    if (p <= 10)
      pcrit <- (0.24 - 0.003 * p) / sqrt(n)
    if (p > 10)
      pcrit <- (0.252 - 0.0018 * p) / sqrt(n)
  }
  if (missing(alpha)){
    delta <- qchisq(0.975, p)
  } else{
    delta <- qchisq(1 - alpha, p)
  }
  d2 <- mahalanobis(x, m0, c0)
  d2ord <- sort(d2)
  dif <- pchisq(d2ord, p) - (0.5:n) / n
  i <- (d2ord >= delta) & (dif > 0)
  if (sum(i) == 0){
    alfan <- 0
  }else{
    alfan <- max(dif[i])
  }
  if (alfan < pcrit){
    alfan <- 0
  }
  if (alfan > 0){
    cn <- max(d2ord[n - ceiling(n * alfan)], delta)
  }
  else{
    cn <- Inf
  }
  w <- d2 < cn
  if (sum(w) == 0) {
    m <- m0
    c <- c0
  } else {
    m <- apply(x[w, ], 2, mean)
    c1 <- as.matrix(x - rep(1, n) %*% t(m))
    c <- (t(c1 * w) %*% c1) / sum(w)
  }
  list(m = m,
       c = c,
       cn = cn,
       w = w)
}

RemoveOutliersBeforeDummification_v2 <- function(df) {
  # select the numerical columns
  # B2 (c25, for how many days have you had at least one of these symptoms)
  # B4 (c41, how many people do you know with these symptoms)
  # E6 (c175, how many years of education do you have)
  # E5 (c177, how many people slept in the housing you are staying)
  dfnum <- df[, c("B2", "B4", "E6", "E5")]
  dfnum$orig_num <- 1:nrow(dfnum)
  
  # dfnum[dfnum == -77]  <- NA
  # dfnum[dfnum == -99]  <- NA
  
  row.names(dfnum) <- 1:nrow(dfnum)
  # apply multivariate outlier detection on complete cases
  dfnumc <- dfnum[complete.cases(dfnum[, 1: 4]), ]  
  robcov <- MASS::cov.mcd(dfnumc[1:4])
  ttmv <- arw_custom(x = dfnumc[1:4],
                     m0 = robcov$center,
                     c0 = robcov$cov)
  distss <- mahalanobis(dfnumc[1:4], robcov$center, robcov$cov)
  outliers_index <- which(distss > ttmv$cn)
  
  # remove these observations from original dtnzm data
  dfnum1 <-  dfnum[-as.numeric(names(outliers_index)), ]
  
  # Filtering with the open responses using upper wisker of boxplot
  # B2: For how many days have you had at least one of these symptoms?
  vals <- dfnum1$B2
  vals <- vals[(!is.na(vals) & (vals >= 0))]
  cutoff_B2 <- boxplot.stats(vals, coef=1.5)$stats[5]
  B2_suspect <- which(dfnum1$B2 > cutoff_B2 | dfnum1$B2 < 0)
  
  
  # B4: How many people do you know with these symptoms?
  vals <- dfnum1$B4
  vals <- vals[(!is.na(vals) & (vals >= 0))]
  cutoff_B4 <- boxplot.stats(vals, coef=1.5)$stats[5]
  B4_suspect <- which(dfnum1$B4 > cutoff_B4 | dfnum1$B4 < 0)
  
  # E5: How many people slept in the place where you stayed last night?
  vals <- dfnum1$E5
  vals <- vals[(!is.na(vals) & (vals >= 0))]
  cutoff_E5 <- boxplot.stats(vals, coef=1.5)$stats[5]
  E5_suspect <- which(dfnum1$E5 > cutoff_E5 | dfnum1$E5 < 0)
  
  # E6: How many years of education have you completed? (not available before Version 5)
  vals <- dfnum1$E6
  vals <- vals[(!is.na(vals) & (vals >= 0))]
  cutoff_E6 <- boxplot.stats(vals, coef=1.5)$stats[5]
  E6_suspect <- which(dfnum1$E6 > cutoff_E6 | dfnum1$E6 < 0)
  
  # collate outliers index
  outliers_index2 <- unique(c(B2_suspect, B4_suspect, E6_suspect, E5_suspect)) 
  
  # get the index of outliers
  non_outliers_index <- dfnum1$orig_num[- outliers_index2]
  
  # continue with original data
  df <- df[non_outliers_index, ]
  
  # df <- df[!(df$B2 > cutoff_B2),]
  # cat("* B2: Symptoms duration cutoff:", cutoff_B2, "dim:", dim(df), "\n")
  # df <- df[!(df$B4 > cutoff_B4),]
  # cat("* B4: Contacts with CLI cutoff:", cutoff_B4, "dim:", dim(df), "\n")
  # df <- df[!(df$E5 > cutoff_E5),]
  # Cat("* E5: People sleeping together cutoff:", cutoff_E5, "dim:", dim(df), "\n")
  # df <- df[!(df$E6 > cutoff_E6),]
  # Cat("* E6: Years of education cutoff:", cutoff_E6, "dim:", dim(df), "\n")
  
  # Removing responses declaring having all symptoms (B1_13 and B1_14 not availabe in all dates)
  all_symptoms <- ((df$B1_1 == 1) & (df$B1_2 == 1) & (df$B1_3 == 1) & (df$B1_4 == 1) & (df$B1_5 == 1) &
                     (df$B1_6 == 1) & (df$B1_7 == 1) & (df$B1_8 == 1) & (df$B1_9 == 1) & (df$B1_10 == 1) &
                     (df$B1_11 == 1) & (df$B1_12 == 1)) # & (df$B1_13 == 1) & (df$B1_14 == 1))
  df <- df[!all_symptoms, ]
  
  return(df)
}


