
F_test <- function(model_name){
  rss <- model_name$deviance
  tss <- model_name$null.deviance
  df_den <- model_name$df.residual
  df_num <- model_name$df.null - model_name$df.residual
  msm <- (tss - rss)/df_num
  msr <- rss/df_den
  F <- msm/msr
  p <- pf(F, df_num, df_den, lower.tail = FALSE)
  f_result <- data.frame(cbind(F, df_num, df_den, p))
  names(f_result) <- c("F-Statistic", "DF numerator", "DF denominator", "p-value")
  mformula <- model_name$formula
  h <- list(mformula, f_result)
  return(h)
}


F_change <- function(model1, model2){
  rss1 <- model1$deviance
  rss2 <- model2$deviance
  ss_change <- rss1 - rss2
  res.df1 <- model1$df.residual
  res.df2 <- model2$df.residual
  df_change <- res.df1 - res.df2
  msr <- (rss2)/res.df2
  msm <- ss_change/df_change
  F <- msm/msr
  p <- pf(F, df_change, res.df2, lower.tail = FALSE)
  m1results <- data.frame(cbind(res.df1, rss1, "", "", "", ""))
  names(m1results) <- c("Res.DF", "RSS", "M.DF", "SS_Change", "F-Statistic", "Pr(>F)")
  m2results <- data.frame(cbind(res.df2, rss2, df_change, ss_change, F, p))
  names(m2results) <- c("Res.DF", "RSS", "M.DF", "SS_Change", "F-Statistic", "Pr(>F)")
  results <- rbind(m1results, m2results)
  m1form <- model1$formula
  m2form <- model2$formula
  resultform <- c(m1form, m2form)
  result <- (list(resultform, results))
  return(result)
}



r_sq <- function(model_name) {
  r2 <- 1 - model_name$deviance/model_name$null.deviance
  return(r2)
}


r_sqAdj <- function(model_name) {
  r2ad <- 1 - ((model_name$deviance/model_name$df.residual)/
                 (model_name$null.deviance/model_name$df.null))
  return(r2ad)
}


sr2 <- function(model_name) {
  r2 <- 1 - model_name$deviance/model_name$null.deviance
  den_df <- model_name$df.residual
  t <- coef(summary(model_name))[, "t value"]
  t <- t[-1] # remove intercept
  sr <- c()
  sr2 <- c()
  for (i in 1:length(t)) {
    sr[i] <- t[i]*(sqrt((1-r2)/den_df))
  }
  sr2 <- (sr)^2
  return(sr2)
}

