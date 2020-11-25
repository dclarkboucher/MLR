#' regression_fit
#'
#' Fits multiple linear regression model using ordinary least squares
#'
#' @param formula an object of class "formula" or one that can be coerced by as.formula
#' @param data an object of class "data.frame" or one that can be coerced by as.data.frame.
#' @param alpha an optional argument indicating significance level of confidence intervals. If not provided, 0.05 is used.
#'
#' @return A list containing model details, estimated coefficients, confidence intervals,
#' fitted values, residuals, variance-covariance matrix, and R-squared.
#'
#' @examples
#' data(mtcars)
#' mlr_output <- regression_fit(formula = mpg ~ wt + hp + wt:hp, data = mtcars)
#'
#' mlr_coefficients <- mlr_output$coefficients
#' wt_estimate <- mlr_coefficients["wt","Estimate"]
#'
#' mlr_residuals <- mlr_output$residuals
#' mlr_vcov <- mlr_output$variances
#' @import stats
#' @export

regression_fit <- function(formula, data, alpha = 0.05){

  out_list <- c()

  #Coerce inputs
  formula <- as.formula(formula)
  data <- as.data.frame(data)

  out_list$formula <- formula

  #Produce X and Y objects
  x <- model.matrix(formula,data)
  y <- model.response(model.frame(formula,data))
  n <- nrow(x)
  p <- ncol(x)

  out_list$dimensions <- c(n, p)
  names(out_list$dimensions) <- c("n", "p")

  #Obtain estimates, fitted values, and residuals
  xt <- t(x)
  xtx_inv <- solve(xt %*% x)
  xtx_inv_xt <- xtx_inv %*% xt
  beta <- xtx_inv_xt %*% y

  fitted_values <- x %*% beta
  residuals <- y - fitted_values

  out_list$fitted_values <- as.vector(fitted_values)
  out_list$residuals <- as.vector(residuals)

  #Compute variance and covariance
  sse <- sum((residuals)^2)
  mse <- sse / (n - p)

  var_covar <- mse * xtx_inv
  colnames(var_covar) <- colnames(x)
  rownames(var_covar) <- colnames(x)

  out_list$variances <- var_covar

  #Test estimates using variance
  se_beta <- sqrt(colSums(var_covar * diag(p)))
  test_stat <- beta / se_beta
  pvals <- 2 * (1 - pt(abs(test_stat), n - p))

  coefs <- as.data.frame(matrix(data = NA, nrow = p, ncol = 4))
  colnames(coefs) <- c("Estimate","Std. Error","t value","Pr(>|t|)")
  rownames(coefs) <- colnames(x)

  coefs[["Estimate"]] <- beta
  coefs[["Std. Error"]] <- se_beta
  coefs[["t value"]] <- test_stat
  coefs[["Pr(>|t|)"]] <- pvals

  out_list$coefficients <- as.matrix(coefs)

  #Compute confidence intervals of estimates
  confint_low <- beta - se_beta * qt(1 - alpha/2, n - p)
  confint_up <-beta + se_beta * qt(1 - alpha/2, n - p)

  confidence_intervals <- cbind(confint_low, confint_up)
  rownames(confidence_intervals) <- colnames(x)
  colnames(confidence_intervals) <- paste(100 * c(alpha/2, 1 - alpha/2), "%")

  out_list$confidence_intervals <- confidence_intervals

  #Conduct ANOVA F test
  ssy <- sum((y - mean(y))^2)
  ssr <- ssy - sse
  msr <- ssr / (p - 1)

  sum_of_squares <- c(ssr, sse, ssy)
  names(sum_of_squares) <- c("SSR", "SSE", "SSY")

  mean_square <- c(msr, mse, ssy/(n - 1))
  names(mean_square) <- c("MSR", "MSE", "MSY")

  f_test <- c()
  f_test$ss <- sum_of_squares
  f_test$ms <- mean_square
  f_test$f_value <- msr / mse
  f_test$pval <- 1 - pf(f_test$f_value, p - 1, n - p)

  out_list$anova <- f_test

  #Compute R-squared and adjusted R-squared
  rsq <- ssr / ssy
  rsq_adj <- 1 - (1 - rsq) * (n - 1) / (n - p)
  rsq_vec <- c(rsq,rsq_adj)
  names(rsq_vec) <- c("Standard", "Adjusted")

  out_list$r_squared <- rsq_vec

  return(out_list)
}

