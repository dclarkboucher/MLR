#' regression_fit
#'
#' Fits multiple regression model
#'
#' @param formula an object of class "formula" or one that can be coerced by as.formula
#' @param data an object of class "data.frame" or one that can be coerced by as.data.frame.
#'
#' @return A list containing the model formula, fitted values, a table of coefficients with test results, and the results of an ANOVA test.
#'
#' @examples
#' data(mtcars)
#' model_fit <- regression_fit(formula = mpg ~ wt + hp + wt:hp, data = mtcars)
#'
#'
#' @export

regression_fit <- function(formula, data){
  formula <- as.formula(formula)
  data <- as.data.frame(data)
  #Produce X and Y objects
  x <- model.matrix(formula,data)
  y <- model.response(model.frame(formula,data))
  n <- nrow(x)
  p <- ncol(x)

  #Obtain estimates
  xt <- t(x)
  xtx_inv <- solve(xt %*% x)
  xtx_inv_xt <- xtx_inv %*% xt
  beta <- xtx_inv_xt %*% y

  #Analysis of variance
  fitted_values <- x %*% beta
  residuals <- y - fitted_values

  sse <- sum((residuals)^2)
  mse <- sse / (n-p)

  my <- mean(y)
  ssy <- sum((y - my)^2)
  ssr <- ssy - sse

  var_covar <- mse * xtx_inv

  #Test estimates
  se_beta <- sqrt(colSums(var_covar * diag(p)))
  testStat <- beta / se_beta
  pvals <- 2*(1 - pt(abs(testStat), n-p))
  coefs <- as.data.frame(matrix(data = NA, nrow = p, ncol = 4))
  colnames(coefs) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  rownames(coefs) <- colnames(x)

  coefs[["Estimates"]] <- beta
  coefs[["Std. Error"]] <- se_beta
  coefs[["t value"]] <- testStat
  coefs[["Pr(>|t|)"]] <- pvals

  #R-squared
  rsq <- ssr/ssy
  rsq_adj <- 1 - (1-rsq) * (n-1) / (n-p)
  rsq_vec <- c(rsq,rsq_adj)

  #Prepare outputs
  outList <- c()
  outList$Formula <- formula
  outList$Residuals <- residuals
  outList$Fitted_values <- fitted_values
  outList$Coefficients <- pvals
  outList$Variance_covariance <- var_covar
  outList$R_squared <- c(rsq,rsq_adj)
  return(3)
}

