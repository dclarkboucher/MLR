
#formula_full. Useful if formula of interest is nested in a full model.
#Complete cases will be selected based on full model so that no
#extra observations are included in analysis. Argument omitted if not nested.

regression_fit <- function(formula, data, formula_full = formula){

  data <- mtcars
  formula <- formula(mpg ~ disp + I(disp^2) + wt + as.factor(gear))

  #If formula full does not contain formula, ignore
  if(! all((all.vars(formula) %in% all.vars(formula_full)))){
    formula_full <- formula
    print("Notice: Full formula does not contain true formula. Argument omitted.")
  }

  #Subset data
  data <- as.data.frame(data)
  data <- data[,all.vars(formula_full)]
  row_select <- complete.cases(data)
  data <- data[row_select,all.vars(formula)]

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

  #Test estimates
  se_beta <- sqrt(colSums(var_covar * diag(p)))
  testStat <- beta / se_beta
  pvals <- 2*(1 - pt(abs(testStat), n-p))

  coefs <- matrix(data = NA, nrow = 4, ncol = p)
  colnames(coef) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  rownames(coef) <- colnames(x)

  coefs[["Estimates"]] <- beta
  coefs[["Std. Error"]] <- se_beta
  coefs[["t value"]] <- testStat
  coefs[["Pr(>|t|)"]] <- pvals

  #Analysis of variance
  fitted_values <- x %*% beta
  residuals <- y - fitted_values

  sse <- sum((residuals)^2)
  mse <- sse / (n-p)

  my <- mean(y)
  ssy <- sum((y - my)^2)
  ssr <- ssy - sse

  var_covar <- MSE * xtx_inv

  #R-squared
  rsq <- ssr/ssy
  rsq_adj <- 1 - (1-rsq) * (n-1) / (n-p)
  rsq_vec <- c(rsq,rsq_adj)

  #Prepare outputs
  outList <- c()
  outList$Formula <- formula
  outList$Residuals <- residuals
  outList$Fitted_values <- yfit
  outList$Coefficients <- pvals
  outList$Variance_covariance <- var_covar
  outList$R_squared <- c(r_sq,r_sq_adj)

}

