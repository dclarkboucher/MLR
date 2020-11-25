data(mtcars)
form <- mpg ~ wt + hp + hp:wt

m1 <- lm(formula = form, data = mtcars)
sm1 <- summary(m1)

m2 <- regression_fit(form, mtcars)

test_that("Coefficient estimates are accurate", {
  expect_equal(m2$coefficients,
               sm1$coefficients)
})

test_that("Fitted values are accurate", {
  expect_equal(m2$fitted_values,
               unname(m1$fitted.values))
})

test_that("Residuals are accurate", {
  expect_equal(m2$residuals,
               unname(m1$residuals))
})

test_that("Variance-covariance is correct", {
  expect_equal(unname(m2$variances),
               unname(vcov(m1)))
})

test_that("R-squared and Adjusted R-squared are correct", {
  expect_equal(unname(m2$r_squared),
               unname(c(sm1$r.squared, sm1$adj.r.squared)))
})

test_that("F test is correct", {
  expect_equal(unname(m2$anova$f_value),
               unname(sm1$fstatistic["value"]))
})

test_that("Confidence intervals are correct", {
  expect_equal(m2$confidence_intervals, confint(m1))
})
