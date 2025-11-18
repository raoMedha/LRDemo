test_that("binary logRegModel matches glm on simulated data", {
  set.seed(123)
  x <- matrix(rnorm(200), ncol = 2)
  p <- 1 / (1 + exp(-(x[, 1] - x[, 2])))
  y <- rbinom(nrow(x), 1, p)

  fit_demo  <- logRegModel(x, y, family = "binomial",
                           lambda = 0, lr = 0.1, nIter = 500)
  phat_demo <- predict(fit_demo, x, type = "response")

  dat <- data.frame(y = y, x1 = x[, 1], x2 = x[, 2])
  fit_glm  <- glm(y ~ x1 + x2, data = dat, family = binomial())
  phat_glm <- predict(fit_glm, type = "response")

  expect_gt(cor(phat_demo, phat_glm), 0.99)
  expect_lt(mean(abs(phat_demo - phat_glm)), 0.05)
})

