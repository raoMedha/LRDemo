#' Fit logistic regression (binary OR multinomial) using gradient descent
#'
#' @param x Numeric matrix or data frame of predictors dim = (n x p)
#' @param y Outcome: can be binary numeric 0/1, factor, or character
#'   If there are only two unique values, a binary model is fit
#'   If there are more than two unique values, a one-vs-rest
#'   multinomial model!
#' @param family Either "binomial" or "multinomial". If missing,
#'   it is inferred from y!
#' @param lambda Greater than 0 L2 penalty on coefficients
#' @param lr Learning rate for gradient descent
#' @param nIter Number of gradient steps (i.e. iterations)
#'
#' @return An object of class \code{"logRegModel"} with components:
#'   \itemize{
#'     \item beta: coefficient vector (binary) or matrix (multinomial)
#'     \item family: "binomial" or "multinomial"
#'     \item levels: class labels for multinomial
#'     \item lambda, lr, nIter: tuning parameters
#'   }
#' @examples
#' set.seed(1)
#' x <- matrix(rnorm(200), ncol = 2)
#' yProb <- 1 / (1 + exp(-(x[,1] - x[,2])))
#' yBin <- rbinom(nrow(x), size = 1, prob = yProb)
#' fitBin <- logRegModel(x, yBin, family = "binomial")
#' pHat <- predict(fitBin, x, type = "response")
#' mean((pHat > 0.5) == yBin)
#'
#' # Multinomial example (3 classes)
#' yMult <- factor(sample(c("A", "B", "C"), nrow(x), replace = TRUE))
#' fitMulti <- logRegModel(x, yMult, family = "multinomial", nIter = 500)
#' predMulti <- predict(fitMulti, x, type = "class")
#' @export
logRegModel <- function(x, y,
                        family = c("binomial", "multinomial"),
                        lambda = 0,
                        lr = 0.1,
                        nIter = 1000) {
  x <- as.matrix(x)

  # Choose Binomial or Multinomial if not specified in function call
  if (missing(family)) {
    if (length(unique(y)) <= 2L) {
      family <- "binomial"
    } else {
      family <- "multinomial"
    }
  } else {
    family <- match.arg(family)
  }

  if (family == "binomial") {
    fit <- logRegBinFit(x, y, lambda = lambda, lr = lr, nIter = nIter)
  } else {
    fit <- logRegMNFit(x, y, lambda = lambda, lr = lr, nIter = nIter)
  }

  fit
}

# helper function --> binary logistic regression
logRegBinFit <- function(x, y, lambda, lr, nIter) {
  # Convert y to 0 or 1
  if (is.factor(y) || is.character(y)) {
    lev <- sort(unique(y))
    if (length(lev) != 2L) {
      stop("Binary family requires exactly 2 outcome categories!")
    }
    y01 <- as.numeric(factor(y, levels = lev)) - 1L
  } else {
    y01 <- as.numeric(y)
    if (!all(y01 %in% c(0, 1))) {
      stop("For binary family, y must be 0 or 1 OR a 2-level factor or character!")
    }
    lev <- sort(unique(y01))
  }

  n <- nrow(x)
  X <- cbind(Intercept = 1, x)
  p <- ncol(X)

  beta <- numeric(p)

  for (iter in seq_len(nIter)) {
    out <- logRegBinGradCpp(X, y01, beta)

    gradLL <- out$grad

    # L2 penalty on slopes only- not on intercept
    penalty <- c(0, lambda * beta[-1])
    grad <- gradLL + penalty

    beta <- beta - lr * grad
  }

  structure(
    list(
      beta = beta,
      family = "binomial",
      levels = lev,
      lambda = lambda,
      lr = lr,
      nIter = nIter
    ),
    class = "logRegModel"
  )
}

# helper function --> multinomial via one-vs-rest logistic
logRegMNFit <- function(x, y, lambda, lr, nIter) {
  if (!is.factor(y)) {
    y <- factor(y)
  }
  lev <- levels(y)
  K <- length(lev)
  if (K < 3L) {
    stop("Multinomial family requires at LEAST 3 outcome categories!!")
  }

  n <- nrow(x)
  X <- cbind(Intercept = 1, x)
  p <- ncol(X)

  # yInt: 0..K-1
  yInt <- as.integer(y) - 1L

  betaMat <- matrix(0, nrow = p, ncol = K)

  for (iter in seq_len(nIter)) {
    out <- logRegOvrCpp(X, yInt, betaMat)
    gradLL <- out$grad

    penalty <- betaMat
    penalty[1, ] <- 0  # no penalty on intercept
    penalty <- lambda * penalty

    grad <- gradLL + penalty

    betaMat <- betaMat - lr * grad
  }

  structure(
    list(
      beta = betaMat,
      family = "multinomial",
      levels = lev,
      lambda = lambda,
      lr = lr,
      nIter = nIter
    ),
    class = "logRegModel"
  )
}
