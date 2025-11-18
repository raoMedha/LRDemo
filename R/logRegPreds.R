#' Predict from a logRegModel object
#'
#' @param object A \code{"logRegModel"} from \code{logRegModel()}
#' @param newdata New predictor data (matrix or data frame)
#' @param type "response" for probabilities, "class" for predicted labels
#' @param threshold Threshold for binary classification when \code{type = "class"}
#'  @param ... not used, only included for compatibility with \code{predict()}
#'
#' @return A numeric vector, factor, or matrix of probabilities depending on
#'   \code{family} and \code{type}
#' @method predict logRegModel
#' @export
predict.logRegModel <- function(object, newdata,
                        type = c("response", "class"),
                        threshold = 0.5) {
  type <- match.arg(type)
  X <- cbind(Intercept = 1, as.matrix(newdata))

  if (object$family == "binomial") {
    eta <- drop(X %*% object$beta)
    pHat <- 1 / (1 + exp(-eta))

    if (type == "response") {
      return(pHat)
    } else {
      if (length(object$levels) == 2L && !all(object$levels %in% c(0, 1))) {
        # map back to original factor levels
        i <- as.integer(pHat >= threshold) + 1L
        return(factor(object$levels[i], levels = object$levels))
      } else {
        return(as.numeric(pHat >= threshold))
      }
    }
  } else if (object$family == "multinomial") {
    betaMat <- object$beta  # p x K
    scores <- X %*% betaMat # n x K
    # softmax ROW-wise
    scoreShift <- sweep(scores, 1, apply(scores, 1, max), FUN = "-")
    expScores <- exp(scoreShift)
    rowSums <- rowSums(expScores)
    P <- expScores / rowSums

    if (type == "response") {
      colnames(P) <- object$levels
      return(P)
    } else {
      i <- max.col(P, ties.method = "first")
      return(factor(object$levels[i], levels = object$levels))
    }
  } else {
    stop("Unknown family in logRegModel!!!")
  }
}
