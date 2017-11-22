#' Calculate Model Prediction Gini with Standardization
#'
#' This function calculates the standardized gini coefficient for model prediction.
#'
#' @param aa Vector of actuals, could be any value
#' @param pp Vector of predictions, could be any value
#' @param print Logical (defualt is FALSE), TRUE indicates printing the original gini before standardization
#' @return A single numeric value for standardized gini
#'
#' @export
#'
#' @examples
#' actuals = c(1,1,1,1,0,1,1,0,1,0,1,0,1,0,0,1,0,0,0,0)
#' predicts = rev(seq_along(actuals)); predicts[9:10] = mean(predicts[9:10])
#' pmut.gini(actuals, predicts, print=TRUE)


pmut.gini <- function(aa, pp, print=FALSE) {
  fun_gini <- function(aa, pp) {
    df = data.frame(aa = aa, pp = pp)
    df = df[order(df$pp, decreasing=TRUE),]
    df$random = (1:nrow(df))/nrow(df)
    totalPos = sum(df$aa)
    # obtain cumulative number of positive examples found (used for computing Model Lorentz)
    df$cumPosFound = cumsum(df$aa)
    # obtain cumulative proportion of positive examples found (Model Lorentz)
    df$Lorentz = df$cumPosFound / totalPos
    # obtain Lorentz minus random
    df$Gini = df$Lorentz - df$random
    return(sum(df$Gini))
  }
  GINI.ap = fun_gini(aa, pp)
  GINI.aa = fun_gini(aa, aa)
  if (print==TRUE) {
    cat("actual-prediction-gini=", GINI.ap, "; actual-actual-gini=", GINI.aa, "\n", sep="")
  }
  GINI = GINI.ap / GINI.aa
  return(GINI)
}

