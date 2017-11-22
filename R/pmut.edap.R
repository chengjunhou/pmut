#' Create Exploratory Visualization for A Vector of Features
#'
#' This function creates visualization for a vector of features, using either \code{\link{pmut.edap.disc}} or \code{\link{pmut.edap.cont}},
#' depending on the feature class. Features of class \code{factor}, \code{character}, and \code{logical} will use \code{pmut.edap.disc};
#' Feature of class \code{numeric} will use \code{pmut.edap.cont};
#' Feature of class \code{integer} with unique values smaller than number of bins specified
#' by \code{meta} will use \code{pmut.edap.disc}, otherwise use \code{pmut.edap.cont}.
#' Some progression information will be printed on console.
#'
#' @param datatable Object of class \code{data.frame} or \code{data.table}
#' @param varvec Vector of character indicating the column names inside \code{datatable} to productionalize the visulization
#' @param targetstring Single character string indicating the column name inside \code{datatable} for the response
#' @param meta Numeric vector with length of 4 (default is c(50,4,0.01,0.99)): 1st indicates number of bins,
#'   2nd indicates bin rounding digits, 3rd and 4th indicate the outlier percentile
#' @param qbin Logical (default is FALSE), FALSE indicates equal length bins, TRUE indicates equal weight bins (quantile view)
#' @param pred.df Object of class \code{data.frame} (optional), with column being prediction from each model
#' @return Views of line plot stacked above the histogram varied by the feature
#'
#' @import data.table
#' @import ggplot2
#' @export
#'
#' @examples
#' # Output the plots into a pdf file
#' pdf("EDA_Diamonds.pdf", width=12, height=10)
#' df = data.frame(ggplot2::diamonds)
#' pmut.edap(df, names(df)[-7], "price")
#' dev.off()


pmut.edap <- function(datatable, varvec, targetstring, meta=c(50,4,0.01,0.99), qbin=FALSE, pred.df=NULL) {
  cat("============", length(varvec), "============\n")
  for (i in 1:length(varvec)) {
    ### feature not found
    if (!varvec[i] %in% names(datatable)) {
      cat("Error", i, varvec[i], ": not in data", "\n")
      next
    }
    ### feature all na
    Class = class(datatable[[varvec[i]]])
    if (all(is.na(datatable[[varvec[i]]])) | all(datatable[[varvec[i]]]=="")) {
      cat("NA", i, varvec[i], "\n")

    ### feature valid
    } else {
      ## discrete feature
      if (Class[1] %in% c("character","factor","logical","ordered")) {
        pmut.edap.disc(datatable, varvec[i], targetstring, pred.df)
      ## continuous feature
      } else if (Class[1] == "numeric") {
        pmut.edap.cont(datatable, varvec[i], targetstring, meta, qbin, pred.df)
      } else if (Class[1] == "integer") {
        # integer feature as discrete
        if (length(unique(datatable[[varvec[i]]])) <= meta[1]) {
          datatable[[varvec[i]]] = as.character(datatable[[varvec[i]]])
          pmut.edap.disc(datatable, varvec[i], targetstring, pred.df)
        # integer feature as continuous
        } else {
          pmut.edap.cont(datatable, varvec[i], targetstring, meta, qbin, pred.df)
        }
      ## other rare class feature
      } else {
        cat("Error", i, varvec[i], ": check feature class", "\n")
      }
      cat("Loop", i, varvec[i], ":", Class, "\n")
    }
  }
}

