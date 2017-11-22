#' Simple Check on Percentage of Missing
#'
#' This function checks percenrage of NA (include empty string for character) for every column inside the data.
#'
#' @param DATA Object of class \code{data.frame} or \code{data.table}
#' @return A named vector having percent of missing for the column
#'
#' @export
#'
#' @examples
#' pmut.data.pmis(data.frame(ggplot2::diamonds))


pmut.data.pmis <- function(DATA) {
  percent_missing <- function(x) {
    y = (is.na(x) | x=="")
    pctg = sum(y)/length(x)
    return(pctg)
  }
  return(sapply(DATA, percent_missing))
}

