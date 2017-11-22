#' Simple Check on Duplication
#'
#' This function checks if there is any duplicated column inside the data.
#'
#' @param DATA Object of class \code{data.frame} or \code{data.table}
#' @return A named bool vector indicating whether the column is duplicated
#' 
#' @export
#' 
#' @examples
#' plds.data.same(data.frame(ggplot2::diamonds))


plds.data.same <- function(DATA) {
  boolvec = duplicated(as.list(DATA))
  names(boolvec) = names(DATA)
  return(boolvec)
}

