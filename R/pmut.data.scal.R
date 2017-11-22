#' Simple Standardization
#'
#' This function standardizes every numeric column inside the data.
#'
#' @param DATA Object of class \code{data.frame} or \code{data.table}
#' @return A \code{data.frame} or \code{data.table} after standardization
#'
#' @export
#'
#' @examples
#' head(pmut.data.scal(data.frame(ggplot2::diamonds)))


pmut.data.scal <- function(DATA) {
  scale_var <- function(x) {
    avg = mean(x)
    std = sd(x)
    x_scale = (x-avg)/std
    return(x_scale)
  }
  classvec = lapply(DATA, class)
  bool = (classvec=="numeric" | classvec=="integer")
  if (class(DATA)[1]=="data.table") {
    DATA[,bool,with=F] = apply(DATA[,bool,with=F], 2, scale_var)
  } else {
    DATA[,bool] = apply(DATA[,bool], 2, scale_var)
  }
  return(DATA)
}

