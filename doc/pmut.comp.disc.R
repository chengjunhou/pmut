#' Create Comparison Visualization for Discrete Feature
#'
#' This function creates count spreading of the targetting feature partitioned on a specified discrete feature.
#' In the line plot, the discrete feature will be the x-axis and the response be the y-axis,
#' which will serve as Actual. NA will be formed as its own level.
#' More lines of Prediction can be created by specifying a prediction \code{data.frame}.


pmut.comp.disc <- function(datatable, varstring, targetstring, hist.bool=FALSE) {
  pb <- ggplot(datatable, aes_string(x=varstring)) +
    geom_bar(aes_string(fill=targetstring), position = "fill") +
    scale_y_continuous(labels = scales::percent) +
    xlab(varstring) + ylab(paste(targetstring,"spread"))

  if (hist.bool) {
    ph <- ggplot(datatable, aes_string(x=varstring)) +
      geom_bar(aes(y=(..count..)/sum(..count..)), fill="#F0E442", colour="grey30") +
      xlab(varstring) + ylab("Proportion")
    gridExtra::grid.arrange(pb, ph, nrow=2, heights=c(5,5))
  } else {
    pb
  }
}
