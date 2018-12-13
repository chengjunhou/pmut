



pmut.comp.cont <- function(datatable, varstring, targetstring, type="hist", binwidth=NULL, hist.bool=FALSE) {
  if (type=="hist") {
    pd <- ggplot(datatable, aes_string(targetstring, fill=varstring)) +
      geom_histogram(aes(y = ..density..), position="identity", binwidth=binwidth, alpha=.5) +
      scale_y_continuous(labels = scales::percent) +
      xlab(varstring) + ylab(paste(targetstring,"distribution"))
  } else {
    pd <- ggplot(datatable, aes_string(targetstring, colour=varstring)) +
      geom_freqpoly(aes(y = ..density..), binwidth=binwidth) +
      scale_y_continuous(labels = scales::percent) +
      xlab(varstring) + ylab(paste(targetstring,"distribution"))
  }

  if (hist.bool) {
    ph <- ggplot(datatable, aes_string(x=varstring)) +
      geom_bar(aes(y=(..count..)/sum(..count..)), fill="#F0E442", colour="grey30") +
      xlab(varstring) + ylab("Proportion")
    gridExtra::grid.arrange(pd, ph, nrow=2, heights=c(5,5))
  } else {
    pd
  }
}
