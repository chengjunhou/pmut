#' Create Exploratory Visualization for Discrete Feature
#'
#' This function creates visualization with a line plot of a specified discrete feature against the response,
#' plus a distribution histogram for that feature.
#' In the line plot, the discrete feature will be the x-axis and the response be the y-axis,
#' which will serve as Actual. NA will be formed as its own level.
#' More lines of Prediction can be created by specifying a prediction \code{data.frame}.
#'
#' @param datatable Object of class \code{data.frame} or \code{data.table}
#' @param varstring Single character string indicating the column name inside \code{datatable} for the discrete feature
#' @param targetstring Single character string indicating the column name inside \code{datatable} for the response
#' @param pred.df Object of class \code{data.frame} (optional), with column being prediction from each model
#' @return A view of line plot stacked above the histogram
#'
#' @import data.table
#' @import ggplot2
#' @export
#'
#' @examples
#' df = data.frame(ggplot2::diamonds)
#' pmut.edap.disc(df, "color", "price")
#' pmut.edap.disc(df, "color", "price", pred.df=data.frame(GLM=as.numeric(df$color)*400+2600))


pmut.edap.disc <- function(datatable, varstring, targetstring, pred.df=NULL) {
  datatable = data.table(datatable)
  datatable$Exposure <- 1
  # color platte
  pal.rd = "#ff0000"
  pal.df = data.frame(col.name=c("Green", "Blue", "Orange", "Iron", "Purple"),
                      val.name=c("GR.VAL", "BL.VAL", "OR.VAL", "IR.VAL", "PU.VAL"),
                      color=c("#33CC33", "#0066ff", "#ffbf80", "#75a3a3", "#c266ff"),
                      stringsAsFactors = FALSE)

  if (!is.null(targetstring) & is.null(pred.df)) {

    # sss: summary satistics
    sss = datatable[ ,.(VAL=mean(get(targetstring),na.rm=T), QTS=sum(Exposure)), by=varstring]
    names(sss)[1] = "VAR"
    sss$VAR = addNA(sss$VAR, ifany=T)
    pl <- ggplot(sss,aes(VAR,VAL,group=1)) +
      geom_line(colour=pal.rd) + geom_point(colour=pal.rd) +
      theme(axis.text.x=element_text(angle=90,hjust=1)) +
      xlab(varstring) + ylab(targetstring)

  } else if (is.null(targetstring) & !is.null(pred.df)) {

    # processing pred.df
    Nrow = dim(pred.df)[1]; Ncol = dim(pred.df)[2]
    LineName = names(pred.df)
    pred.df = cbind(pred.df, data.frame(matrix(0,nrow=Nrow,ncol=5-Ncol)))
    names(pred.df) = pal.df$col.name
    datatable <- cbind(datatable, pred.df)
    # sss: summary satistics
    sss = datatable[ ,.(GR.VAL=mean(Green,na.rm=T), BL.VAL=mean(Blue,na.rm=T), OR.VAL=mean(Orange,na.rm=T),
                        IR.VAL=mean(Iron,na.rm=T), PU.VAL=mean(Purple,na.rm=T), QTS=sum(Exposure)), by=varstring]
    names(sss)[1] = "VAR"
    sss$VAR = addNA(sss$VAR, ifany=T)
    # subsetting sss
    sss.sub = as.data.frame(sss)[,1:(1+Ncol)]
    sss.sub.mlt = melt(sss.sub, id.var=1)
    # plotting
    pl <- ggplot(sss.sub.mlt, aes(x=VAR,group=variable)) +
      geom_line(aes(y=value,colour=variable)) + geom_point(aes(y=value,colour=variable)) +
      scale_colour_manual(values=pal.df$color, labels=LineName) +
      theme(axis.text.x=element_text(angle=90,hjust=1),legend.position="top",legend.title = element_blank()) +
      xlab(varstring) + ylab(targetstring)

  } else if (!is.null(targetstring) & !is.null(pred.df)) {

    # processing pred.df
    Nrow = dim(pred.df)[1]; Ncol = dim(pred.df)[2]
    LineName = names(pred.df)
    pred.df = cbind(pred.df, data.frame(matrix(0,nrow=Nrow,ncol=5-Ncol)))
    names(pred.df) = pal.df$col.name
    datatable <- cbind(datatable, pred.df)
    # sss: summary satistics
    sss = datatable[ ,.(VAL=mean(get(targetstring),na.rm=T), GR.VAL=mean(Green,na.rm=T), BL.VAL=mean(Blue,na.rm=T), OR.VAL=mean(Orange,na.rm=T),
                        IR.VAL=mean(Iron,na.rm=T), PU.VAL=mean(Purple,na.rm=T), QTS=sum(Exposure)), by=varstring]
    names(sss)[1] = "VAR"
    sss$VAR = addNA(sss$VAR, ifany=T)
    # subsetting sss
    sss.sub = as.data.frame(sss)[,1:(2+Ncol)]
    sss.sub.mlt = melt(sss.sub, id.var=1)
    # plotting
    pl <- ggplot(sss.sub.mlt, aes(x=VAR,group=variable)) +
      geom_line(aes(y=value,colour=variable)) + geom_point(aes(y=value,colour=variable)) +
      scale_colour_manual(values=c(pal.rd,pal.df$color), labels=c("Actual",LineName)) +
      theme(axis.text.x=element_text(angle=90,hjust=1),legend.position="top") +
      guides(colour=guide_legend(title=NULL,nrow=1)) +
      xlab(varstring) + ylab(targetstring)

  } else {
    return(NULL)
  }

  ph <- ggplot(sss,aes(VAR,QTS)) +
    geom_bar(stat="identity", aes(y=(QTS/sum(QTS))), fill="#F0E442", colour="grey30") +
    theme(axis.text.x=element_text(angle=90,hjust=1)) + scale_y_continuous(labels=scales::percent) +
    xlab(varstring) + ylab("Proportion")

  gridExtra::grid.arrange(pl, ph, nrow=2, heights=c(5,5))
}

