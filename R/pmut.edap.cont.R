#' Create Exploratory Visualization for Continuous Feature
#'
#' This function creates visualization with a line plot of a specified continuous feature against the response,
#' plus a distribution histogram for that feature.
#' In the line plot, the continuous feature will be cut into bins and then placed on the x-axis. The response will be the y-axis,
#' which will serve as Actual. Binning characteristics will be controlled by \code{meta} and \code{qbin}. NA will be formed as its own bin.
#' More lines of Prediction can be created by specifying a prediction \code{data.frame}.
#'
#' @param datatable Object of class \code{data.frame} or \code{data.table}
#' @param varstring Single character string indicating the column name inside \code{datatable} for the discrete feature
#' @param targetstring Single character string indicating the column name inside \code{datatable} for the response
#' @param meta Numeric vector with length of 4 (default is c(50,4,0.01,0.99)): 1st indicates number of bins,
#'   2nd indicates bin rounding digits, 3rd and 4th indicate the outlier percentile
#' @param qbin Logical (default is FALSE), FALSE indicates equal length bins, TRUE indicates equal weight bins (quantile view)
#' @param pred.df Object of class \code{data.frame} (optional), with column being prediction from each model
#' @return A view of line plot stacked above the histogram
#'
#' @import data.table
#' @import ggplot2
#' @export
#'
#' @examples
#' df = data.frame(ggplot2::diamonds)
#' pmut.edap.cont(df, "carat", "price")
#' pmut.edap.cont(df, "carat", "price", meta=c(12,2,0,1), qbin=TRUE)
#' pmut.edap.cont(df, "carat", "price", pred.df=data.frame(GLM=df$carat*7000-1000))

pmut.edap.cont <- function(datatable, varstring, targetstring, meta=c(50,4,0.01,0.99), qbin=FALSE, pred.df=NULL) {
  datatable = data.table(datatable)
  datatable$Exposure <- 1
  # color platte
  pal.rd = "#ff0000"
  pal.df = data.frame(col.name=c("Green", "Blue", "Orange", "Iron", "Purple"),
                      val.name=c("GR.VAL", "BL.VAL", "OR.VAL", "IR.VAL", "PU.VAL"),
                      color=c("#33CC33", "#0066ff", "#ffbf80", "#75a3a3", "#c266ff"),
                      stringsAsFactors = FALSE)
  if (qbin==FALSE) {
    # equal width bin
    cut.min = quantile(datatable[[varstring]], meta[3], na.rm=TRUE)
    cut.max = quantile(datatable[[varstring]], meta[4], na.rm=TRUE)
    cut.a = unname(round(cut.min, meta[2]))
    cut.z = unname(round(cut.max, meta[2]))
    cut.s = round((cut.z - cut.a)/meta[1], meta[2])
    if (cut.s==0) {
      stop("Width Cut Error: round-up digits need to be enlarged")
    }
    cut.seq = seq(from=cut.a, by=cut.s, length.out=meta[1]-1)
  } else {
    # equal count bin
    qun.seq = seq(from=meta[3], by=(meta[4]-meta[3])/(meta[1]-2), length.out=meta[1]-1)
    cut.seq = unname(quantile(datatable[[varstring]], qun.seq, na.rm=TRUE))
    cut.seq = round(cut.seq, meta[2])
    if (any(duplicated(cut.seq))) {
      stop("Quntile Cut Error: round-up digits need to be enlarged")
    }
  }
  # add min & max to head & tail
  hundred = as.integer(paste0("1", paste(rep("0",meta[2]),collapse="")))
  minvarstring = min(datatable[[varstring]], na.rm=TRUE)
  maxvarstring = max(datatable[[varstring]], na.rm=TRUE)
  if (cut.seq[1] > minvarstring) {
    cut.seq = c(floor(minvarstring*hundred)/hundred, cut.seq)
  }
  if (cut.seq[length(cut.seq)] < maxvarstring) {
    cut.seq = c(cut.seq, ceiling(maxvarstring*hundred)/hundred)
  }
  # cut varstring
  datatable[["BIN"]] = cut(datatable[[varstring]], cut.seq)
  datatable[["BIN"]][which(datatable[[varstring]]==minvarstring)] = levels(datatable[["BIN"]])[1]

  if (!is.null(targetstring) & is.null(pred.df)) {

    # sss: summary satistics
    sss = datatable[ ,.(VAL=mean(get(targetstring),na.rm=T), QTS=sum(Exposure)), by=BIN]
    names(sss)[1] = "VAR"

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
                        IR.VAL=mean(Iron,na.rm=T), PU.VAL=mean(Purple,na.rm=T), QTS=sum(Exposure)), by=BIN]
    names(sss)[1] = "VAR"
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
                        IR.VAL=mean(Iron,na.rm=T), PU.VAL=mean(Purple,na.rm=T), QTS=sum(Exposure)), by=BIN]
    names(sss)[1] = "VAR"
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

