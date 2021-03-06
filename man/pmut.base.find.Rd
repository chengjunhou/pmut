% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pmut.base.find.R
\name{pmut.base.find}
\alias{pmut.base.find}
\title{Obtain Meta Information for Each Column}
\usage{
pmut.base.find(DATA)
}
\arguments{
\item{DATA}{Object of class \code{data.frame} or \code{data.table}}
}
\value{
A list of two elements, 1st being meta information for categorical columns, 2nd for numeric columns
}
\description{
This function finds the meta information for each column within training data,
which will be used to process testing and/or new data so that it can be scored without error,
check \code{\link{pmut.base.prep}} for the part of testing data processing.
Meta information for columns of class \code{factor}, \code{character}, and \code{logical} will be stored in one list.
Each element of the list contains three slots: 1st \code{$VarString} is column name, 2nd \code{$LvlVec} is vector of unique levels,
3rd \code{$LvlBase} is base level name which is the level with most counts.
Meta information for columns of class \code{integer}, and \code{numeric} will be stored in another list.
Each element of the list contains two slots: 1st \code{$VarString} is column name,
2nd \code{$ValueMean} is its value mean (median for integer).
}
\examples{
temp = pmut.base.find(data.frame(ggplot2::diamonds))
temp[[1]]  # categorical meta
temp[[2]]  # numeric meta
}
