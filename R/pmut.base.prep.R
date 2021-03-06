#' Process Data to be Socred with Meta Information
#'
#' This function takes meta information generated by \code{\link{pmut.base.find}}, prepares new data so that it can be scored without error.
#' It conducts a few things: it handles missing value imputation either by assigning to base level (categorical) or mean value (numeric),
#' note that a new column marking the imputed numeric entry is generated in the mean time;
#' it assigns levels not found in meta but observed in new data to base level;
#' it handles levels found in meta but not observed in new data by treating the column as \code{factor};
#' it handles entire column found in meta but not observed in new data by imputing the entire column with its base or mean;
#' it attaches symbol "!" with every base level; lastly, it orders the columns alphabetically.
#' Note that data processed by this function will only have two classes: \code{factor} for categorical, \code{numeric} for numeric.
#' Then \code{model.matrix} will produce data matrix with exactly identical format as training,
#' so that it can be scored for a \code{glmnet} or \code{xgboost} model.
#'
#' @param DATA Object of class \code{data.frame} or \code{data.table}
#' @param CatMeta List of meta information for categorical features generated by \code{pmut.base.find}
#' @param NumMeta List of meta information for numeric features generated by \code{pmut.base.find}
#' @return A \code{data.frame} or \code{data.table} ready to be scored
#'
#' @export
#'
#' @examples
#' temp = pmut.base.find(data.frame(ggplot2::diamonds))
#' # remove two columns
#' newdata = data.frame(ggplot2::diamonds)[,-c(2,6)]
#' # generate na
#' newdata$price[5:15] = NA
#' # assign new color
#' newdata$color = "NEW"
#' # temp[[1]] categorical meta, temp[[2]] numeric meta
#' newdata = pmut.base.prep(newdata, temp[[1]], temp[[2]])
#' head(newdata)
#' sapply(newdata, class)


pmut.base.prep <- function(DATA, CatMeta, NumMeta) {
  cat.vec = unlist(lapply(CatMeta, function(x) x$VarString[1]))
  num.vec = unlist(lapply(NumMeta, function(x) x$VarString[1]))
  if (class(DATA)[1]=="data.table") {
    DATA = DATA[, intersect(names(DATA), c(cat.vec, num.vec)), with=F]
  } else {
    DATA = DATA[, intersect(names(DATA), c(cat.vec, num.vec))]
  }

  cat("====== Cat:", length(cat.vec), "Runs ======", "\n")
  for (i in 1:length(CatMeta)) {
    VarString = CatMeta[[i]]$VarString[1]
    LvlVec = CatMeta[[i]]$LvlVec
    BaseString = CatMeta[[i]]$LvlBase

    if (VarString %in% names(DATA)) {
      ### keep going if VarString is in DATA
      if (class(DATA[[VarString]])[1] %in% c("character","factor","logical","ordered")) {
        ## check feature class
        # temp contain the feature vector
        temp = as.character(DATA[[VarString]])
        temp = c(BaseString, temp)
        # impute missing
        temp[is.na(temp) | temp==""] <- BaseString
        # bin level not found in CatMeta to its base
        temp[!temp%in%LvlVec] <- BaseString
        # rename base level by adding !
        temp[temp==BaseString] <- paste0("!",BaseString)
        LvlVec[LvlVec==BaseString] <- paste0("!",BaseString)
        # treat categorical col as factor
        temp <- as.factor(temp)
        # prolong levels, levels must be subset of LvlVec
        levels(temp) <- c( levels(temp), LvlVec[!LvlVec%in%levels(temp)] )
        #levels(DATA[[VarString]])[levels(DATA[[VarString]])==BaseString] <- paste0("!",BaseString)
        DATA[[VarString]] <- temp[-1]
        cat("Loop", i, VarString, ": success", "\n")
      } else {
        ## check feature class
        cat("Error", i, VarString, ": class change", "\n")
      }
    } else {
      ### generate VarString if it is not in DATA
      temp = rep(BaseString,dim(DATA)[1])
      # rename base level by adding !
      temp[temp==BaseString] <- paste0("!",BaseString)
      LvlVec[LvlVec==BaseString] <- paste0("!",BaseString)
      # treat categorical col as factor
      temp <- as.factor(temp)
      # prolong levels, levels must be subset of LvlVec
      levels(temp) <- c( levels(temp), LvlVec[!LvlVec%in%levels(temp)] )
      DATA[[VarString]] <- temp
      cat("Warn", i, VarString, ": entire feature generated", "\n")
    }
  }

  cat("====== Num:", length(num.vec), "Runs ======", "\n")
  for (i in 1:length(NumMeta)) {
    VarString = NumMeta[[i]]$VarString[1]
    ValueMean = NumMeta[[i]]$ValueMean
    if (VarString %in% names(DATA)) {
      ### keep going if VarString is in DATA
      if (class(DATA[[VarString]])[1] %in% c("integer","numeric")) {
        ## check feature class
        temp = DATA[[VarString]]
        nabool = is.na(temp) | temp==""
        if (any(nabool)) {
          # generate indicator column
          DATA[[paste0(VarString,"_miss")]] <- factor(nabool, levels=c("TRUE","FALSE"))
          # impute missing
          temp[is.na(temp) | temp==""] <- ValueMean
          DATA[[VarString]] <- temp
        }
        cat("Loop", i, VarString, ": success", "\n")
      } else {
        ## check feature class
        cat("Error", i, VarString, ": class change", "\n")
      }
    } else {
      ### generate VarString if it is not in DATA
      DATA[[VarString]] <- ValueMean
      cat("Warn", i, VarString, ": entire feature generated", "\n")
    }
  }

  if (class(DATA)[1]=="data.table") {
    DATA = DATA[, order(names(DATA)), with=F]
  } else {
    DATA = DATA[, order(names(DATA))]
  }
  return(DATA)
}

