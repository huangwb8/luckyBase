

#' @title Classify a vector by specified classifier
#' @description classify help classify a vector by specified classifier
#' @param vector a vector
#' @param classifier  a list of classifier.
#' @param cover if \code{cover=T}(default), informations not provided by \code{classifier} would be annotated as \code{Not Available}. Otherwise, they would be kept as raw.
#' @param useNA If the name of classification is \code{NA} and \code{useNA=TRUE}, it would be turned into \code{NA} value. Default is \code{FAULT}
#' @details
#' 2021-10-08: If the name of classification is \code{NA}, it would be turned into \code{NA} value.
#' @author Weibin Huang<\email{654751191@@qq.com}>
#' @examples
#' ## example
#' vector <- 1:100
#'
#' ## classifier exist
#' classifier1 <- list(
#'   lower = c(1:49),
#'   upper = c(50:90)
#' )
#' v1 <- classify(vector,classifier1)
#' table(v1)
#'
#' ## upper not exist
#' classifier2 <- list(
#'   lower = c(1:49),
#'   upper = c(101:900)
#' )
#' v1 <- classify(vector,classifier2)
#' table(v1)
#' @export
classify <- function(vector, classifier, cover=T, useNA=F){
  vector <- as.character(vector)

  ## standard vector
  if(cover){
    vector1 <- rep("Not Available",length(vector))
  } else {
    vector1 <- vector
  }

  ## replace
  for(i in 1:length(classifier)){ # i = 1
    n.i <- names(classifier)[i]
    c.i <- as.character(classifier[[i]])
    if(n.i=='NA' & useNA){
      # NA annotation
      vector1[vector %in% c.i] <- NA
    } else {
      vector1[vector %in% c.i] <- n.i
    }
  }
  return(vector1)
}















