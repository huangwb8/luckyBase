

#' @title Fastextra
#' @description A fast way to extra part of elements in a vector
#' @param vt A character vector
#' @param split The split string
#' @param n The position of string you want to select. If \code{NULL}, then all the substrings would be used.
#' @return a character or a list
#' @author Weibin Huang<\email{654751191@@qq.com}>
#' @export
Fastextra <- function(vt,split,n=NULL){
  vt <- as.character(vt)
  get1 <- function(i,split,n=NULL){
    if(is.null(n)){
      vt1.i <- unlist(strsplit(i,split)) #全部输出
    } else {
      vt1.i <- unlist(strsplit(i,split))[n]
    }
    return(vt1.i)
  }
  vt1 <- apply(as.matrix(vt),1,function(z)get1(z,split = split,n = n))
  vt1 <- as.vector(vt1)
  return(vt1)
}

