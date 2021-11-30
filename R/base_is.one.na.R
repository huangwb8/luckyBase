

#' @title Is one NA?
#' @description Is one NA?
#' @param vt a vector
#' @return logic
#' @author Weibin Huang<\email{654751191@@qq.com}>
#' @examples
#' a <- c(NA,NA);is.one.na(a)
#' b <- c(NA,2);is.one.na(b)
#' c <- c(1,3);is.one.na(c)
#' @export
is.one.na <- function(vt){
  l2 <- table(is.na(vt))
  l2.i <- T %in% names(l2) #FALSE则说明没有F,即全为空NA值。
  if(l2.i){
    #说明至少有一个NA值在向量里
    return(T)
  } else {
    return(F)
  }
}

