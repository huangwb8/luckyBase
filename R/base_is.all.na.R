

#' @title Is all NA?
#' @description Is all NA?
#' @param vt a vector
#' @return logic
#' @author Weibin Huang<\email{654751191@@qq.com}>
#' @examples
#' a <- c(NA,NA);is.all.na(a)
#' b <- c(NA,2);is.all.na(b)
#' c <- c(1,3);is.all.na(c)
#' @export
is.all.na <- function(vt){
  l2 <- table(is.na(vt))
  l2.i <- F %in% names(l2) #FALSE则说明没有F,即全为空NA值。
  if(!l2.i){
    #全为NA值
    return(T)
  } else {
    return(F)
  }
}

