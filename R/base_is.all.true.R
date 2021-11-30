


#' @title Is all True?
#' @description Is all True?
#' @param vt a vector
#' @return logic
#' @author Weibin Huang<\email{654751191@@qq.com}>
#' @examples
#' a <- c(T,T,T);is.all.true(a)
#' a <- c(T,T,F);is.all.true(a)
#' b <- c(T,2);is.all.true(b)
#' c <- c(F,3);is.all.true(c)
#' @export
is.all.true <- function(vt){
  if(!is.logical(vt)){
    #说明有不是逻辑值的成分
    return(F)
  } else {
    #全是逻辑值
    l1 <- vt %in% T
    l2 <- all(l1==T)
    return(l2)
  }
}





