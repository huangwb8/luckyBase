
#' @title countListElement
#' @description Calculate the sum of bottom non-list objects in a list
#' @param list a list object
#' @return Numeric. The sum of bottom non-list objects in the list.
#' @author Weibin Huang<\email{hwb2012@@qq.com}>
#' @examples
#' l <- list(a=list(a1=1, a2=2), b=list(b1=1, b2=2))
#' countListElement(l) # 4
#' @export
countListElement <- function(list) {
  count <- 0
  for (item in list) {
    if (is.list(item)) {
      count <- count + countListElement(item)
    } else {
      count <- count + 1
    }
  }
  return(count)
}
