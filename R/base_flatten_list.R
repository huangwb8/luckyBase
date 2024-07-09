#' @title Flatten a complex list to a 1-D list
#' @description Flatten a complex list to a 1-D list
#' @param lst a list
#' @return 1-D list
#' @author Weibin Huang<\email{hwb2012@@qq.com}>
#' @export
flatten_list <- function(lst) {

  result <- list()

  flatten <- function(lst) {
    for (elem in lst) {
      if (is.list(elem)) {
        flatten(elem)
      } else {
        result <<- c(result, elem)
      }
    }
  }

  flatten(lst)

  return(result)
}
