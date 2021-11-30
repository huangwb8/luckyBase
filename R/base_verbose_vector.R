

#' @title verbose_vector
#' @description verbose a vector. For example, report \code{c(1,2,3)} as \code{"1", "2", "3"}
#' @param vector a vector
#' @importFrom tidyr %>%
#' @return a character
#' @author Weibin Huang<\email{654751191@@qq.com}>
#' @export
verbose_vector <- function(vector){
  vector %>% paste0('\"',.,'\"') %>% for(i in .) cat(i,sep = "",', ')
  # return(substring(v2, 1, (nchar(v2) - 1)))

}

