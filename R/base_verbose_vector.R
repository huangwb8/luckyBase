

#' @title verbose_vector
#' @description verbose a vector. For example, report \code{c(1,2,3)} as \code{"1", "2", "3"}
#' @param vector a vector
#' @param tablename whether verbose table names of the vector
#' @importFrom tidyr %>%
#' @return a character
#' @author Weibin Huang<\email{654751191@@qq.com}>
#' @export
verbose_vector <- function(vector, tablename = T){
  if(tablename){
    vector <- names(table(vector, useNA = "always"))
  }
  vector %>% paste0('\"',.,'\"') %>% for(i in .) cat(i,sep = "",', ')
  # return(substring(v2, 1, (nchar(v2) - 1)))
}

