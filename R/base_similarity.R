

#' @title Caluculate similarity of two vectors
#' @description Caluculate similarity of two vectors
#' @param vec1 A vector
#' @param vec2 A vector
#' @param method One of \code{"jaccard"}
#' \itemize{
#'   \item \code{jaccard} calculate similarity that doesn't depend on the order of elements in the vectors
#' }
#' @return Numeric that defines similarity.
#' @author Weibin Huang<\email{hwb2012@@qq.com}>
#' @examples
#' vec1 <- c(1, 2, 3, 4, 5, 1, 2)
#' vec2 <- c(3, 4, 5, 6, 7, 3, 4)
#' sim <- similarity(vec1, vec2, method='jaccard')
#' luckyVerbose("Jaccard similarity:", round(sim, 4))
#' @export
similarity <- function(vec1, vec2, method='jaccard'){
  if(method == 'jaccard'){
    res <- similarity_jaccard(vec1, vec2)
  } else {
    stop('similarity: wrong method. Please use one of "jaccard"!')
  }
  return(res)
}


####%%%%%%%%%%%%%%%%% Assistant functions %%%%%%%%%%%%%%%%%%%%%%####


similarity_jaccard <- function(vec1, vec2) {
  # 将向量转换为集合（去重）
  set1 <- unique(vec1)
  set2 <- unique(vec2)

  # 计算交集和并集
  intersection <- length(intersect(set1, set2))
  union <- length(union(set1, set2))

  # 计算 Jaccard 相似度
  jaccard_similarity <- intersection / union

  return(jaccard_similarity)
}










