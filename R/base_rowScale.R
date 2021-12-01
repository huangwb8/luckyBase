
#' @title Row scale for expression matrix
#' @description Row scale for expression matrix
#' @param matrix matrix with gene row and sample col
#' @param log.convert whether to do log2 convertion before scaling
#' @seealso \code{\link[base]{scale}}.
#' @author Weibin Huang<\email{654751191@@qq.com}>
#' @export
rowScale <- function(matrix,log.convert = T){
  if(log.convert ==F){m1 <- matrix} else {
    m1 <- log2(matrix+1)
  }
  x1 <- apply(m1,1,scale)
  rownames(x1) <- colnames(matrix)
  return(t(x1))
}
