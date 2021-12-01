
#' @title merge matrix of duplicates in row/col orientation
#' @description sometime we want to merge duplicate IDs(for example,gene id or patient id) by specified strategy(for example,if we merge gene expression data, \code{\link{mean}} is commonly used). \code{mergeMatrixDup} provide a convenient way to do this job.
#' @param x matrix or data frame
#' @param mergeCol whether to merge duplicate in col orientation
#' @param mergeRow whether to merge duplicate in row orientation
#' @param fun_col the function used in col merge
#' @param fun_row the function used in row merge
#' @param refCol Character. the reference of col merge. It must be aligned precisely with col IDs in the \code{x}, otherwise the result is not reliable.
#' @param refRow Character. the reference of row merge. It must be aligned precisely with row IDs in the \code{x}, otherwise the result is not reliable. if \code{refRow=NULL}, \code{mergRow} would be forced to \code{F}
#' @param parallel parallel method is still in BETA.
#' @return repaired data of input
#' @seealso \code{\link{tapply}}; \code{\link{apply}}
#' @author Weibin Huang<\email{654751191@@qq.com}>
#' @examples
#' set.seed(2018);dimname <- list(rownames=paste("row",sample(1:100,100,replace = T),sep = "_"),colnames=paste("col",sample(1:100,100,replace = T),sep = "_"))
#' set.seed(2019);dat <- sample(1:1000,10000,replace = T);dat <- matrix(dat,nrow=100,dimnames = dimname )
#'
#' ## There are some duplicates IDs in both col and row orientation
#' table(duplicated(rownames(dat))) # T=38
#' table(duplicated(colnames(dat))) # T=36
#'
#' ## merge duplicates
#' refCol = dimname[["colnames"]]
#' refRow = dimname[["rownames"]]
#' x2 <- mergeMatrixDup(dat,
#'                      mergeCol = T,
#'                      refCol = dimname[["colnames"]],
#'                      fun_col = mean,
#'                      mergeRow = T,
#'                      refRow = dimname[["rownames"]],
#'                      fun_row = mean)
#'
#' ## test whether the result is correct(All TRUE)
#' mean(dat[refRow %in% "row_7","col_28"]) == x2["row_7","col_28"]
#' mean(dat[refRow %in% "row_77","col_28"]) == x2["row_77","col_28"]
#' mean(dat["row_34",refCol %in% "col_42"]) == x2["row_34","col_42"]
#' mean(dat["row_47",refCol %in% "col_73"]) == x2["row_47","col_73"]
#' @export
mergeMatrixDup <- function(x,
                           mergeCol = T,
                           fun_col = mean,
                           refCol = NULL,
                           mergeRow = T,
                           fun_row = mean,
                           refRow = NULL,
                           parallel = T){

  ## select duplicate data for row
  if(mergeRow & !is.null(refRow)){
    LuckyVerbose("Merge duplicate for row...")
    dupID_row <- refRow[duplicated(refRow)]
    logi <- refRow %in% dupID_row
    x2 <- x[logi, ] # View(x[!logi, ])
    ref <- refRow[grep(T,logi)]
    x2 <- apply(x2,2,function(x)tapply(x,ref,fun_row))

    if(length(ref)== 2){
      x2 <- matrix(x2,nrow = 1,byrow = F,dimnames = list(unique(ref),names(x2)))
      }

    merN <- rownames(x2)
    x2 <- rbind(x[!logi, ],x2)
    rownames(x2)<- c(refRow[!logi],merN)
  } else {
    LuckyVerbose("Ignore duplicate for row.")
    x2 <- x
  }

  ## select duplicate data for col
  if(mergeCol){
    LuckyVerbose("Merge duplicate for col...")
    ## new data
    x <- x2

    ## refCol
    if(is.null(refCol)) refCol = colnames(x)

    ## select duplicate data
    dupID_Col <- refCol[duplicated(refCol)]
    logi <- refCol %in% dupID_Col
    x2 <- x[,logi] # View(x[,!logi])
    ref <- refCol[grep(T,logi)]
    x2 <- t(apply(x2,1,function(x)tapply(x,ref,fun_col)))

    if(length(ref)== 2){
      x2 <- matrix(x2,nrow = 1,byrow = F,dimnames = list(unique(ref),names(x2)))
    }

    merN <- colnames(x2)
    x2 <- cbind(x[,!logi],x2)
    colnames(x2) <- c(refCol[!logi],merN)

  } else {
    LuckyVerbose("Ignore duplicate for col.")
  }

  ## Output data
  LuckyVerbose("All done!")
  return(x2)

}












