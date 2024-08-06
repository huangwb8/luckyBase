

#' @title FPKM to TPM
#' @description RNA-Seq: FPKM to TPM
#' @param mt FPKM matrix (row is gene, column is sample)
#' @param verbose Whether to report the process
#' @return TPM matrix
#' @author Weibin Huang<\email{hwb2012@@qq.com}>
#' @export
FPKM2TPM <- function(mt, verbose=T){

  x1 <- Sys.time()
  mt <- as.matrix(mt)

  ## 转换为数值型向量
  if(verbose) LuckyVerbose("It may take a little long time if the matrix is huge. Be patient.")
  dimname <- list(rownames=rownames(mt),colnames=colnames(mt))
  mt1 <- matrix(as.numeric(as.character(mt)),nrow = nrow(mt),dimnames = dimname)

  ## fpkm->tpm
  colsum <- colSums(mt1)
  mt2 <- t(mt1)
  mt3 <- (10^6)*(mt2/colsum)
  mt4 <- t(mt3)

  ## time difference
  x2 <- Sys.time()
  time <- as.numeric(as.character(x2-x1))
  time <- round(time,digits = 3)
  if(verbose) LuckyVerbose(paste0("All doned! The process takes ",time," seconds.",collapse = ""))

  ## 输出结果

  return(mt4)

}


# test

# data(fpkm) # fpkm <- assay(data)
# tpm <- FPKMtoTPM(fpkm)
# tpm[1,1] == (fpkm[1,1]/sum(as.numeric(as.character(fpkm[,1]))))*(10^6) ##T，说明计算是正确的。
# colSums(tpm)/nrow(tpm) #this is a constant value.
# STAD.TPM <- tpm
# save(STAD.TPM,file = "STAD.TPM.rda")
# dim(STAD.TPM)
