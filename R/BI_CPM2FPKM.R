#' @title CPM to FPKM
#' @description RNA-Seq CPM to FPKM RNA-Seq Read Count to FPKM-like Gene length-normalized expression matrix
#' @param mt Read count matrix(row is gene, column is sample)
#' @param geneid Only support \code{ensembl} now
#' @param genomic Only support \code{hg38}, \code{hg19}, or \code{hg38_biomart} now
#' @return FPKM-like expression matrix
#' @details
#' FPKM-like expression = Read Count × 10^9 / (Gene Length(bp) × Total Read Count)
#' @author Weibin Huang<\email{hwb2012@@qq.com}>
#' @export
CPM2FPKM <- function(
    mt,
    geneid = "ensembl",
    genomic = c('hg38','hg38_biomart','hg19')[1]
){

  if(genomic=='hg38'){
    # load('./data/common.annot.width_2018-11-26.rda')
    gene.annotations <- common.annot.width;
  } else if(genomic=='hg19'){
    # load('./data/common.annot.width_GRCh37.19_2018-11-29.rda')
    gene.annotations <- common.annot.width_GRCh37.19
  } else if(genomic=='hg38_biomart'){
    gene.annotations <- read.table(system.file("extdata", "Biomart.annotations.hg38.txt", package="luckyBase"), sep="\t", header=TRUE)
  } else {
    stop('CPM2FPKM: Not supported genomic!')
  }

  if(geneid != "ensembl"){
    stop('CPM2FPKM: Please use ENSEMBL Gene ID!')
  }

  coGene <- intersect(rownames(mt), gene.annotations$ENSEMBL)

  mt <- mt[coGene,]
  gene_lengths <- gene.annotations[match(coGene, gene.annotations$ENSEMBL),]$width

  # Calculate FPKM
  mt_fpkm <- apply(mt, 2, function(x)cpm_to_fpkm(x, gene_length))

  return(mt_fpkm)

}

####%%%%%%%%%%%%%%%%%Assistant function%%%%%%%%%%%%%%%%####

cpm_to_fpkm <- function(cpm, gene_length) {
  # 检查输入参数
  if (!is.numeric(cpm) || !is.numeric(gene_length)) {
    stop("CPM和基因长度必须是数值型")
  }
  if (length(cpm) != length(gene_length)) {
    stop("CPM和基因长度的长度必须相同")
  }

  # 计算FPKM
  fpkm <- cpm * 1e3 / gene_length

  return(fpkm)
}





