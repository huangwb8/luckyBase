

#' @title Read Count to FPKM
#' @description RNA-Seq Read Count to FPKM RNA-Seq Read Count to FPKM-like Gene length-normalized expression matrix
#' @param readcount Read count matrix(row is gene, column is sample)
#' @param geneid Only support \code{ensembl} now
#' @param genomic Only support \code{hg38} now
#' @return FPKM-like expression matrix
#' @details
#' FPKM-like expression = Read Count × 10^9 / (Gene Length(bp) × Total Read Count)
#' @author Weibin Huang<\email{hwb2012@@qq.com}>
#' @export
ReadCount2FPKM <- function(
    readcount,
    geneid = "ensembl",
    genomic = c('hg38','hg38_biomart','hg19')
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
    stop('ReadCount2FPKM: Not supported genomic!')
  }

  if(geneid != "ensembl"){
    stop('ReadCount2FPKM: Please use ENSEMBL Gene ID!')
  }

  coGene <- intersect(rownames(readcount), gene.annotations$ENSEMBL)

  readcount <- readcount[coGene,]
  gene_lengths <- gene.annotations[match(coGene, gene.annotations$Gene_ID),]$width
  total_counts <- colSums(readcount)

  # Calculate FPKM
  readcount_fpkm <- calculate_fpkm(readcount, gene_lengths, total_counts)

  return(readcount_fpkm)

}


####%%%%%%%%%%%%%%%%%Assistant function%%%%%%%%%%%%%%%%####

calculate_fpkm <- function(read_counts, gene_lengths, total_mapped_reads) {
  # 确保行和列的长度匹配
  if (nrow(read_counts) != length(gene_lengths)) {
    stop("The number of genes in read_counts and gene_lengths must match.")
  }

  # 创建一个FPKM矩阵
  fpkm_matrix <- matrix(0, nrow = nrow(read_counts), ncol = ncol(read_counts))
  rownames(fpkm_matrix) <- rownames(read_counts)
  colnames(fpkm_matrix) <- colnames(read_counts)

  # 计算每个样本的FPKM值
  for (i in 1:ncol(read_counts)) {
    fpkm_matrix[, i] <- (read_counts[, i] * 1e9) / (gene_lengths * total_mapped_reads[i])
  }

  return(fpkm_matrix)
}
