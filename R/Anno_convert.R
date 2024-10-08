

#' @title multiple annotation based on dataset common.annot.
#' @description multiple annotation based on dataset common.annot.Its colnames include:"ENSEMBL","ENSEMBL.Versions","SYMBOL","GENENAME","ENTREZID","UNIGENE" and "gene_type".
#' @param vt genes you want to annotation
#' @param fromtype the raw type of the genes.
#' @param totype the annotated type of the genes.
#' @param db a dataset containing annotation information. In lucky package, it is often \code{common.annot} dataset for \code{hg38}.
#'
#' @author Weibin Huang<\email{654751191@@qq.com}>
#' @export
convert <- function(vt,
                    fromtype="ENSEMBL",
                    totype="SYMBOL",
                    db=common.annot){
  if(all(NA %in% vt ==T)){
    #提示有空值。此时会标记NA为第一个NA对应的totype值，此为错误标注。
    vt[which(is.na(vt))] <- "NA value"
    t <- as.character(db[,totype]);f <-as.character(db[,fromtype])
    p <- match(vt,f)
    return(t[p])
  } else {
    #无空值。直接输出
    t <- as.character(db[,totype]);f <-as.character(db[,fromtype])
    p <- match(vt,f)
    return(t[p])
  }
}



#' @title multiple annotation based on dataset common.annot.
#' @description multiple annotation based on dataset common.annot.Its colnames include:"ENSEMBL","ENSEMBL.Versions","SYMBOL","GENENAME","ENTREZID","UNIGENE" and "gene_type".
#' @inheritParams convert
#' @export
convert1 <- function(vt,
                    fromtype="SYMBOL",
                    totype="ENSEMBL",
                    db=common.annot){
  if(all(NA %in% vt ==T)){
    #提示有空值。此时会标记NA为第一个NA对应的totype值，此为错误标注。
    vt[which(is.na(vt))] <- "NA value"
    t <- as.character(db[,totype]);f <-as.character(db[,fromtype])
    p <- match(vt,f)
    return(t[p])
  } else {
    #无空值。直接输出
    t <- as.character(db[,totype]);f <-as.character(db[,fromtype])
    p <- match(vt,f)
    return(t[p])
  }
}

#' @title multiple annotation based on dataset common.annot.
#' @description multiple annotation based on dataset common.annot.Its colnames include:"ENSEMBL","ENSEMBL.Versions","SYMBOL","GENENAME","ENTREZID","UNIGENE" and "gene_type".
#' @inheritParams convert
#' @export
convert2 <- function(vt,
                     fromtype="ENTREZID",
                     totype="SYMBOL",
                     db=common.annot){
  if(all(NA %in% vt ==T)){
    #提示有空值。此时会标记NA为第一个NA对应的totype值，此为错误标注。
    vt[which(is.na(vt))] <- "NA value"
    t <- as.character(db[,totype]);f <-as.character(db[,fromtype])
    p <- match(vt,f)
    return(t[p])
  } else {
    #无空值。直接输出
    t <- as.character(db[,totype]);f <-as.character(db[,fromtype])
    p <- match(vt,f)
    return(t[p])
  }
}

#' @title multiple annotation based on dataset common.annot.
#' @description multiple annotation based on dataset common.annot.Its colnames include:"ENSEMBL","ENSEMBL.Versions","SYMBOL","GENENAME","ENTREZID","UNIGENE" and "gene_type".
#' @param data a list of feature data
#' @param fromtype one or more fromtype, like "ENTREZID","SYMBOL", and "ENSEMBL"
#' @param totype one target totype, like "ENTREZID","SYMBOL", and "ENSEMBL"
#' @param goldstandard if annotated with different results, select this type as gold standard
#' @inheritParams convert
#' @details a plus version of \link{convert} for GEO annotation
#' @export
Plus.convert <- function(data,type,
                         fromtype = c("ENTREZID","SYMBOL"),
                         totype = "ENSEMBL",
                         goldstandard = "ENTREZID",
                         db=common.annot){

  ## Test
  if(F){

    f <- fData(eset)
    f2 <- Plus.convert(data = list(DesignEset = f),
                       type = list(DesignEset = c("UNIGENE","GENE")),
                       fromtype = c("UNIGENE","SYMBOL"),
                       totype = "ENTREZID",
                       goldstandard = "SYMBOL",
                       db=common.annot)
    f2 <- f2$DesignEset$metadata[c('ID','ENTREZID')]

  }


  ## 名
  ns <- names(data)

  ## 注释
  l1 <- NULL
  for(i in 1:length(data)){ # i=1
    ## 提取数据
    data.i <- data[[i]]
    type.i <- type[[i]]
    data.i2 <- subset(data.i,select = type.i)
    data.i3 <- subset(data.i,select = setdiff(colnames(data.i),type.i))

    ## 进行注释
    anno.i <- NULL
    for (j in 1:ncol(data.i2)) { #j=1
      anno.j <- as.data.frame(data.i2[,j],stringsAsFactors = F)
      anno.j2 <- convert(as.character(anno.j[,1]),
                         fromtype = fromtype[j],
                         totype = totype,
                         db = db)
      anno.i <- rbind(anno.i,anno.j2)
    }
    anno.i2 <- t(anno.i)
    lg1 <- apply(anno.i2,1,is.all.na);lg1 <- !lg1
    anno.i3 <- anno.i2[lg1,]
    anno.i3 <- as.data.frame(anno.i3)
    colnames(anno.i3) <- paste("from.",fromtype,sep = "")

    ## 提取注释
    # x <- anno.i3[1,]
    get1 <- function(x){
      x1 <- x[!is.na(x)]
      x2 <- unique(x1)
      if(length(x2) > 1){
        #注释不正常
        return("abnormal")
      } else {
        #注释正常
        return(x2)
      }
    }
    anno.i3$id <- apply(anno.i3,1,get1)
    anno.i3 <- cbind(data.i2[lg1,],anno.i3,data.i3[lg1,])
    colnames(anno.i3)[1:ncol(data.i2)] <- colnames(data.i2)
    colnames(anno.i3)[(ncol(anno.i3) - ncol(data.i3) + 1) : ncol(anno.i3)] <- colnames(data.i3) # 2021-11-4: 如果data.i3仅一列，colname会产生错误。此代码可避免此错误
    test1 <- table(anno.i3$id == "abnormal")
    if(T %in% names(test1)){
      LuckyVerbose(paste0(ns[i],": Attention!There are ",test1[names(test1) %in% T]," abnormal annotation."))
      ## 正常注释
      abnormal.anno <- anno.i3[anno.i3$id == "abnormal",]
      LuckyVerbose(paste0("Gold standard id is ",goldstandard,"."))
      s1 <- paste0("from.",goldstandard)
      normal.anno <- ifelse(anno.i3$id == "abnormal",as.character(anno.i3[,s1]),as.character(anno.i3$id))
      anno.i3$id <- normal.anno
      abnormal.anno$id2 <- abnormal.anno[,s1]
    } else {
      print(paste0(ns[i],": All right annotation."))
      normal.anno <- anno.i3$id
      abnormal.anno <- NULL
    }
    colnames(anno.i3)[match('id',colnames(anno.i3))] <- totype

    ## 输出结果
    li <- list(anno = normal.anno,
               abnormal = abnormal.anno,
               metadata = anno.i3)
    l1 <- c(l1,list(li))
    names(l1)[i] <- ns[i]
  }

  ## 输出结果
  return(l1)

}



