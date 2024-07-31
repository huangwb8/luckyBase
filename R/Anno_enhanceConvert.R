#' @name enhanceConvert
#' @title enhanceConvert
#' @description enhance luckyBase::convert with extra annotation
#' @param enhanceList A list with Symbol-EnsemblID pairs
#' @param verbose Whether to report running process
#' @inheritParams convert
#' @author Weibin Huang<\email{hwb2012@@qq.com}>
#' @seealso \code{\link{convert}}
#' @examples
#' enhanceList = list(
#'   'FAM101B' = 'ENSG00000183688',
#'   'APITD1' = 'ENSG00000175279',
#'   'C17orf70' = 'ENSG00000185504',
#'   'SHFM1' = 'ENSG00000127922',
#'   'MCEMP1' = 'ENSG00000183019',
#'   'FGF7P3' = 'ENSG00000204837',
#'   'LOC100506718' = 'ENSG00000185070',
#'   'MAGI2-AS3' = 'ENSG00000234456',
#'   'LINC01279' = 'ENSG00000091986',
#'   'ZNF667-AS1' = 'ENSG00000166770',
#'   'LOC100507334' = 'ENSG00000261760',
#'   'ZNF542P' = 'ENSG00000240225',
#'   'C10orf54' = 'ENSG00000107738',
#'   "SEPTIN4" = "ENSG00000108387",
#'   "LINC02381" = "ENSG00000250742",
#'   "AC005747.1"  = "ENSG00000007237",
#'   "AC016747.1" = "ENST00000420918",
#'   "CALHM5" = "ENSG00000178033",
#'   "CAVIN1" = "ENSG00000177469",
#'   "CCN4" = "ENSG00000104415",
#'   "DIPK2B" = "ENSG00000147113",
#'   "GSDME" = "ENSG00000105928",
#'   "GUCY1A1" = "ENSG00000164116",
#'   "GUCY1B1" = "ENSG00000061918",
#'   "JCAD" = "ENSG00000165757",
#'   "LHFPL6" = "ENSG00000183722",
#'   "MAGI2-AS3" = "ENSG00000234456",
#'   "MSC-AS1" = "ENSG00000235531",
#'   "NIBAN1" = "ENSG00000135842",
#'   "PNMA8B" = "ENSG00000204851",
#'   "RTL5" = "ENSG00000242732",
#'   "SPART" = "ENSG00000133104",
#'   "TENM4" = "ENSG00000149256",
#'   "ZNF667-AS1" = "ENSG00000166770"
#' )
#' @export
enhanceConvert <- function(vt,
                           fromtype = "SYMBOL",
                           totype = "ENSEMBL",
                           db = common.annot,
                           enhanceList = NULL,
                           verbose = T){

  # Test
  if(F){
    vt <- l$`Pan-F-TBRS`
    fromtype="SYMBOL"
    totype="ENSEMBL"
    db=common.annot
    enhanceList <- list(
      'FAM101B' = 'ENSG00000183688',
      'APITD1' = 'ENSG00000175279',
      'C17orf70' = 'ENSG00000185504',
      'SHFM1' = 'ENSG00000127922',
      'MCEMP1' = 'ENSG00000183019',
      'FGF7P3' = 'ENSG00000204837',
      'LOC100506718' = 'ENSG00000185070',
      'MAGI2-AS3' = 'ENSG00000234456',
      'LINC01279' = 'ENSG00000091986',
      'ZNF667-AS1' = 'ENSG00000166770',
      'LOC100507334' = 'ENSG00000261760',
      'ZNF542P' = 'ENSG00000240225',
      'C10orf54' = 'ENSG00000107738'
    )
  }

  if(is.null(enhanceList)){
    # Use default enhanceList
    if(verbose) LuckyVerbose('enhanceConvert: Use default enhanceList...')
    enhanceList <- readRDS(system.file("extdata", "enhanceList.rds", package = "luckyBase"))
  }

  # Enhanced annotation
  vt2 <- convert(vt,fromtype,totype,db)
  if(sum(is.na(vt2)) > 0){
    idx.annot <- match(vt[is.na(vt2)],names(enhanceList))
    idx.raw <- match(vt[is.na(vt2)],vt)
    if(verbose) LuckyVerbose('enhanceConvert: ',paste0(vt[is.na(vt2)],collapse = '; '))
    vt2[idx.raw] <- as.character(enhanceList[idx.annot])
    vt2[vt2 %in% "NULL"] <- NA
  }

  # Output
  return(vt2)

}

