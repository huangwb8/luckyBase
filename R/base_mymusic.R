

#' @title play music via tuneR
#' @description play music via tuneR
#' @param n the order of available music paths
#' @param path the path of music files ducument
#' @return a string of music file path
#' @author Weibin Huang<\email{654751191@@qq.com}>
#' @import tuneR
#' @examples
#' mymusic(1)
#' @export
mymusic <- function(n=1, path=NULL){
  if(is.null(path)){
    # path <- "E:/RCloud/music"
    path <- system.file("extdata", package = "luckyBase")
  }
  music.list <- list.files(path, pattern = "Rend_",full.names = T)
  music <- music.list[n]
  tuneR::play(music)
}






