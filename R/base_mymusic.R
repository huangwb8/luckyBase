

#' @title play music via tuneR
#' @description play music via tuneR
#' @param n the order of available music paths
#' @param path the path of music files ducument
#' @return a string of music file path
#' @author Weibin Huang<\email{654751191@@qq.com}>
#' @importFrom tuneR play
#' @examples
#' mymusic(1)
#' @export
mymusic <- function(n=1, path=NULL){
  if(is.null(path)){
    path <- "E:/RCloud/music"
  }
  music.list <- list.files(path,pattern = "Rend_",full.names = T)
  if(length(music.list)==0){
    # 在给定的地址中未找到音频文件。
    library(tuneR)
    w <- system.file("extdata", "wind 00_00_04-00_00_28.m4a", package = "lucky")
    tuneR::play(w)
  } else {
    # 可以找到文件
    music <- music.list[n];
    tuneR::play(music)
  }
}






