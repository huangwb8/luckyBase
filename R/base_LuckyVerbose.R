


#' @title easy report system in function building
#' @description easy report system in function building
#' @param levels an integer >= 1
#' @param type one of "cat" and "message"
#' @param ... one or multiple characters
#' @return a verbose report
#' @seealso \code{\link[base]{cat}};\code{\link[base]{message}}
#' @author Weibin Huang<\email{654751191@@qq.com}>
#' @examples
#' LuckyVerbose("AVC")
#' LuckyVerbose("AVC",levels = 1)
#' LuckyVerbose("AVC",levels = 3)
#' LuckyVerbose("AVC",levels = 3,type="message")
#' @export
LuckyVerbose <- function(...,levels = 1,type = NULL, show.sys.time = T){
  ## Verbose type
  if(is.null(type)){
    if(levels == 1){
      type <-  "message"
    } else {
      type <-  "cat"
    }
  }

  ## level symbol
  if(levels > 1){
    ls <- paste(rep("o",(levels-1)),collapse = "")
  } else {
    ls <- ""
  }

  ## System time
  if(show.sys.time){
    ls <- paste0(as.character(Sys.time()),' | ', ls)
  }

  ## do Verbose
  if(type == "message"){
    return(base::message(ls,...))
  } else if(type == "cat") {
    return(base::cat(ls,...,"\n"))
  } else {
    print("Please input right type.")
  }

}


# Legacy function: 2023-07-29
LuckyVerbose_legacy <- function(...,levels = 1,type = NULL){
  ## Verbose type
  if(is.null(type)){
    if(levels == 1){
      type <-  "message"
    } else {
      type <-  "cat"
    }
  }

  ## level symbol
  if(levels > 1){
    s1 <- paste(rep(" ",(levels-1)),collapse = "")
    s2 <- paste(rep("o",(levels-1)),collapse = "")
    ls <- paste(s1,s2,collapse = "")
  } else {
    ls <- ""
  }

  ## do Verbose
  if(type == "message"){
    return(base::message(ls," ",...))
  } else {
    if(type == "cat"){ return(base::cat(ls,...,"\n")) } else {
      print("Input right type.")
    }
  }

}
