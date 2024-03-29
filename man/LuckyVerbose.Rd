% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/base_LuckyVerbose.R
\name{LuckyVerbose}
\alias{LuckyVerbose}
\title{easy report system in function building}
\usage{
LuckyVerbose(..., levels = 1, type = NULL, show.sys.time = T)
}
\arguments{
\item{...}{one or multiple characters}

\item{levels}{an integer >= 1}

\item{type}{one of "cat" and "message"}
}
\value{
a verbose report
}
\description{
easy report system in function building
}
\examples{
LuckyVerbose("AVC")
LuckyVerbose("AVC",levels = 1)
LuckyVerbose("AVC",levels = 3)
LuckyVerbose("AVC",levels = 3,type="message")
}
\seealso{
\code{\link[base]{cat}};\code{\link[base]{message}}
}
\author{
Weibin Huang<\email{654751191@qq.com}>
}
