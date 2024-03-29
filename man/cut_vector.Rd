% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/base_cut_vector.R
\name{cut_vector}
\alias{cut_vector}
\title{cut a vector into lots of parts}
\usage{
cut_vector(vt, nsplit = 100)
}
\arguments{
\item{vt}{a vector}

\item{nsplit}{the number of parts you want to split}
}
\value{
a list contain lots of vectors
}
\description{
cut a vector into lots of parts
}
\examples{
l1 <- cut.vector(1:54800,nsplit=98)
View(l1)
}
\author{
Weibin Huang<\email{654751191@qq.com}>
}
