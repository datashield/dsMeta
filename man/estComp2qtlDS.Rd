\name{estComp2qtlDS}
\alias{estComp2qtlDS}
\title{Compute mean, sd and size for two quantitaive variables comparison}
\usage{
estComp2qtlDS(formula = NULL, data = NULL)
}
\arguments{
  \item{formuala}{a formula that that specifies the two
  variables}
}
\value{
a numeric vector, the mean, sd and lengths of each of the
two vectors.
}
\description{
helps to computes measures for two-group compaparison with
two quantitative variables.
}
\details{


generates a contingency table and puts the mean, sd and
length values in a vector. The ouput of this function can
be used for example in the calculations of: raw mean
difference, standardized mean difference and other
measures.
}
\author{
Gaye, A.
}

