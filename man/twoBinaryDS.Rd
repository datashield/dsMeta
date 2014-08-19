\name{twoBinaryDS}
\alias{twoBinaryDS}
\title{Compute cells frequencies for two dichotomous variables comparison}
\usage{
twoBinaryDS(formula = NULL, data = NULL)
}
\arguments{
  \item{formula}{a formula that that indicates the outcome
  and explanatory variable}

  \item{data}{the dataframe that holds the variables.}
}
\value{
a numeric vector, the values 2x2 contingency table.
}
\description{
helps to computes measures for two-group compaparison with
two binary variables.
}
\details{


generates a 2x2 table and puts the cell frequencies in a
vector. Cell counts smaller than the allowed minimum count
are replaced by 0. The ouput of this function can be used
for example in the calculations of: log relative risk, log
odds ratio and risk difference.
}
\author{
Gaye, A.
}

