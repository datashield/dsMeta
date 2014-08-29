\name{est4comparisonDS}
\alias{est4comparisonDS}
\title{Generates the information required to compute estimates for two-group comparisons}
\usage{
est4comparisonDS(formula = NULL, data = NULL, vartype = NULL)
}
\arguments{
  \item{formula}{a formula that that specifies the two
  variables.}

  \item{data}{the dataframe that holds the variables.}

  \item{vartype}{the type of the two variables: 'binary' or
  'qtl'.}
}
\value{
a numeric vector, the values in the 2x2 contingency table
(for two binaries) or the mean, sd and lengths of each of
the two quantitative vectors.
}
\description{
Generates summaries required for the comparison of two
binary or quantitative variables.
}
\details{


For dichotomous variables:generates a 2x2 table and puts
the cell frequencies in a vector. Cell counts smaller than
the allowed minimum count are replaced by 0. The ouput of
this function can be used for example in the calculations
of: log relative risk, log odds ratio and risk difference.
For quantitative variables: generates a contingency table
and puts the mean, sd and length values in a vector. The
ouput of this function can be used for example in the
calculations of: raw mean difference, standardized mean
difference and other measures.
}
\author{
Gaye, A.
}

