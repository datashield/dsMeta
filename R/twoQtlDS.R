#'
#' @title Compute mean, sd and size for two quantitaive variables comparison
#' @description helps to computes measures for two-group compaparison with two quantitative variables.
#' @details generates a contingency table and puts the mean, sd and length values in a vector. 
#' The ouput of this function can be used for example in the calculations of: raw mean difference,
#' standardized mean difference and other measures.
#' @param variables a list of two quantitative variables.
#' @apram data the dataframe that holds the variables.
#' @return a numeric vector, the mean, sd and lengths of each of the two vectors.
#' @author Gaye, A.
#' @export
#'
twoQtlDS <- function(variables=NULL,data=NULL){
  
  # this filter sets the minimum number of observations that are allowed 
  nfilter <- 5
  
  # allow for the variable to be accessed individually 
  if(!(is.null(data))){ attach(data) }
  
  # get the mean, sd and length
  output <- c()
  for(i in 1:2){
    c1 <-  mean(variables[[i]], na.rm=TRUE)
    c2 <-  sd(variables[[i]], na.rm=TRUE)
    c3 <-  length(variables[[i]])
    if(c3 < nfilter){
      c1 <- NA
      c2 <- NA
      c3 <- NA
    }
    output <- append(output, c(c1,c2,round(c3)))
  }

  return(output)
}
