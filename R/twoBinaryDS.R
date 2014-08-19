#'
#' @title Compute cells frequencies for two dichotomous variables comparison
#' @description helps to computes measures for two-group compaparison with two binary variables.
#' @details generates a 2x2 table and puts the cell frequencies in a vector. Cell counts
#' smaller than the allowed minimum count are replaced by 0. The ouput of this function
#' can be used for example in the calculations of: log relative risk, log odds ratio and 
#' risk difference.
#' @param formula a formula that that indicates the outcome and explanatory variable
#' @param data the dataframe that holds the variables.
#' @return a numeric vector, the values 2x2 contingency table.
#' @author Gaye, A.
#' @export
#'
twoBinaryDS <- function(formula=NULL,data=NULL){
  
  # this filter sets the minimum number of observations that are allowed 
  nfilter <- 5
  
  # allow for the variable to be accessed individually 
  if(!(is.null(data))){ attach(data) }
  mat <- table(eval(terms(formula)[[3]]), eval(terms(formula)[[2]]))
  
  # loop through the count and turn to 0 those < to the minimum cell count allowed
  output <- c()
  for(i in 1:2){
    for(j in 1:2){
      if(mat[j,i] < nfilter) {
        output <- append(output, 0)
      }else{
        output <- append(output, mat[j,i])
      }
    }
  }	  
  return(output)
}
