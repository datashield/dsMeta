#'
#' @title Generates the information required to compute estimates for two-group comparisons
#' @description Generates summaries required for the comparison of two binary or quantitative variables.
#' @details For dichotomous variables:generates a 2x2 table and puts the cell frequencies in a vector. Cell counts
#' smaller than the allowed minimum count are replaced by 0. The ouput of this function
#' can be used for example in the calculations of: log relative risk, log odds ratio and 
#' risk difference.
#' For quantitative variables: generates a contingency table and puts the mean, sd and length values in a vector. 
#' The ouput of this function can be used for example in the calculations of: raw mean difference,
#' standardized mean difference and other measures.
#' @param formula a formula that that specifies the two variables.
#' @param data the dataframe that holds the variables.
#' @param vartype the type of the two variables: 'binary' or 'qtl'.
#' @return a numeric vector, the values in the 2x2 contingency table (for two binaries) or the mean, sd and lengths 
#' of each of the two quantitative vectors.
#' @author Gaye, A.
#' @export
#'
est4comparisonDS <- function(formula=NULL,data=NULL,vartype=NULL){
  
  # this filter sets the minimum number of observations that are allowed 
  nfilter <- setFilterDS()
  
  # allow for the variable to be accessed individually 
  if(!(is.null(data))){ attach(data) }
  
  # get the info depending on the type of the variables
  if(vartype=='binary'){
    # comparison of two binary variables
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
  }else{
    # comparison of two quantitative variables
    output <- c()
    for(i in c(2,3)){
      invar <- eval(terms(formula)[[i]])
      c1 <-  mean(invar, na.rm=TRUE)
      c2 <-  sd(invar, na.rm=TRUE)
      c3 <-  length(invar)
      if(c3 < nfilter){
        c1 <- NA
        c2 <- NA
        c3 <- NA
      }
      output <- append(output, c(c1,c2,round(c3)))
    }
  }
  
  return(output)
  
}
