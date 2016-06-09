#' A correctedMarkers Function
#'
#' This function allows you to perform several multiple regression on metabolomic data.
#' @param dat Data matrix.
#' @param fac Index of the group of samples.
#' @param con Index of correction variable.
#' @param comp Index (range) of measured compounds.
#' @keywords regression
#' @keywords metabolomics
#' @export
#' @examples
#' correctedMarkers(iris, 5, 1, 2:4)
correctedMarkers <- function(dat, fac, con, comp){
  # Builds multiple regression for the specified range of data.
  #
  # Args:
  #   dat: Data matrix.
  #   fac: The column index for the predictor variable.
  #   con: The column index for the correction variable.
  #   comp: The range of indexes for the columns of responses.
  #
  # Returns:
  #   The matrix with p-values of having zero estimates for coefficient.
  
  
  # Error handling
    # TODO
  
  
  # Init output
  res <- c()
  
  # Loop through compounds
  for (i in comp){
    
    # Fit multiple regression
    fm <- lm( dat[,i] ~ dat[,fac]*dat[,con])
    
    # Extract p-values for predictors
    pvs <- summary(fm)$coefficients[,4]
    
    # Add to the resulting matrix
    res <- rbind(res, pvs[2:4])
    
  } 
  
  # Name the outcome
  colnames(res) <- c("Group", "Confounder", "Group*Confounder")
  rownames(res) <- colnames(dat)[comp]

  # Return result
  res
  
}

