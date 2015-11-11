#' @title Calculates the difference between rows for a target column in grouped data
#' @description This function calculates the difference between rows for a target 
#' column in grouped data. For non-grouped data it would be possible to use rowShift
#' @details This function is used when the analyst needs to calculate the difference
#' between rows for a column. The data should be grouped by an identifying column:
#'  for non-grouped data it is possible to use the rowShift function and a subtraction
#'  to perform the same operation 
#' @param input a character, the name of the data frame to process
#' @param id_col a character, the name of the field in the data frame that groups
#'  the data
#' @param target_col a character, the name of the field in the data frame
#' @param newobj a character, the name of the new vector in which values have been shifted 
#' If no name is specified the default name is the name of the original vector followed by the suffix 
#' '.delta' e.g. 'LAB_HDL.delta' if the name of the vector is 'LAB_HDL'.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @return a new vector containing the deltas
#' @author Bishop, T.
#' @export
#' @examples {
#' 
#'#To do
#' 
#' }
#' 

ds.delta <- function(input=NULL, id_col=NULL, target_col=NULL, newobj=NULL, datasources=NULL){
  
  # if no opal login details are provided look for 'opal' objects in the environment
  
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
  
  if(is.null(input)){
    stop("Please provide the name of a data frame!", call.=FALSE)
  }
  
  # check if the input object(s) is(are) defined in all the studies
  defined <- isDefined(datasources, input)
  
  if(is.null(newobj)){
    newobj <- paste0(input,".delta")
  }
  
  # call the server side function and do the calculation for each server
  for(i in 1:length(datasources)){
    message(paste0("--Calculating delta on ", names(datasources)[i], "..."))
    cally <- paste0("deltaDS(", input,",'",id_col ,"','", target_col,"')")
    datashield.assign(datasources[i], newobj, as.symbol(cally))
    
    # check that the new object has been created and display a message accordingly
    finalcheck <- isAssigned(datasources[i], newobj) 
    
  }
}