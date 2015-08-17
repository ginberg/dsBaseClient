#' 
#' @title Sets the row names for a data frame
#' @description This function Sets the row names for a data frame using a server side
#' object as the input
#' @details This function Sets the row names for a data frame using a server side
#' object as the input. The number of row names must match the number of rows
#' @param x a data frame to which the row names will be applied.
#' @param rownames a vector of names that exists on the server side with the 
#' same number of names as there are rows in x
#' @param newobj a character, the name of the new vector in which missing values have been replaced. 
#' If no name is specified the default name is the name of the original vector followed by the suffix 
#' '.newrows'
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @return a new vector or table structure with the same class is stored on the server site.
#' @author Bishop, T.
#' @export
#' @examples {
#' TBC
#' 
#' }
#' 
ds.setRowNames = function(x=NULL, rownames=NULL, newobj=NULL, datasources=NULL) {
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
  
  if(is.null(x)){
    stop("Please provide the name of a vector!", call.=FALSE)
  }
  
  # check if replacement values have been provided
  if(is.null(rownames)){
    stop("Please provide a list of replacement values!", call.=FALSE)
  }
  # check if the input object(s) is(are) defined in all the studies
  defined_x <- isDefined(datasources, x)
  
  # call the internal function that checks the input object is of the same class in all studies.
  typ_x <- checkClass(datasources, x)
  
  # if the input object is not a matrix or a dataframe stop
  if(typ_x != 'data.frame' & typ_x != 'matrix'){
    stop("The input vector must be of type 'data.frame' or a 'matrix'!", call.=FALSE)
  }  
  # check if the input object(s) is(are) defined in all the studies
  defined_r <- isDefined(datasources, rownames)
  
  # call the internal function that checks the input object is of the same class in all studies.
  typ_r <- checkClass(datasources, rownames)
  
  # if the input object is a matrix or a dataframe stop
  if(typ_r == 'data.frame' & typ_r == 'matrix'){
    stop("The rownames vector must not be of type 'data.frame' or a 'matrix'!", call.=FALSE)
  }
  
  # TO DO - row names must not be duplicated!
  
  for(i in 1:length(datasources)){
    # get the number of rows in the input data frame and the number of row
    # names. If they are not equal, throw an error.
    
    dims_x <- datashield.aggregate(datasources[i], as.symbol(paste0('dim(', x, ')' )))
    print(dims_x)
    r_x <- dims_x[[1]][1]
    
    cally <- paste0("length(", rownames, ")")
    r_r <- datashield.aggregate(datasources[i], as.symbol(cally))
    
    if(r_x != r_r){
      message("The number of row names must equal the number of rows.")
      stop(paste0("This is not the case in ", names(datasources)[i]), call.=FALSE)
    }
  }
  
  if(is.null(newobj)){
    newobj <- paste0(x,".newrow")
  }
  
  # call the server side function and doo the replacement for each server
  for(i in 1:length(datasources)){
    message(paste0("--Processing ", names(datasources)[i], "..."))
    cally <- paste0("setRowNamesDS(", x, ",", rownames, ")")
    print(cally)
    datashield.assign(datasources[i], newobj, as.symbol(cally))
    
    # check that the new object has been created and display a message accordingly
    finalcheck <- isAssigned(datasources[i], newobj) 
    
  }
  
}