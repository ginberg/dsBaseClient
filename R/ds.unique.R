#' 
#' @title Returns unique rows for a data frame based on defined columns
#' @description This function creates a new data frame with rows that are unique
#' on a given set of coumns
#' @details This function is used to subset a data frame on rows that are unique for
#' a chosen set of columns. For the columns that are not part of the grouping set
#' the first value is returned
#' @param x a data frame to process.
#' @param f the set of columns as a character vector that will be used to select
#' the unique rows 
#' @param newobj a character, the name of the new data frame which contains the unique
#' rows. All columns are returned, not just those that are part of the grouping set
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @return a new vector or table structure with the same class is stored on the server site.
#' @author Gaye, A.
#' @export
#' @examples {
#' 
#' TBC
#' }
#' 
ds.unique <- function(x=NULL, f=NULL, datasources=NULL, newobj=NULL){
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
  
  if(is.null(x)){
    stop("Please provide the name of a vector!", call.=FALSE)
  }
  
  if(is.null(f)){
    stop("Please provide a vector of values to filter on!", call.=FALSE)
  }
  
  # check if the input object(s) is(are) defined in all the studies
  inputElts <- extract(x)
  if(is.na(inputElts[[1]])){
    defined <- isDefined(datasources, inputElts[[2]])
  }else{
    defined <- isDefined(datasources, inputElts[[1]])
  }
  
  if(is.null(newobj)){
    newobj <- paste0(inputElts[[2]],".unique")
  }
  
  # call the server side function and do the unique operation for each server
  for(i in 1:length(datasources)){
    message(paste0("--Processing ", names(datasources)[i], "..."))
    cally <- paste0("uniqueDS(",x,",c('",paste(f,collapse="','"),"'))")
    datashield.assign(datasources[i], newobj, as.symbol(cally))
    
    # check that the new object has been created and display a message accordingly
    finalcheck <- isAssigned(datasources[i], newobj) 
    
    # if the input vector is within a table structure append the new vector to that table
    if(!(is.na(inputElts[[1]]))){
      cally <-  paste0("cbind(", inputElts[[1]], ",", newobj, ")")
      datashield.assign(datasources[i], inputElts[[1]], as.symbol(cally)) 
    }
  }
}