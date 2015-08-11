#' 
#' @title Shifts a column up and down by a number of rows
#' @description This function shifts a vector up or down by a number of rows specified
#' @details This function is used when the analyst needs to generate a new column
#' by shifting an existing column up or down by a number of rows. This is similar to a
#' lag function in SAS. A negative integer shifts the column down, a positive number
#' shifts the column up. 
#' @param x a character, the name of the vector to process.
#' @param newobj a character, the name of the new vector in which values have been shifted 
#' If no name is specified the default name is the name of the original vector followed by the suffix 
#' '.shift' e.g. 'LAB_HDL.shift' if the name of the vector is 'LAB_HDL'.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param ShiftLen a positive or negative integer specifying the number of rows to
#' shift up or down by.
#' @return a new vector or table structure with the shifted column is stored on the server side.
#' @author Bishop, T.
#' @export
#' 
#' 
#' @examples {
#' 
#'   # load that contains the login details
#'   data(logindata)
#' 
#'   # login and assign all the stored variables.
#'   opals <- datashield.login(logins=logindata,assign=TRUE)
#' 
#'   # shift variable 'LAB_HDL' down by 1 row
#'   ds.rowShift(x='D$LAB_HDL',shiftLen=-1)
#'   
#'   # shift variable 'LAB_HDL' up by 6 rows
#'   ds.rowShift(x='D$LAB_HDL',shiftLen=6)
#'   
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(opals)
#' 
#' }
#' 

ds.rowShift <- function(x=NULL, datasources=NULL, newobj=NULL, shiftLen = 1L){
  
  # if no opal login details are provided look for 'opal' objects in the environment
  
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
  
  if(is.null(x)){
    stop("Please provide the name of a vector!", call.=FALSE)
  }
  
  # check if the input object(s) is(are) defined in all the studies
  
  inputElts <- extract(x)
  
  if(is.na(inputElts[[1]])){
    defined <- isDefined(datasources, inputElts[[2]])
  }else{
    defined <- isDefined(datasources, inputElts[[1]])
  }
  
  if(is.null(newobj)){
    newobj <- paste0(inputElts[[2]],".shift")
  }
  
  
  
  # call the server side function and do the shift for each server
  
  for(i in 1:length(datasources)){
    
    message(paste0("--Processing ", names(datasources)[i], "..."))
    cally <- paste0("rowShiftDS(", x, paste0(", ",shiftLen), ")")
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