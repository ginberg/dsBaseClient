#' 
#' @title Generates the summary of an object
#' @description Provides some insight about an object. Unlike the similar R function
#' only a limited class of objects can be used as input to reduce the risk of disclosure.
#' @details The class and size of the object are returned and various other information are 
#' also returned depending of the class of the object. Potentially disclosive information
#' such as the minimum and maximum values of numeric vectors are not returned. The summary 
#' is given for each study separately.
#' @param x a numeric or factor variable
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login.
#' 
#' @return the returned information depends on the class of the objects.
#' @author Gaye, A.
#' @export
#' @examples \dontrun{
#' 
#'   # load the login data
#'   data(logindata)
#' 
#'   # login and assign all the variable held in the data repository
#'   conns <- datashield.login(logins=logindata,assign=TRUE)
#' 
#'   # Example 1: suummary of a numerical variable
#'   ds.summary(x='D$LAB_TSC')
#' 
#'   # Example 1: suummary of a binary variable
#'   ds.summary(x='D$GENDER')
#' 
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(conns)
#' 
#' }
#' 
ds.summary <- function(x=NULL, datasources=NULL){
  
  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }
  
  if(is.null(x)){
    stop("Please provide the name of the input vector!", call.=FALSE)
  }
  
  # the input variable might be given as column table (i.e. D$x)
  # or just as a vector not attached to a table (i.e. x)
  # we have to make sure the function deals with each case
  xnames <- extract(x)
  varname <- xnames$elements
  obj2lookfor <- xnames$holders
  
  # check if the input object(s) is(are) defined in all the studies
  if(is.na(obj2lookfor)){
    defined <- isDefined(datasources, varname)
  }else{
    defined <- isDefined(datasources, obj2lookfor)
  }
  
  # call the internal function that checks if the input object is of the same class in all studies.
  typ <- checkClass(datasources, x)
  
  # the input object must be a numeric or an integer vector
  # the input object must be a dataframe or a factor
  if(typ != 'data.frame' & typ != 'character' & typ != 'factor' & typ != 'integer' & typ != 'list' & typ != 'logical' & typ != 'matrix' & typ != 'numeric'){
    stop("The input object must be a 'data.frame', 'character', factor', 'integer', 'list', 'logical', 'matrix' or 'numeric'.", call.=FALSE)
  }
  
  stdnames <- names(datasources)
  numsources <- length(datasources)
  finalOutput <- list()
                                    
  # now get the summary depending on the type of the input variable
  if (typ == "data.frame" | typ == "matrix") {
    validity <- datashield.aggregate(datasources, as.symbol(paste0('isValidDS(', x, ')')))
    dims <- datashield.aggregate(datasources[validity == TRUE], as.symbol(paste0('dim(', x, ')')))
    cols <- datashield.aggregate(datasources[validity == TRUE], as.symbol(paste0('colnames(', x, ')')))
    for (i in 1:length(datasources[validity == TRUE])) {
      r <- dims[[i]][1]
      c <- dims[[i]][2]
      finalOutput[[i]] <- list('class'=typ, 'number of rows'=r, 'number of columns'=c, 'variables held'=cols[[i]])
    }
    names(finalOutput) <- names(datasources[validity == TRUE])
    for (d in datasources[validity == FALSE]) finalOutput[[d@name]] <- 'INVALID object!'
  }
  
  if (typ == "character") {
    validity <- datashield.aggregate(datasources[i], as.symbol(paste0('isValidDS(', x, ')')))
    l <- datashield.aggregate(datasources[validity == TRUE], as.symbol(paste0('length(', x, ')' )))
    for (i in 1:length(datasources[validity == TRUE])) {
      finalOutput[[i]] <- list('class'=typ, 'length'=l[[i]])
    }
    names(finalOutput) <- names(datasources[validity == TRUE])
    for (d in datasources[validity == FALSE]) finalOutput[[d@name]] <- 'INVALID object!'
  }
  
  if (typ == "factor") {
    validity <- datashield.aggregate(datasources, as.symbol(paste0('isValidDS(', x, ')')))
    l <- datashield.aggregate(datasources[validity == TRUE], as.symbol(paste0('length(', x, ')' )))
    categories <- datashield.aggregate(datasources[validity == TRUE], as.symbol(paste0('levels(', x, ')' )))
    freq <- datashield.aggregate(datasources[validity == TRUE], as.symbol(paste0('table1dDS(', x, ')' )))
    for (i in 1:length(datasources[validity == TRUE])) {
      stdsummary <- list('class'=typ, 'length'=l[[i]], 'categories'=categories[[i]])
      for(j in 1:length(categories[[i]])){
        f <- freq[[i]]
        stdsummary[[3+j]] <- f[1][[1]][1,j]
      }
      names(stdsummary)[4:(3+length(categories[[i]]))] <- paste0("count of '", categories[[i]], "'")
      finalOutput[[i]] <- stdsummary
    }
    names(finalOutput) <- names(datasources[validity == TRUE])
    for (d in datasources[validity == FALSE]) finalOutput[[d@name]] <- 'INVALID object!'
  }
  
  if(typ == "integer" | typ == "numeric"){
    validity <- datashield.aggregate(datasources, as.symbol(paste0('isValidDS(', x, ')')))
    l <- datashield.aggregate(datasources[validity == TRUE], as.symbol(paste0('length(', x, ')' )))
    q <- (datashield.aggregate(datasources[validity == TRUE], as.symbol(paste0('quantileMeanDS(', x, ')' ))))
    for (i in 1:length(datasources[validity == TRUE])) {
      finalOutput[[i]] <- list('class'=typ, 'length'=l[[i]], 'quantiles & mean'=q[[i]])
    }
    names(finalOutput) <- names(datasources[validity == TRUE])
    for (d in datasources[validity == FALSE]) finalOutput[[d@name]] <- 'INVALID object!'
  }
  
  if (typ == "list") {
    l <- datashield.aggregate(datasources, as.symbol(paste0('length(', x, ')' )))
    elts <- datashield.aggregate(datasources, as.symbol(paste0('namesDS(', x, ')')))
    for (i in 1:numsources) {
      if (is.null(elts[[i]])) {
        stdsummary <- list('class'=typ, 'length'=l[[i]])
      } else {
        stdsummary <- list('class'=typ, 'length'=l[[i]], 'elements held in the list'=elts[[i]])
      }
      finalOutput[[i]] <- stdsummary
    }
    names(finalOutput) <- stdnames
  }
  
  if (typ == "logical") {
    validity <- datashield.aggregate(datasources, as.symbol(paste0('isValidDS(', x, ')')))
    l <- datashield.aggregate(datasources[validity == TRUE], as.symbol(paste0('length(', x, ')')))
    freq <- datashield.aggregate(datasources[validity == TRUE], as.symbol(paste0('table1dDS(', x, ')')))
    for (i in 1:length(datasources[validity == TRUE])) {
      stdsummary <- list('class'=typ, 'length'=l[[i]])
      for (j in 1:length(2)) {
        f <- freq[[i]]
        stdsummary[[2+j]] <- freq[1][[1]][1,j]
      }
      names(stdsummary)[3:(2+2)] <- paste0("count of '", c('FALSE','TRUE'), "'")
      finalOutput[[i]] <- stdsummary
    }
    names(finalOutput) <- names(datasources[validity == TRUE])
    for (d in datasources[validity == FALSE]) finalOutput[[d@name]] <- 'INVALID object!'
  }
  
  return(finalOutput)
}