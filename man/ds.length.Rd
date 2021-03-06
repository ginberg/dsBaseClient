\name{ds.length}
\alias{ds.length}
\title{Gets the length of a vector or list}
\usage{
ds.length(x = NULL, type = "combine", datasources = NULL)
}
\arguments{
  \item{x}{a string character, the name of a vector}

  \item{type}{a character which represents the type of
  analysis to carry out. If \code{type} is set to
  'combine', a global variance is calculated if \code{type}
  is set to 'split', the variance is calculated separately
  for each study.}

  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}
}
\value{
a numeric, the number of elements of the input vector or
list.
}
\description{
This function is similar to R function \code{length}.
}
\details{


The function returns the pooled length or the length of the
a vector or a list for each study.
}
\examples{
{

  # load that contains the login details
  data(logindata)

  # login and assign all the variables stored on the server side
  opals <- datashield.login(logins=logindata,assign=TRUE)

  # Example 1: Get the total number of observations across all the studies for the variable 'LAB_TSC' - default behaviour
  ds.length(x='D$LAB_TSC')

  # Example 2: Get the number of observations on each study, for the variable 'LAB_TSC'
  ds.length(x='D$LAB_TSC', type='split')

  # clear the Datashield R sessions and logout
  datashield.logout(opals)

}
}
\author{
Gaye, A.
}

