\name{ds.asMatrix}
\alias{ds.asMatrix}
\title{Attempts to turn its argument into a matrix}
\usage{
ds.asMatrix(x = NULL, newobj = NULL, datasources = NULL)
}
\arguments{
  \item{x}{a character, the name of the an object to conver
  to a matrix.}

  \item{newobj}{the name of the new vector.If this argument
  is set to \code{NULL}, the name of the new variable is
  the name of the input variable with the suffixe
  '_matrix'}

  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources. By default an internal function looks for
  'opal' objects in the environment and sets this
  parameter.}
}
\value{
nothing is returned to the client, the new object is stored
on the server side.
}
\description{
This function is similar to R function \code{as.matrix}.
}
\details{


Unlike the R function 'as.matrix' the output matrix is
generated only if the input matrix is valid (i.e. has a
number of rows greater than the minimal number of
observations allowed).
}
\examples{
{

  # load that contains the login details
  data(logindata)

  # login and assign all the stored variable(s)
  # (by default the assigned dataset is a dataframe named 'D')
  opals <- datashield.login(logins=logindata,assign=TRUE)

  # turn the data frame 'D' (default name of the dataframe assigned above) into a matrix
  ds.asMatrix(x='D')

  # clear the Datashield R sessions and logout
  datashield.logout(opals)

}
}
\author{
Gaye, A.; Isaeva, J.
}
\seealso{
\link{ds.asNumeric} to turn a variable into a numeric type.

\link{ds.asFactor} to turn a variable into a factor type.

\link{ds.asCharacter} to turn a variable into a character
type.

\link{ds.asList} to construct an object of type list.
}

