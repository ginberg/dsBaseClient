\name{subsetHelper}
\alias{subsetHelper}
\title{Compares subset and original object sizes and eventually carries out subsetting}
\usage{
subsetHelper(dts, data, rs = NULL, cs = NULL)
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{data}{a string character, the name of the dataframe
  or the factor vector and the range of the subset.}

  \item{rs}{a vector of two integers, the indices of the
  rows de extract.}

  \item{cs}{a vector of two integers or one or more
  characters.}
}
\value{
a message or the class of the object if the object has the
same class in all studies.
}
\description{
This is an internal function.
}
\details{


This function is called by the function 'ds.subset' to
ensure that the requested subset is not larger than the
original object.
}
\keyword{internal}

