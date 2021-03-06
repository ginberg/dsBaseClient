\name{getPooledVar}
\alias{getPooledVar}
\title{Gets a pooled variance}
\usage{
getPooledVar(dtsources, x)
}
\arguments{
  \item{dtsources}{a list of opal object(s) obtained after
  login in to opal servers; these objects hold also the
  data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{x}{a character, the name of a numeric vector}
}
\value{
a pooled variance
}
\description{
This is an internal function.
}
\details{


This function is called to avoid calling the client
function 'ds.var' which may stop the process due to some
checks not required when computing a mean inside a
function.
}
\keyword{internal}

