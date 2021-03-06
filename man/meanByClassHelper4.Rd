\name{meanByClassHelper4}
\alias{meanByClassHelper4}
\title{Gets the subset tables out of the list (i.e. unlist)}
\usage{
meanByClassHelper4(dtsource, alist, initialtable, variable = NA,
  categories = NA)
}
\arguments{
  \item{dtsource}{a list of opal object(s) obtained after
  login in to opal servers; these objects hold also the
  data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{alist}{the name of the list that holds the final
  subset tables}

  \item{initialtable}{a character the name of the table
  that the subset were generated from}

  \item{variable}{a character, the variable to subset on}

  \item{categories}{a character vector, the classes in the
  variables to subset on}
}
\value{
the 'loose' subset tables are stored on the server side
}
\description{
This is an internal function.
}
\details{


This function is called by the function 'ds.meanByClass' to
obtain 'loose' subset tables because the 'subsetByClass'
function does not handle a table within a list.
}
\author{
Gaye, A.
}
\keyword{internal}

