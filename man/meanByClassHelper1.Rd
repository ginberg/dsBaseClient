\name{meanByClassHelper1}
\alias{meanByClassHelper1}
\title{Generates subset tables}
\usage{
meanByClassHelper1(dtsource, tables, variable, categories)
}
\arguments{
  \item{dtsource}{an opal object(s) obtained after login in
  to opal servers;}

  \item{tables}{a character vector, the tables to
  breakdown}

  \item{variable}{a character, the variable to subset on}

  \item{categories}{a character vector, the classes in the
  variables to subset on}
}
\value{
a character the names of the new subset tables.
}
\description{
This is an internal function.
}
\details{


This function is called by the function 'ds.meanByClass' to
break down the initial table by the specified categorical
variables.
}
\author{
Gaye, A.
}
\keyword{internal}

