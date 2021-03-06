\name{ds.table2D}
\alias{ds.table2D}
\title{Generates 2-dimensional contingency tables}
\usage{
ds.table2D(x = NULL, y = NULL, type = "combine", warningMessage = TRUE,
  datasources = NULL)
}
\arguments{
  \item{x}{a character, the name of a numerical vector with
  discrete values - usually a factor.}

  \item{y}{a character, the name of a numerical vector with
  discrete values - usually a factor.}

  \item{type}{a character which represent the type of table
  to ouput: pooled table or one table for each data source.
  If \code{type} is set to 'combine', a pooled
  2-dimensional table is returned; if If \code{type} is set
  to 'split' a 2-dimensional table is returned for each
  data source.}

  \item{warningMessage}{a boolean, if set to TRUE (deafult)
  a warning is displayed if any returned table is invalid.
  Warning messages are suppressed if this parameter is set
  to FALSE. However the analyst can still view 'validity'
  information which are stored in the output object
  'validity' - see the list of output objects.}

  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}
}
\value{
A list object containing the following items:
\item{counts}{ table(s) that hold counts for each
level/category. If some cells counts are invalid (see
'Details' section) only the total (outer) cell counts are
displayed in the returned individual study tables or in the
pooled table.} \item{rowPercent}{ table(s) that hold row
percentages for each level/category. Inner cells are
reported as missing if one or more cells are 'invalid'.}
\item{colPercent}{ table(s) that hold column percentages
for each level/category. Inner cells are reported as
missing if one or more cells are 'invalid'.}
\item{chi2Test}{Chi-squared test for homogeneity.}
\item{validity}{ a text that informs the analyst about the
validity of the output tables. If any tables are invalid
the studies they are originated from are also mentioned in
the text message.}
}
\description{
The function ds.table2D is a client-side wrapper function.
It calls the server-side function 'table2dDS' to generate
2-dimensional tables for all data sources.
}
\details{


The table returned by the server side function might be
valid (non disclosive - no table cell have counts between 1
and the minimal number agreed by the data owner and set in
opal) or invalid (potentially disclosive - one or more
table cells have a count between 1 and the minimal number
agreed by the data owner). If a 2-dimensional table is
invalid all the cells are set to NA except the total count.
This way it is possible the know the total count and
combine total counts across data sources but it is not
possible to identify the cell(s) that had the small counts
which render the table invalid.
}
\examples{
{

  # load the file that contains the login details
  data(logindata)

  # login and assign all the variables to R
  opals  <-  datashield.login(logins=logindata,assign=TRUE)

  # Example 1: generate a two dimensional table, outputting combined contingency tables - default behaviour
  output <- ds.table2D(x='D$DIS_DIAB', y='D$GENDER')
  # display the 5 results items, one at a time to avoid having too much information displayed at the same time
  output$counts
  output$rowPercent
  output$colPercent
  output$chi2Test
  output$validity

  # Example 2: generate a two dimensional table, outputting study specific contingency tables
  ds.table2D(x='D$DIS_DIAB', y='D$GENDER', type='split')
  # display the 5 results items, one at a time to avoid having too much information displayed at the same time
  output$counts
  output$rowPercent
  output$colPercent
  output$chi2Test
  output$validity

  # Example 3: generate a two dimensional table, outputting combined contingency tables
  # *** this example shows what happens when one or studies return an invalis table ***
  output <- ds.table2D(x='D$DIS_CVA', y='D$GENDER')
  output$counts
  output$rowPercent
  output$colPercent
  output$chi2Test
  output$validity

  # Example 4: same example as above but output is given for each study, separately (i.e. type='split')
  # *** this example shows what happens when one or studies return an invalis table ***
  output <- ds.table2D(x='D$DIS_CVA', y='D$GENDER', type='split')
  output$counts
  output$rowPercent
  output$colPercent
  output$chi2Test
  output$validity

  # clear the Datashield R sessions and logout
  datashield.logout(opals)

}
}
\author{
Gaye, A.; Burton, P.
}
\seealso{
\link{ds.table1D} for the tabulating one vector.
}

