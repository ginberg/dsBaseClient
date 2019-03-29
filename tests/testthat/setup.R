#-------------------------------------------------------------------------------
# Copyright (c) 2019 OBiBa. All rights reserved.
## This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#
# Datashield test suite set up
#

library(dsBaseClient)
library(DSLite)
library(testthat)

# check server-side package is installed
if (!requireNamespace("dsBase", quietly = TRUE)) {
  stop("dsBase package is required in the local R installation for the execution of the tests.",
       call. = FALSE)
}

# load simulated test datasets and corresponding login definition
data("CNSIM1")
data("CNSIM2")
data("CNSIM3")
data("logindata.dslite.cnsim")

# new DSLiteServer, hosting the simulated test datasets
dslite.server <- newDSLiteServer(tables=list(CNSIM1=CNSIM1, CNSIM2=CNSIM2, CNSIM3=CNSIM3))
logindata <- logindata.dslite.cnsim
options(datashield.env=environment())

conns <- datashield.login(logins=logindata, assign=TRUE)
