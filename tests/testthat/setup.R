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

if (!requireNamespace("dsBase", quietly = TRUE)) {
  stop("dsBase package is required in the local R installation for the execution of the tests.",
       call. = FALSE)
}

data("CNSIM1")
data("CNSIM2")
data("CNSIM3")
testServ <- newDSLiteServer(tables=list(CNSIM1=CNSIM1, CNSIM2=CNSIM2, CNSIM3=CNSIM3))
options(datashield.env=environment())

server <- c("sim1", "sim2", "sim3")
driver <- rep("DSLiteDriver", 3)
url <- rep("testServ", 3)
user <- rep("", 3)
password <- rep("", 3)
table <- c("CNSIM1", "CNSIM2", "CNSIM3")
logindata <- data.frame(server, driver, url, user, password, table)

conns <- datashield.login(logins=logindata,assign=TRUE)
