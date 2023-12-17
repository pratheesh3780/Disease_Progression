library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(epifitter)
library(knitr)

rmdfiles <- c("README.rmd")
sapply(rmdfiles, knit, quiet = T)

##single level maximum potential
source("dir/mpsle_ui.R")
source("dir/mpsle_server.R")

##single level limited potential
source("dir/lpsle_ui.R")
source("dir/lpsle_server.R")

##single factor experiment limited potential
source("dir/lpsfe_ui.R")
source("dir/lpsfe_server.R")

##two factor experiment limited potential
source("dir/lptfe_ui.R")
source("dir/lptfe_server.R")