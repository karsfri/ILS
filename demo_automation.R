
source("https://github.com/karsfri/ILS/raw/master/theme_hms.R")
theme_set_ils()

library(DBI)
library(tidyverse)
library(sf)
library(seasonal)
library(tsibble)
library(lubridate)
library(readxl)
# SQL tenging
con <- dbConnect(odbc::odbc(), "HgrDwh_Prod", timeout = 10, encoding = "WINDOWS-1252")