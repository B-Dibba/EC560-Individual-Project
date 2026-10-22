
# ── Run Logging 
log_file <- paste0("output/log_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
sink(log_file, split = TRUE)  # saves console output to log file
cat("Run started:", format(Sys.time()), "\n")
cat("R version:", R.version$version.string, "\n\n")


#Set working directory:
setwd("C:/Users/dibba/OneDrive/Desktop/University of Oregon/2nd Year/Theory of IO/EC560-Individual-Project")

#Packages 
# install.packages(c("tidyverse", "data.table"))
library(tidyverse)
library(data.table)

# File paths
raw_path  <- "data/raw/"
proc_path <- "data/processed/"

#Load & Inspect the Raw Data
market <- fread(paste0(raw_path, "Origin_and_Destination_Survey_DB1BMarket_2025_1.csv"))
coupon <- fread(paste0(raw_path, "Origin_and_Destination_Survey_DB1BCoupon_2025_1.csv"))
ticket <- fread(paste0(raw_path, "Origin_and_Destination_Survey_DB1BTicket_2025_1.csv"))

# Quick inspection
glimpse(market)
glimpse(coupon)
glimpse(ticket)

# Check dimensions and missing values
dim(market)
colSums(is.na(market))




# ── Close Log 
cat("\nRun completed:", format(Sys.time()), "\n")
sink()  # stops logging


