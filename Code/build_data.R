
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
market <- fread(paste0(raw_path, "Origin_and_Destination_Survey_DB1BMarket_2023_1.csv"))
coupon <- fread(paste0(raw_path, "Origin_and_Destination_Survey_DB1BCoupon_2023_1.csv"))

# Quick inspection
glimpse(market)
glimpse(coupon)

# Check dimensions and missing values
dim(market)
colSums(is.na(market))