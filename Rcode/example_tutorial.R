rm(list=ls())
library(tidyverse)

FullData <- read.csv("./data/qs.csv", stringsAsFactors = FALSE)
# Load packages:
#library("dplyr")
#library("tidyr")
# Frequency at baseline:
Data <- FullData %>%
  filter(EPOCH == "BASELINE",
         grepl("^PSSR\\d+A$",QSTESTCD)) %>%
  select(USUBJID,QSTEST,QSORRES) %>%
  spread(QSTEST, QSORRES) %>%
  select(-USUBJID) %>%
  mutate_each(funs(replace(.,.=="NOT ANSWERED",NA))) %>%
  mutate_each(funs(ordered(.,c("NOT AT ALL","ONCE A WEEK",
                               "2-4 TIMES PER WEEK/HALF THE TIME",
                               "5 OR MORE TIMES PER WEEK/ALMOST ALWAYS"))))
names(Data) <- seq_len(ncol(Data))
head(Data)

# paqueteria de ellos
library("bootnet")

Network <- estimateNetwork(Data,
                            default = "EBICglasso")
