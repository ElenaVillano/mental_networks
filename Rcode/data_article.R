rm(list=ls())
library(tidyverse)
library(bootnet)
library("qgraph")

setwd("~/Documents/mental_networks")
FullData <- read.csv("data/qs.csv", stringsAsFactors = FALSE)

# Load packages:
library("dplyr")
library("tidyr")
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

colnames(Data) <- paste("qu", seq(1:17), sep="")
head(Data)
summary(Data)
dim(Data)

Data <- Data %>% drop_na()

levels(Data$qu1) <- c(1:4)
levels(Data$qu2) <- c(1:4)
levels(Data$qu3) <- c(1:4)
levels(Data$qu4) <- c(1:4)
levels(Data$qu5) <- c(1:4)
levels(Data$qu6) <- c(1:4)
levels(Data$qu7) <- c(1:4)
levels(Data$qu8) <- c(1:4)
levels(Data$qu9) <- c(1:4)
levels(Data$qu10) <- c(1:4)
levels(Data$qu11) <- c(1:4)
levels(Data$qu12) <- c(1:4)
levels(Data$qu13) <- c(1:4)
levels(Data$qu14) <- c(1:4)
levels(Data$qu15) <- c(1:4)
levels(Data$qu16) <- c(1:4)
levels(Data$qu17) <- c(1:4)

tail(is.na(Data),100)

any(is.na(Data))

data_cor <- cor_auto(Data)

Data_num <- mutate_all(Data, function(x) as.numeric(as.character(x)))

#Network <- qgraph::EBICglasso(qgraph::cor_auto(Data), n=1000, threshold=TRUE)

Network3 <- estimateNetwork(Data_num, default = "EBICglasso",
                            threshold=TRUE)

plot(Network3, layout="spring", labels=TRUE)

# Centrality indices:
library("qgraph")
centralityPlot(Network3, include = c("Betweenness", "Closeness", "Strength"))


### a. intervalos de confianza para los pesos de las aristas. 

boot13 <- bootnet(Network3, nBoots = 1000)

summary(boot13)

plot(boot13, labels= FALSE, order = 'sample')

### b. bootstrap de subconjuntos para medidas de centralidad

boot23 <- bootnet(Network3, nBoots = 1000, type = 'case')

plot(boot23, include = c("Betweenness", "Closeness", "Strength"))

corStability(boot23)


### c. Test for significant differences

differenceTest(boot13, 3, 17,'strength')

plot(boot13, 'edge', plot = "difference", onlyNonZero = TRUE, order='sample')

plot(boot13, "strength")


