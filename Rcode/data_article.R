# Data test for tutorial
# It doesn't work the same as in the article
# but here is the data converted to numbers

# Clean working directory
rm(list=ls())

# Load packages:
library("dplyr")
library("tidyr")
library("tidyverse")
library("bootnet")
library("qgraph")

# Setting Working directory
setwd("~/Documents/mental_networks")

## Original code for database:::
#FullData <- read.csv("data/qs.csv", stringsAsFactors = FALSE)
## Frequency at baseline:
#Data <- FullData %>%
#  filter(EPOCH == "BASELINE",
#         grepl("^PSSR\\d+A$",QSTESTCD)) %>%
#  select(USUBJID,QSTEST,QSORRES) %>%
#  spread(QSTEST, QSORRES) %>%
#  select(-USUBJID) %>%
#  mutate_each(funs(replace(.,.=="NOT ANSWERED",NA))) %>%
#  mutate_each(funs(ordered(.,c("NOT AT ALL","ONCE A WEEK",
#                               "2-4 TIMES PER WEEK/HALF THE TIME",
#                               "5 OR MORE TIMES PER WEEK/ALMOST ALWAYS"))))
#names(Data) <- seq_len(ncol(Data))
#colnames(Data) <- paste("qu", seq(1:17), sep="")
#head(Data)
#summary(Data)
#dim(Data)
#Data
#Data <- Data %>% drop_na()
#Data_num <- mutate_all(Data, function(x) as.numeric(x))
#write_csv(Data_num, "data/questions_data_tutorial.csv", col_names = TRUE)

# Clean data

Data_num <- read.csv("data/questions_data_tutorial.csv")

Network3 <- estimateNetwork(Data_num, default = "EBICglasso",
                            threshold=TRUE)

plot(Network3, layout="spring", labels=TRUE)

# Centrality indices:
library("qgraph")
centralityPlot(Network3, include = c("Betweenness", "Closeness", "Strength"))


### a. intervalos de confianza para los pesos de las aristas. 

boot13 <- bootnet(Network3, nBoots = 100)

summary(boot13)

plot(boot13, labels= FALSE, order = 'sample')

### b. bootstrap de subconjuntos para medidas de centralidad

boot23 <- bootnet(Network3, nBoots = 100, type = 'case')

plot(boot23, include = c("Betweenness", "Closeness", "Strength"))

corStability(boot23)


### c. Test for significant differences

differenceTest(boot13, 3, 17,'strength')

plot(boot13, 'edge', plot = "difference", onlyNonZero = TRUE, order='sample')

plot(boot13, "strength")


