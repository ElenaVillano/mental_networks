############## Análisis con los datos del tamizaje ###############

# limpiamos escritorio
rm(list=ls())

# paqueterías necesarias
library("tidyverse")
library("psychTools") 
library("qgraph")
library("bootnet")

# fija tu directorio de trabajo en una carpeta específica
setwd("~/Documents/mental_networks")

# Base de datos
datos_raw_riesgo <- read_csv('data/BDCOVIDNivelRiesgo.csv')

# preguntas de sólo la psicopatologías
respuestas <- datos_raw_riesgo[,42:74]

# renombramos para facilidad de interpretacion
names(respuestas) <- c('st1','st2','st3','st4','av5',
                       'av6','av7','st8','di9','di10',
                       'di11','st12','di13','di14','di15',
                       'di16','st17','aG18','aG19','aG20',
                       'aG21','aG22','aS23','aS24','aS25',
                       'so26','so27','so28','so29','so30',
                       'de31','de32','de33')

# 1 conjunto de preguntas
pcl5_set <- respuestas[,1:17]
dean_set <- respuestas[,18:33]
todo_set <- respuestas[,1:33]



# Estimate network:
pcl5_net <- estimateNetwork(pcl5_set, 
                            default = "EBICglasso",
                            threshold=TRUE)
net_pcl5 <- pcl5_net$graph

dean_net <- estimateNetwork(dean_set, 
                            default = "EBICglasso",
                            threshold=TRUE)
net_dean <- dean_net$graph

todo_net <- estimateNetwork(todo_set, 
                            default = "EBICglasso",
                            threshold=TRUE)
net_todo <- todo_net$graph
########## Network
# Estimated network:
qgraph(net_pcl5, 
       layout='spring',filename='Rcode/nets/net_pcl5', filetype = "pdf", 
       height = 8, width = 8)

qgraph(net_dean, 
       layout='spring',filename='Rcode/nets/net_dean', filetype = "pdf", 
       height = 8, width = 8)

qgraph(net_todo, 
       layout='spring',filename='Rcode/nets/net_todo', filetype = "pdf", 
       height = 8, width = 8)

# Medidas de centralidad para la red

pdf(file='Rcode/nets/centrality_nets.pdf', width = 8, height = 8, onefile = T)
centralityPlot(pcl5_net, include = c("Betweenness", "Closeness", "Strength"),
               orderBy = 'Strength')
centralityPlot(dean_net, include = c("Betweenness", "Closeness", "Strength"),
               orderBy = 'Strength')
centralityPlot(todo_net, include = c("Betweenness", "Closeness", "Strength"),
               orderBy = 'Strength')
dev.off()


########## A. intervalos de confianza para los pesos de las aristas
# Bootstrap 1000 values
boot_IC_ar <- bootnet(Network1, nBoots = 1000, nCores = 1)

summary(boot_IC_ar)

# Plot bootstrapped edge CIs:
plot(boot_IC_ar, labels = FALSE, order = "sample")

######### B. Bootstrap de subconjuntos para ver la estabilidad de las medidas de centralidad

boot_cen_estab <- bootnet(Network1, nBoots = 1000, type = 'case')

plot(boot_cen_estab, include = c("Betweenness", "Closeness", "Strength"))

corStability(boot_cen_estab)

######### C. Test for significan differences

differenceTest(boot_IC_ar, 3, 17,'strength')

plot(boot_IC_ar, 'edge', plot = "difference", onlyNonZero = TRUE, order='sample')

plot(boot_IC_ar, "strength")

