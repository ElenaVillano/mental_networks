# Some minor graphics for data exploration
# By Elena Villalobos, Enero 2022

# Clean workspace
rm(list=ls())

# Packages
library(tidyverse)

# Setting working directory
setwd("~/Documents/mental_networks")

# Data
datos_raw_riesgo <- read_csv('data/BDCOVIDNivelRiesgo.csv')
head(datos_raw_riesgo)
summary(datos_raw_riesgo)
dim(datos_raw_riesgo)
options(tibble.print_max = 109)
datos_raw_riesgo[10]

# Some colors
colores <- c('#38A6A5','#99C935','#EDAD08','#ee4d5b','royalblue','aquamarine3','turquoise3')

# perfil
datos_raw_riesgo %>% 
  count(Cuál_es_su_perfil) 

#
datos_raw_riesgo %>% 
  count(Sexo) %>% 
  ggplot(aes(x=Sexo, y=n)) +
  geom_bar(stat='identity') 

hist(datos_raw_riesgo$Edad_años_cumplidos)


datos_raw_riesgo %>% 
  count(Escolaridad) %>% 
  ggplot(aes(x=Escolaridad, y=n)) +
  geom_bar(stat='identity') 

datos_raw_riesgo %>% 
  count(País) %>% 
  ggplot(aes(x=País, y=n)) +
  geom_bar(stat='identity') 

datos_raw_riesgo %>% 
  count(Entidad_federativa) %>% 
  ggplot(aes(x=Entidad_federativa, y=n)) +
  geom_bar(stat='identity') 

datos_raw_riesgo %>% 
  count(Está_diagnosticadao_con_diabetes,
        Está_diagnosticadao_con_obesidad)

datos_raw_riesgo %>% 
  count(Está_diagnosticadao_con_diabetes)

datos_raw_riesgo %>% 
  count(Está_diagnosticadao_con_hipertensión)

datos_raw_riesgo %>% 
  count(Está_diagnosticadao_con_obesidad)

datos_raw_riesgo %>% 
  count(Está_diagnosticadao_con_depresión)

datos_raw_riesgo %>% 
  count(Está_diagnosticadao_con_esquizofrenia)

datos_raw_riesgo %>% 
  count(Está_diagnosticadao_con_alzheimer)

datos_raw_riesgo %>% 
  count(Está_diagnosticadao_con_cáncer)

datos_raw_riesgo %>% 
  count(Está_diagnosticadao_con_hipertiroidismo)

datos_raw_riesgo %>% 
  count(Está_diagnosticadao_con_hipotiroidismo)


### Estrés ###
layout(matrix(c(1:8),ncol=2))
hist(datos_raw_riesgo$r1)
hist(datos_raw_riesgo$r2)
hist(datos_raw_riesgo$r3)
hist(datos_raw_riesgo$r4)
hist(datos_raw_riesgo$r8)
hist(datos_raw_riesgo$r12)
hist(datos_raw_riesgo$r17)


### Evitacion ###
layout(matrix(c(1:3),ncol=3))
hist(datos_raw_riesgo$r5)
hist(datos_raw_riesgo$r6)
hist(datos_raw_riesgo$r7)
# la r5 y la r6 tienen un comportamiento muy 
# similar

### Distanciamiento ###
layout(matrix(c(1:8),ncol=2))
hist(datos_raw_riesgo$r9)
hist(datos_raw_riesgo$r10)
hist(datos_raw_riesgo$r11)
hist(datos_raw_riesgo$r12)
hist(datos_raw_riesgo$r13)
hist(datos_raw_riesgo$r14)
hist(datos_raw_riesgo$r15)
hist(datos_raw_riesgo$r16)

### Ansiedad generalizada/Triteza ###
layout(matrix(c(1:8),ncol=2))
hist(datos_raw_riesgo$r9)
hist(datos_raw_riesgo$r10)
hist(datos_raw_riesgo$r11)
hist(datos_raw_riesgo$r12)
hist(datos_raw_riesgo$r13)
hist(datos_raw_riesgo$r14)
hist(datos_raw_riesgo$r15)
hist(datos_raw_riesgo$r16)


### Violencia ###
layout(1)
layout(matrix(c(1:8),ncol=2))
par(mar=c(2,3,2,2))
barplot(prop.table(table(datos_raw_riesgo$r57)))
barplot(prop.table(table(datos_raw_riesgo$r58)))
barplot(prop.table(table(datos_raw_riesgo$r59)))
barplot(prop.table(table(datos_raw_riesgo$r60)))
barplot(prop.table(table(datos_raw_riesgo$r61)))
barplot(prop.table(table(datos_raw_riesgo$r62)))
barplot(prop.table(table(datos_raw_riesgo$r63)))
barplot(prop.table(table(datos_raw_riesgo$r64)))
