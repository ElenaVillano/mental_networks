rm(list=ls())

library(tidyverse)

datos_raw <- read_csv('./data/raw_answers.csv')

preguntas <- tibble(seq(1,109),
                    names(datos_raw))

#write_csv(preguntas, './data/preguntas.csv')
head(datos_raw)

summary(datos_raw)
