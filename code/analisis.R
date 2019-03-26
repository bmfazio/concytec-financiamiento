library(tidyverse)
library(here)

datos <- read_csv(here("out", "tabla_final.csv"))
datos$gano <- !is.na(datos$presupuesto)

table(datos$ano)

# Analisis de permutacion
sub <- datos %>% filter(ano == "2017")

nsim <- 10**5
difs <- numeric(nsim)
sapply(difs,
       function(x){
         sub %>%
           filter(gano) %>%
           mutate(genero = sample(genero)) %>%
           group_by(genero) %>%
           summarise(presupuesto = mean(presupuesto)) %>%
           pull(presupuesto) %>%
           diff %>% abs
       }) -> aer

sub %>% filter(gano) %>%
  group_by(genero) %>% summarise(presupuesto = mean(presupuesto)) %>%
  pull(presupuesto) %>% diff %>% abs -> real

prop.table(table(aer < real))

sub <- datos %>% filter(ano != "2016-I")

nsim <- 10**5
difs <- numeric(nsim)
sapply(difs,
       function(x){
         sub %>%
           filter(gano) %>%
           mutate(genero = sample(genero)) %>%
           group_by(genero) %>%
           summarise(presupuesto = mean(presupuesto)) %>%
           pull(presupuesto) %>%
           diff %>% abs
       }) -> aer1

sub %>% filter(gano) %>%
  group_by(genero) %>% summarise(presupuesto = mean(presupuesto)) %>%
  pull(presupuesto) %>% diff %>% abs -> real1

prop.table(table(aer1 < real1))

sub <- datos %>% filter(ano != "2016-II")

nsim <- 10**5
difs <- numeric(nsim)
sapply(difs,
       function(x){
         sub %>%
           filter(gano) %>%
           mutate(genero = sample(genero)) %>%
           group_by(genero) %>%
           summarise(presupuesto = mean(presupuesto)) %>%
           pull(presupuesto) %>%
           diff %>% abs
       }) -> aer2

sub %>% filter(gano) %>%
  group_by(genero) %>% summarise(presupuesto = mean(presupuesto)) %>%
  pull(presupuesto) %>% diff %>% abs -> real2

prop.table(table(aer2 < real2))


# Descriptivos
sub <- datos %>% filter(ano == "2017")

glm(gano ~ genero + doctorado, data = sub)

table(sub$genero, sub$gano)

sub %>% filter(gano) %>% group_by(genero) %>% summarise(presupuesto = mean(presupuesto))

sub %>% filter(gano) %>%
  ggplot(aes(x = presupuesto, y = 1, color = genero)) +
  geom_point()

###
sub <- datos %>% filter(ano != "2017")

glm(gano ~ genero + doctorado, data = sub)

table(sub$genero, sub$gano)

sub %>% filter(gano) %>% group_by(genero) %>% summarise(presupuesto = mean(presupuesto))

sub %>% filter(gano) %>%
  ggplot(aes(x = presupuesto, y = 1, color = genero)) +
  geom_point()