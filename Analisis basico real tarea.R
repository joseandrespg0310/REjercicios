################################################
#####ANALISIS BASICO CON BASE DE DATOS REAL#####
################################################

###############
###LIBRERIAS###
###############
library(readxl)
library(haven)
library(psych) 
library(dplyr)
library(tidyverse)
library(expss)
library(ggplot2)

###########################
###DIRECTORIO DE TRABAJO###
###########################
setwd("C:/SEGUNDO SEMESTRE/Taller de Estadística/PRIMER PARCIAL/RSTUDIO")


########################
###BASE DATOS DEL INE###
########################
#Abro mi base de datos real obtenida del INE
bd <- read_excel("EXPORTACIONES 2023p.xlsx")

###################
###VISUALIZACION###
###################
#Cantidad de variables
ncol(bd)

#Cantidad de observaciones 
nrow(bd)

#Veamos de manera general que tengo 
ls(bd)

#Tipos de variables
str(bd)

#Podemos observar los primeros datos
head(bd)

#Veamos si tenemos valores nulos
sapply(bd, function(x) sum(is.na(x)))

#Cambiemos un nombre de variable
names(bd)[names(bd) == 'DESADU'] <- 'desaduanizacion'

#Cambiemos al original y veamos la siguiente opcion
names(bd)[names(bd) == 'desaduanizacion'] <- 'DESADU'

#Intentaremos cambiar todo de golpe a minusculas
names(bd) <- tolower(names(bd))

############
###LABELS###
############
#Ahora incluiremos labels para nuestras variables
bd <- apply_labels(bd,
                   adudes = "Codigo lugar desaduanizacion",
                   desadu = "Descripcion del lugar de desaduanizacion")

#Hagamos una tabla de frecuencias
table(bd$desadu)

#Hagamos un grafico de barras
barplot(table(bd$desadu))

#Cambiemos los valores para que si lea
barplot(table(bd$desadu),
        las = 2)

################
###EJERCICIOS###
################
#1. Encuentren el valor maximo de exportaciones
BDmax <- bd %>%
  summarize(across(c(mes, nandina),
                   ~ max(.x, na.rm = TRUE),
                   .names = "{col}")) %>%
  mutate(Statistic = "Maximum")

#2. Encuentren el valor promedio de de exportaciones
bd$nandina<- as.numeric(bd$nandina)
mean(bd$nandina)

#3. Hagan un grafico de barras para via de despacho
table(bd$desvia)
barplot(table(bd$desvia))
barplot(table(bd$desvia),
        las = 2))

#####################
###SERIE DE TIEMPO###
#####################
#Abramos la base
btc <- read_excel("Bitcoin.xlsx")

#Cambiemos el nombre para que sea algo más simple
names(btc)[names(btc) == "Exchange Date"] <- "date"

#El primer paso, y el mas complicado es convertir la fecha a fecha
btc$Date <- as.Date(btc$date, format = "%Y-%b-%d")

#Veamos si funciono
class(btc$Date)
class(btc$Open)
#Grafiquemos 
ggplot(btc,
       aes(x = Date,
           y = Open)) +
  geom_point(col = 'blue',
             size = 0.5)

#Recordemos que los valores tienen un sentido
ggplot(btc,
       aes(x = Date,
           y = Open)) +
  geom_line(col = 'blue',
             size = 0.5)

#Los valores nulos nos estan generando problema
#Usaremos la metodologia mas basica de imputaciones
btc %>%
  fill(Open, .direction = c("up"))

#Guardemos estos valores en una nueva columna
btc <- btc %>%
  mutate(Open_f = Open) %>%
  fill(Open_f, .direction = c("up"))

#Veamos si esta nueva variable esta completa
ggplot(btc,
       aes(x = Date,
           y = Open_f)) +
  geom_line(col = 'blue',
            size = 0.5)


#Podemos poner dos lineas en el mismo grafico
ggplot(btc,
       aes(x = Date)) +
  geom_line(aes(y = Low),
            col = 'green',
            size = 0.5,
            alpha = 0.7) + 
  geom_line(aes(y = High),
            col = 'red',
            size = 0.5,
            alpha = 0.7)

################
###EJERCICIOS###
################
#1. Corrijan el problema de NA en Low y High creando nuevas variables llamadas Low_f y High_f
btc %>%
  fill(Low, .direction = c("up"))
btc <- btc %>%
  mutate(Low_f = Open) %>%
  fill(Low_f, .direction = c("up"))

btc %>%
  fill(High, .direction = c("up"))
btc <- btc %>%
  mutate(High_f = Open) %>%
  fill(High_f, .direction = c("up"))

#2. Vuelvan a realizar el grafico de linea para Low_f y High_f
ggplot(btc,
       aes(x = Date)) +
  geom_line(aes(y = Low),
            col = 'red',
            size = 0.5,
            alpha = 0.7) + 
  geom_line(aes(y = High),
            col = 'blue',
            size = 0.5,
            alpha = 0.7)

#3. Encuentren el promedio entre High y Low y grafiquenlo junto a High y Low
btc <- btc %>%
  mutate(mean = (High + Low) / 2)

ggplot(btc, aes(x = Date)) +
  geom_line(aes(y = Low, col = "magenta"),
            size = 0.6,
            alpha = 0.7) +
  geom_line(aes(y = High, 
                col = "green"),
                size = 0.6,
                alpha = 0.7) +
  geom_line(aes(y = mean, col = "blue")) +
  labs(title = "Gráfico de High, Low y Promedio",
       x = "Date",
       y = "Open")

