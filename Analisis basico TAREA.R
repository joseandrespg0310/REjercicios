#####################################
#####ANALISIS BASICO CON EJEMPLO#####
#####################################

###############
###LIBRERIAS###
###############
library(readxl)
library(haven)
library(psych)
library(dplyr)
library(tidyverse)

###########################
###DIRECTORIO DE TRABAJO###
###########################
setwd("C:/SEGUNDO SEMESTRE/Taller de Estadística/PRIMER PARCIAL/RSTUDIO")


##########################
###ABRIR BASES DE DATOS###
##########################
#Como mencionamos existen diferentes formatos, intentemos abrir los mas conocidos
#Excel
BD1 <- read_excel("deforestacion.xlsx")
#Veamos que tenemos
View(BD1)

#Stata
BD2 <- read_dta("Personas_Vivienda_2021.dta")
#Veamos que tiene esta
View(BD2)

#CSV
BD3 <- read.csv("winequality-red.csv",
                sep = ";")
#Veamos que tiene
View(BD3)

###################
###VISUALIZACION###
###################
#Ya vimos como visualizar de manera general, ahora veamos otras formas
#Podemos tener un resumen general 
ls(BD1)

#Podemos ver los tipos de variables
str(BD1)

#Podemos tener una descripcion general
describe(BD1)

#Podemos observar los primeros datos
head(BD1)

#Y los ultimos
tail(BD1)

################
###EJERCICIOS###
################
#1. Hagan el mismo analisis que hicimos para la base de datos de vinos

###################
###OBSERVACIONES###
###################
#Podemos ver la cantidad de variables 
nrow(BD1)

#Podemos combinarlo para que nos mencione esto junto a un texto
cat("Tenemos", nrow(BD1), "observaciones")

#Analicemos el nivel de observacion
unique(BD1['year'])
n_distinct(BD1['year'])

################
###EJERCICIOS###
################
#1. Analicen la encuesta de hogares con la variable 'folio'


###############
###VARIABLES###
###############
#Observamos cuantas variables tenemos
ncol(BD1)

#Recordemos como podemos ver los nombres de las variables
ls(BD1)

#Recordemos lo que decíamos para acceder a una variable
#Forma con $
BD1$deforestation_value

#Forma con ['']
BD1['deforestation_value']

###############
###EJERCICIO###
###############
#1. Accedan a la variable alcohol de BD3 con $
BD3$alcohol

#2. Accedan a la variable quality de BD3 con ['']
BD3['quality']

###################
###MAS VARIABLES###
###################

#Revisemos el tipo de variables que tenemos
typeof(BD1$year)

#Revisemos su clase
class(BD1$year)

#Transformemos una variable numerica a texto
as.character(BD1$year)

#Para transformar realmente tengo que guardar el resultado
BD1$year <- as.character(BD1$year)

#Veamos que paso
str(BD1)

#Nos hemos equivocado al hacer la variable y quiero que la misma sea numerica
BD1$year <- as.numeric(BD1$year)

################
###EJERCICIOS###
################
#1. Cambien el tipo de variable de quality de la base de vinos a texto
as.character(BD3$quality)
BD3$quality <- as.character(BD3$quality)
typeof(BD3$quality)

###################
###VALORES NULOS###
###################
#Veamos un ejemplo real de valores nulos
head(BD2['s01a_07_2'])

#Analicemos una sola variable
sum(is.na(BD2['s01a_07_2']))

#Podemos hacer un resumen de los valores nulos
sapply(BD2, function(x) sum(is.na(x)))

#Veamos nuestra base de deforestacion
sapply(BD1, function(x) sum(is.na(x)))

###############
###EJERCICIO### 
###############
#1. Analicen si la base de datos de vino tiene valores nulos
sapply(BD3, function(x) sum(is.na(x)))


###########
###MEDIA###
###########
#La estimacion es bastante simple y directa
mean(BD1$deforestation_value)

#Podemos guardar estos objetos para otros usos
defor_bolivia <- mean(BD1$deforestation_value)

#Pero podemos tener problemas si intentamos estimar con valores nulos
mean(BD2$ylab)

#Tenemos que ajustar el comando para que funcione bien
mean(BD2$ylab, na.rm = TRUE)

#Y recordemos que solo hay media para numeros
mean(BD1$year)
mean(as.character(BD1$year))
################
###EJERCICIOS###
################
#1. Encuentren la edad promedio de los jefes de hogar en Bolivia
mean(BD2$s01a_03)

#2. Encuentren el porcentaje de alcohol promedio de los vinos rojos
mean(BD3$alcohol)

#############
###MEDIANA###
#############
#La estimacion es bastante simple y directa
median(BD1$deforestation_value)

#Podemos guardar estos objetos para otros usos
defor_bolivia <- median(BD1$deforestation_value)

#Pero podemos tener problemas si intentamos estimar con valores nulos
median(BD2$ylab)

#Tenemos que ajustar el comando para que funcione bien
median(BD2$ylab, na.rm = TRUE)

#Y recordemos que solo hay media para numeros
median(BD1$year)

################
###EJERCICIOS###
################
#1. Encuentren la edad mediana de los jefes de hogar en Bolivia
median(BD2$s01a_03)

#2. Encuentren el porcentaje de alcohol mediano de los vinos rojos
median(BD3$alcohol)

###############
###CUARTILES###
###############
#Veamos el valor del Q1 del salario
Q1 <- quantile(BD2$ylab, 0.25, na.rm = TRUE)
Q1

#Veamos el valor del Q3 del salario
Q3 <- quantile(BD2$ylab, 0.75, na.rm = TRUE)
Q3

################
###EJERCICIOS###
################
#1. Encuentren el Q1 de deforestacion en Bolivia
Q1 <- quantile(BD1$deforestation_value, 0.25, na.rm = TRUE)
Q1

#2. Encuentren el Q3 de la edad de los jefes de hogar en Bolivia
Q3 <- quantile(BD2$s01a_03, 0.75, na.rm = TRUE)
Q3

#########################
###DESVIACION ESTANDAR###
#########################
#Observemos como se obtiene
sqrt(var(BD1$deforestation_value))

#Al igual que antes lo puedo guardar
sd_defor_bolivia <- sqrt(var(BD1$deforestation_value))

#Y al igual que con la media tenemos problema si hay valores nulos
sqrt(var(BD2$ylab))

#Y la solucion es la misma
sqrt(var(BD2$ylab, na.rm = TRUE))

################
###EJERCICIOS###
################
#1. Encuentren la desviacion estandar de emisiones generadas por deforestacion en Bolivia
#2. Encuentren la desviacion estandar de edad de los jefes de hogar en Bolivia
sqrt(var(BD2$s01a_03))
#3. Encuentren la desviacion estandar del porcentaje de alcohol promedio de los vinos rojos
sqrt(var(BD3$alcohol))

################
###HISTOGRAMA###
################
#Realizar un histograma es bastante simple
hist(BD1$deforestation_value)

#Veamos que ocurre cuando tenemos valores nulos
hist(BD2$ylab)

################
###EJERCICIOS###
################
#1. Realicen un histograma de la edad de los jefes de hogar en Bolivia
hist(BD2$s01a_03)

#2. Realicen un histograma del porcentaje de alcohol promedio de los vinos rojos
hist(BD3$alcohol)

#############
###BOXPLOT###
#############
#Hagamos un boxplot de la deforestacion
boxplot(BD1$deforestation_value/1000,
        main = "Boxplot de deforestación",
        ylab = "Miles de HA",
        col = "lightblue")

# Identificar valores atípicos
outliers <- boxplot.stats(BD1$deforestation_value / 1000)$out

# Etiquetar valores atípicos con los años
for (i in seq_along(outliers)) {
  # Encontrar el índice del valor atípico en los datos originales
  idx <- which(BD1$deforestation_value / 1000 == outliers[i])
  text(x = 1.1, y = outliers[i],
       labels = BD1$year[idx],
       pos = 4,
       col = "black",
       cex = 0.5,
       alpha = 0.5)
}

################
###EJERCICIOS###
################
#1. Realicen un boxplot de la edad de los jefes de hogar en Bolivia
boxplot(BD2$s01a_03/1000,
        main = "Boxplot de edad de jefes h",
        ylab = "Miles de HA",
        col = "pink")
#2. Realicen un boxplot del porcentaje de alcohol promedio de los vinos rojos
boxplot(BD3$alcohol/1000,
        main = "Boxplot de deforestación",
        ylab = "Miles de HA",
        col = "yellow")

##########################
###NUESTRA PROPIA TABLA###
##########################
#Hemos mencionado que queremos hacer nuestra propia tabla
#Eso requiere un poco mas de trabajo
#Pero como se dice, todo en la vida cuesta!
BD2mean <- BD2 %>%
  summarize(across(c(ylab, s01a_03),
                   ~ mean(.x, na.rm = TRUE),
                   .names = "{col}")) %>%
  mutate(Statistic = "Mean")

BD2var <- BD2 %>%
  summarize(across(c(ylab, s01a_03),
                   ~ var(.x, na.rm = TRUE),
                   .names = "{col}")) %>%
  mutate(Statistic = "Variance")

BD2max <- BD2 %>%
  summarize(across(c(ylab, s01a_03),
                   ~ max(.x, na.rm = TRUE),
                   .names = "{col}")) %>%
  mutate(Statistic = "Maximum")

BD2min <- BD2 %>%
  summarize(across(c(ylab, s01a_03),
                   ~ min(.x, na.rm = TRUE),
                   .names = "{col}")) %>%
  mutate(Statistic = "Minimum")

# Combinar los resultados
BD2summary <- bind_rows(BD2mean, BD2var, BD2max, BD2min)
BD2summary$ylab <- as.numeric(format(BD2summary$ylab,
                                     scientific = FALSE))
BD2summary$ylab <- round(BD2summary$ylab,
                         digits = 2)
View(BD2summary)

#Tambien tenemos otras opciones como ser
summary(BD2[, c("ylab", "s01a_03")])

###############
###EJERCICIO###
###############
#1. Hagan una tabla resumen de variables que ustedes deseen en la base de vinos
#Obten la media, var, max y min
BD3mean <- BD3 %>%
  summarize(across(c(alcohol, density),
                   ~ mean(.x, na.rm = TRUE),
                   .names = "{col}")) %>%
  mutate(Statistic = "Mean")

BD3var <- BD3 %>%
  summarize(across(c(alcohol, densidy),
                   ~ var(.x, na.rm = TRUE),
                   .names = "{col}")) %>%
  mutate(Statistic = "Variance")

BD3max <- BD3 %>%
  summarize(across(c(alcohol, densidy),
                   ~ max(.x, na.rm = TRUE),
                   .names = "{col}")) %>%
  mutate(Statistic = "Maximum")

BD3min <- BD3 %>%
  summarize(across(c(alcohol, densidy),
                   ~ min(.x, na.rm = TRUE),
                   .names = "{col}")) %>%
  mutate(Statistic = "Minimum")