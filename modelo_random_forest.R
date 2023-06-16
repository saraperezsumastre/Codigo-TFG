# LIBRERIAS ---------------------------------------------------------------
library(ggplot2)
library(randomForest)
library(tidyverse)
library(dplyr)
library(readxl)

# TRAIN TEST --------------------------------------------------------------

# Como me habia colapsado alguna vez, lo voy a hacer sobre el dataframe reducido

# Semilla aleatoria para la reproducibilidad
set.seed(123)
#TRAIN
train_rf <- df_reducido %>%
  filter(Year %in% c(2018,2019, 2020, 2021))
# TEST
test_rf <- df_reducido %>%
  filter(Year == 2022)
# Se queda train en 24.000 y test en 5.500

# MODELO 1 RF -------------------------------------------------------------

# caragar ux random forest
source("aux_random_forest.R")

# variable objetivo es Confianza_consumidor
# el resto variables predictoras

# entrenar un randomforest
# . para decir que use todas las variables predictoras sin tener que escribirlas
mod_rf <- randomForest(Confianza_consumidor ~ ., data = train_rf)

pred_rf_train_rf <- predict(mod_rf, newdata = train_rf)
pred_rf_test_rf <- predict(mod_rf, newdata = test_rf)

rmse(train_rf$Confianza_consumidor, pred_rf_train_rf)
# 0.226
rmse(test_rf$Confianza_consumidor, pred_rf_test_rf)
# 0.479

# INTUIMOS QUE PUEDE HABER OVERFITTING
# porque hay mucha diferencia entre el rmse en train_rf y test_rf

# especificar el numero de variables y numero de muestras
# ntree nº de arboles que quiero generar
# mtry las veces que se hacen los cortes el m minuscula de random forest
mod_rf_2 <- randomForest(Confianza_consumidor ~ ., data = train_rf,
                         ntree = 250,
                         mtry = 3)


pred_rf_train_rf2 <- predict(mod_rf_2, newdata = train_rf)
pred_rf_test_rf2 <- predict(mod_rf_2, newdata = test_rf)

rmse(train_rf$Confianza_consumidor, pred_rf_train_rf2)
# AQUI SALE MAS ALTO CON 3.56
rmse(test_rf$Confianza_consumidor, pred_rf_test_rf2)
# 6.75
# Buscar la mejor combinacion con gridsearch

# GRID SEARCH -------------------------------------------------------------
# los valores posibles que voy a probar de cada hiperparametro

mtry <- 1:6
ntree <- c(1, 250, 500, 750, 1000, 1250, 1500)
# configuraciones posibles 7*6 porque solo hay 2 dimensiones
# con lo cual hay 42 posibles combinaciones

# construir la malla en un formato dataframe
# producto cartesiano, ver todas combinaciones posibles
# es la funcion expand.grid, tendria que tener 42 filas
grid_search <- expand.grid(mtry = mtry, ntree = ntree)
# NO ENTIENDO ESTO

# Prueba
tuneRF(train_rf[, -1], train_rf$Confianza_consumidor, mtryStart = 1, mtryEnd = 6, ntreeTry = 6, stepFactor = 1.5)
# mtry = 1  OOB error = 154.5433  Searching left ... Searching right ... mtry OOBError 1    1 154.5433
