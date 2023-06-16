# MODELOS ARBOLES DE REGRESION
# BIBLIOTECAS ARBOLES -----------------------------------------------------
library(rpart)
library(rpart.plot)
library(tidyverse)
library(dplyr)
library(FNN)
library(ggplot2)
library(modeest)
library(ISLR)
library(dslabs)
library(MASS)
library(caTools)
library(GGally)
library(readxl)

# DIVIDIR TRAIN Y TEST ----------------------------------------------------

df_completo <- read.csv("df_completo.csv")
# Train: 2018-2021 
# Test: 2022

# Semilla aleatoria para la reproducibilidad
set.seed(123)

train <- df_completo %>%
  filter(Year %in% c(2018,2019, 2020, 2021))

# TEST
test <- df_completo %>%
  filter(Year == 2022)


# ARBOL 1 -----------------------------------------------------------------

# Modelo con parametros pero con todas las variables
arbol1 <- rpart(Confianza_consumidor ~ .,
                data = train,
                control = rpart.control(
                  minbucket = 2,
                  cp = 0.1,
                  maxdepth = 5))

rpart.plot(arbol1)

pred1 <- predict(arbol1, newdata = test)

rpart.rules(arbol1, style = "tall")

# ERRORES arbol 1
# RSME
sqrt(mean((test$Confianza_consumidor - pred1)**2))# RMSE 26.142
# MAE
mean(abs(test$Confianza_consumidor - pred1))
# MAPE 
mean(abs((test$Confianza_consumidor - pred1)/test$Confianza_consumidor))

# Gráfico de líneas con datos reales y predicciones de Arbol

df_lineas1 <- data.frame(
  MES = test$MES,
  Reales = test$Confianza_consumidor,
  Predicciones = as.vector(pred1)
)

# Crear el gráfico de líneas
ggplot(data = df_lineas1, aes(x = MES)) +
  geom_line(aes(y = Reales, color = "Reales")) +
  geom_line(aes(y = Predicciones, color = "Predicciones")) +
  labs(colour = "", title = "Comparación de Datos Reales y Predicciones", x = "MES", y = "Confianza Consumidor") +
  scale_color_manual(values = c("Reales" = "blue", "Predicciones" = "red")) +
  theme_bw()

tabla_predicciones_arboles1 <- data.frame(MES = test_2022$MES, Year = test$Year,
                                 Valor_Real = test_2022$Confianza_consumidor,
                                 Prediccion = pred_2022)
print(tabla_predicciones_arboles1)
table_result_arboles <- aggregate(cbind(Valor_Real, Prediccion) ~ MES + Year, data = tabla_predicciones_arboles1, mean)
print(table_result_arboles1)


# ARBOL2 ------------------------------------------------------------------

# Con los mismos parametros que el 1 pero sin Year, MES y Variacion

arbol2 <- rpart(Confianza_consumidor ~ .-Year -MES -Variacion,
                data = train,
                control = rpart.control(
                  minbucket = 2,
                  cp = 0.01,
                  maxdepth = 8))

rpart.plot(arbol2)

pred2 <- predict(arbol2, newdata = test)

rpart.rules(arbol2, style = "tall")

# ERRORES arbol 2
# RSME
sqrt(mean((test$Confianza_consumidor - pred2)**2))
# MAE
mean(abs(test$Confianza_consumidor - pred2))
# MAPE 
mean(abs((test$Confianza_consumidor - pred2)/test$Confianza_consumidor))
# RMSE 21.87
# Gráfico de líneas con datos reales y predicciones de Arbol

df_lineas2 <- data.frame(
  MES = test$MES,
  Reales = test$Confianza_consumidor,
  Predicciones = as.vector(pred2)
)

# Crear el gráfico de líneas
ggplot(data = df_lineas2, aes(x = MES)) +
  geom_line(aes(y = Reales, color = "Reales")) +
  geom_line(aes(y = Predicciones, color = "Predicciones")) +
  labs(colour = "", title = "Comparación de Datos Reales y Predicciones", x = "MES", y = "Confianza Consumidor") +
  scale_color_manual(values = c("Reales" = "blue", "Predicciones" = "red")) +
  theme_bw()

tabla_predicciones_arboles2 <- data.frame(MES = test_2022$MES, Year = test$Year,
                                          Valor_Real = test_2022$Confianza_consumidor,
                                          Prediccion = pred2)
print(tabla_predicciones_arboles2)
table_result_arboles2 <- aggregate(cbind(Valor_Real, Prediccion) ~ MES + Year, data = tabla_predicciones_arboles2, mean)
print(table_result_arboles2)

# ARBOL 3 -----------------------------------------------------------------

# Con las variables de la correlacion
# Pero correlacion que incluye year, mes y variacion

# [1] "Year"                             "Valoracion_sit_ec_espana"        
# [3] "Evolucion_precio_vivienda"        "Valoracion_encontrar_empleo"     
# [5] "Valoracion_prospectiva_espana"    "Valoracion_sit_ec_hogar"         
# [7] "Valoracion_mejorar_empleo_esp_6m" "MES"                             
# [9] "Variacion"                        "Evolucion_ahorro_personal"   

arbol3 <- rpart(Confianza_consumidor ~ Year + Valoracion_sit_ec_espana +
                  Evolucion_precio_vivienda + Valoracion_encontrar_empleo +
                  Valoracion_prospectiva_espana + Valoracion_sit_ec_hogar +
                  Valoracion_mejorar_empleo_esp_6m + MES + Variacion +
                  Evolucion_ahorro_personal,
                data = train,
                control = rpart.control(maxdepth = 5,
                                        cp=0.01, minbucket = 2,
                                        minsplit = 1)) 
rpart.plot(arbol3)
summary(arbol3)
rpart.rules(arbol3, style = "tall")

pred3 <- predict(arbol3, newdata = test)

# ERRORES
# RSME
sqrt(mean((test$Confianza_consumidor - pred3)**2))
# MAE
mean(abs(test$Confianza_consumidor - pred3))
# MAPE 
mean(abs((test$Confianza_consumidor - pred3)/test$Confianza_consumidor))
# RMSE 28

# Gráfico de líneas con datos reales y predicciones de Arbol
df_lineas3 <- data.frame(
  MES = test$MES,
  Reales = test$Confianza_consumidor,
  Predicciones = as.vector(pred3)
)

# Crear el gráfico de líneas
ggplot(data = df_lineas3, aes(x = MES)) +
  geom_line(aes(y = Reales, color = "Reales")) +
  geom_line(aes(y = Predicciones, color = "Predicciones")) +
  labs(colour = "", title = "Comparación de Datos Reales y Predicciones", x = "MES", y = "Confianza Consumidor") +
  scale_color_manual(values = c("Reales" = "blue", "Predicciones" = "red")) +
  theme_bw()

tabla_predicciones_arboles3 <- data.frame(MES = test_2022$MES, Year = test$Year,
                                          Valor_Real = test_2022$Confianza_consumidor,
                                          Prediccion = pred3)
print(tabla_predicciones_arboles3)
table_result_arboles3 <- aggregate(cbind(Valor_Real, Prediccion) ~ MES + Year, data = tabla_predicciones_arboles3, mean)
print(table_result_arboles3)

# ARBOL 4 -----------------------------------------------------------------

# Con las variables de la correlacion tambien pero sin Year mes y variacion
# [1] "Valoracion_sit_ec_espana"         "Evolucion_precio_vivienda"       
# [3] "Valoracion_encontrar_empleo"      "Valoracion_prospectiva_espana"   
# [5] "Valoracion_sit_ec_hogar"          "Valoracion_mejorar_empleo_esp_6m"
# [7] "Evolucion_ahorro_personal"        "Valoracion_prospectiva_hogar"    
# [9] "Valoracion_inflacion"             "Evolucion_paro_entorno_6m"  

arbol4 <- rpart(Confianza_consumidor ~ Valoracion_sit_ec_espana +
                  Evolucion_precio_vivienda + Valoracion_encontrar_empleo +
                  Valoracion_prospectiva_espana + Valoracion_sit_ec_hogar +
                  Valoracion_mejorar_empleo_esp_6m + Evolucion_ahorro_personal +
                  Valoracion_prospectiva_hogar + Valoracion_inflacion +
                  Evolucion_paro_entorno_6m,
                data = train,
                control = rpart.control(maxdepth = 8,
                                        cp=0.001, minbucket = 5,
                                        minsplit = 1)) 

rpart.plot(arbol4)
summary(arbol4)
rpart.rules(arbol4, style = "tall")

pred4 <- predict(arbol4, newdata = test)

# ERRORES
# RSME
sqrt(mean((test$Confianza_consumidor - pred4)**2))
# MAE
mean(abs(test$Confianza_consumidor - pred4))
# MAPE 
mean(abs((test$Confianza_consumidor - pred4)/test$Confianza_consumidor))
# RMSE 20.92

# Gráfico de líneas con datos reales y predicciones de Arbol

df_lineas4 <- data.frame(
  MES = test$MES,
  Reales = test$Confianza_consumidor,
  Predicciones = as.vector(pred4)
)

# Crear el gráfico de líneas
ggplot(data = df_lineas4, aes(x = MES)) +
  geom_line(aes(y = Reales, color = "Reales")) +
  geom_line(aes(y = Predicciones, color = "Predicciones")) +
  labs(colour = "", title = "Comparación de Datos Reales y Predicciones", x = "MES", y = "Confianza Consumidor") +
  scale_color_manual(values = c("Reales" = "blue", "Predicciones" = "red")) +
  theme_bw()

tabla_predicciones_arboles4 <- data.frame(MES = test_2022$MES, Year = test$Year,
                                          Valor_Real = test_2022$Confianza_consumidor,
                                          Prediccion = pred4)
print(tabla_predicciones_arboles4)
table_result_arboles4 <- aggregate(cbind(Valor_Real, Prediccion) ~ MES + Year, data = tabla_predicciones_arboles4, mean)
print(table_result_arboles4)
# ARBOL 5 -----------------------------------------------------------------

# ARBOL 5 CON LAS VARIABLES DEL INFORME
arbol5 <- rpart(Confianza_consumidor ~ Situacion_economia_hogar +
                  Valoracion_mejorar_empleo_esp_6m + Valoracion_sit_ec_espana + 
                  Valoracion_encontrar_empleo + Valoracion_prospectiva_hogar +
                  Valoracion_prospectiva_espana,
                data = train,
                control = rpart.control(maxdepth = 10,
                                        cp=0.001, minbucket = 2,
                                        minsplit = 5))

rpart.plot(arbol5)
summary(arbol5)

rpart.rules(arbol5, style = "tall")

pred5 <- predict(arbol5, newdata = test)

# ERRORES
# RSME
sqrt(mean((test$Confianza_consumidor - pred5)**2))
# MAE
mean(abs(test$Confianza_consumidor - pred5))
# MAPE 
mean(abs((test$Confianza_consumidor - pred5)/test$Confianza_consumidor))
# RMSE 19.53
# Gráfico de líneas con datos reales y predicciones de Arbol

df_lineas5 <- data.frame(
  MES = test$MES,
  Reales = test$Confianza_consumidor,
  Predicciones = as.vector(pred5)
)

# Crear el gráfico de líneas
ggplot(data = df_lineas5, aes(x = MES)) +
  geom_line(aes(y = Reales, color = "Reales")) +
  geom_line(aes(y = Predicciones, color = "Predicciones")) +
  labs(colour = "", title = "Comparación de Datos Reales y Predicciones", x = "MES", y = "Confianza Consumidor") +
  scale_color_manual(values = c("Reales" = "blue", "Predicciones" = "red")) +
  theme_bw()

tabla_predicciones_arboles5 <- data.frame(MES = test_2022$MES, Year = test$Year,
                                          Valor_Real = test_2022$Confianza_consumidor,
                                          Prediccion = pred5)
print(tabla_predicciones_arboles5)
table_result_arboles5 <- aggregate(cbind(Valor_Real, Prediccion) ~ MES + Year, data = tabla_predicciones_arboles5, mean)
print(table_result_arboles5)
view(table_result_arboles5)

# ARBOL 6 -----------------------------------------------------------------
# important_features2
# [1] "Valoracion_encontrar_empleo"      "Evolucion_precio_vivienda"       
# [3] "Valoracion_sit_ec_espana"         "Valoracion_prospectiva_espana"   
# [5] "Valoracion_mejorar_empleo_esp_6m" "Valoracion_sit_ec_hogar"         
# [7] "Evolucion_tipo_interes"           "Valoracion_inflacion"            
# [9] "Intencion_comprar_vivienda"       "Edad"

# [1] "Year"                             "MES"                             
# [3] "Variacion"                        "Valoracion_sit_ec_espana"        
# [5] "Evolucion_tipo_interes"           "Valoracion_prospectiva_espana"   
# [7] "Valoracion_mejorar_empleo_esp_6m" "Evolucion_precio_vivienda"       
# [9] "Valoracion_sit_ec_hogar"          "Situacion_laboral"

arbol6 <- rpart(Confianza_consumidor ~ Valoracion_encontrar_empleo +
                  Evolucion_precio_vivienda + Valoracion_sit_ec_espana + Valoracion_prospectiva_espana + 
                  Valoracion_mejorar_empleo_esp_6m + Valoracion_sit_ec_hogar + Evolucion_tipo_interes +
                  Valoracion_inflacion + Intencion_comprar_vivienda + Edad,
                data = train,
                control = rpart.control(maxdepth = 8,
                                        cp=0.001, minbucket = 1,
                                        minsplit = 2))

arbol6 <- rpart(Confianza_consumidor ~ Year + MES + Variacion + Valoracion_sit_ec_espana +
                  Evolucion_tipo_interes + Valoracion_prospectiva_espana + 
                  Valoracion_mejorar_empleo_esp_6m + Evolucion_precio_vivienda +
                  Valoracion_sit_ec_hogar + Situacion_laboral,
                data = train,
                control = rpart.control(maxdepth = 8,
                                        cp=0.001, minbucket = 1,
                                        minsplit = 2))

rpart.plot(arbol6)
summary(arbol6)

pred6 <- predict(arbol6, newdata = test)
rpart.rules(arbol6, style = "tall")

# ERRORES
# RSME
sqrt(mean((test$Confianza_consumidor - pred6)**2))
# MAE
mean(abs(test$Confianza_consumidor - pred6))
# MAPE 
mean(abs((test$Confianza_consumidor - pred6)/test$Confianza_consumidor))
# Nada, 20.96

# Gráfico de líneas con datos reales y predicciones de Arbol

df_lineas6 <- data.frame(
  MES = test$MES,
  Reales = test$Confianza_consumidor,
  Predicciones = as.vector(pred6)
)

# Crear el gráfico de líneas
ggplot(data = df_lineas6, aes(x = MES)) +
  geom_line(aes(y = Reales, color = "Reales")) +
  geom_line(aes(y = Predicciones, color = "Predicciones")) +
  labs(colour = "", title = "Comparación de Datos Reales y Predicciones", x = "MES", y = "Confianza Consumidor") +
  scale_color_manual(values = c("Reales" = "blue", "Predicciones" = "red")) +
  theme_bw()


tabla_predicciones_arboles6 <- data.frame(MES = test_2022$MES, Year = test$Year,
                                          Valor_Real = test_2022$Confianza_consumidor,
                                          Prediccion = pred6)
table_result_arboles6 <- aggregate(cbind(Valor_Real, Prediccion) ~ MES + Year, data = tabla_predicciones_arboles6, mean)
print(table_result_arboles6)

# ARBOL 7 -----------------------------------------------------------------

arbol7 <- rpart(Confianza_consumidor ~ Valoracion_sit_ec_espana + Valoracion_encontrar_empleo +
                           Evolucion_precio_vivienda + Valoracion_prospectiva_espana + 
                           Valoracion_mejorar_empleo_esp_6m + Valoracion_inflacion + Evolucion_tipo_interes +
                           Valoracion_sit_ec_hogar + Evolucion_ahorro_personal + Valoracion_prospectiva_hogar,
                         data = train_reducido,
                         control = rpart.control(maxdepth = 8,
                                                 cp=0.001, minbucket = 1,
                                                 minsplit = 2))


rpart.plot(arbol7)
summary(arbol7)

pred7 <- predict(arbol7, newdata = test)

rpart.rules(arbol7, style = "tall")

# ERRORES
# RSME
sqrt(mean((test$Confianza_consumidor - pred7)**2))
# MAE
mean(abs(test$Confianza_consumidor - pred7))
# MAPE 
mean(abs((test$Confianza_consumidor - pred7)/test$Confianza_consumidor))
# Nada, 28
# Con el arbol 8 da igual, 20.98

# Gráfico de líneas con datos reales y predicciones de Arbol
pred7 <- predict(arbol7, newdata = test, type = "vector")


df_lineas7 <- data.frame(
  MES = test$MES,
  Reales = test$Confianza_consumidor,
  Predicciones = as.vector(pred7)
)

ggplot(data = df_lineas7, aes(x = MES)) +
  geom_line(aes(y = Reales, color = "Reales")) +
  geom_line(aes(y = Predicciones, color = "Predicciones")) +
  labs(colour = "", title = "Comparación de Datos Reales y Predicciones", x = "MES", y = "Confianza Consumidor") +
  scale_color_manual(values = c("Reales" = "blue", "Predicciones" = "red")) +
  theme_bw()

tabla_predicciones_arboles7 <- data.frame(MES = test_2022$MES, Year = test$Year,
                                          Valor_Real = test_2022$Confianza_consumidor,
                                          Prediccion = pred7)
table_result_arboles7 <- aggregate(cbind(Valor_Real, Prediccion) ~ MES + Year, data = tabla_predicciones_arboles7, mean)
print(table_result_arboles7)

# REDUCIR ERRORES ---------------------------------------------------------

# Ejecutar distinto codigo para reducir el error de los arboles

# Medidas de impureza
# Definición del modelo de árbol de decisión
set.seed(123)
tree_model <- rpart(Confianza_consumidor ~ ., data = df_train, method = "class",
                    control = rpart.control(cp = 0))

# Cálculo de la impureza utilizando la entropía, el índice de Gini y la ganancia de información
impurity_entropy <- t(apply(tree_model$frame[, 4], 1, function(x) {
  data.frame(entropy = sum(-x * log2(x)), measure = "entropy")
}))
impurity_gini <- t(apply(tree_model$frame[, 4], 1, function(x) {
  data.frame(gini = sum(x*(1-x)), measure = "gini")
}))
impurity_info_gain <- t(apply(tree_model$frame[, 5], 1, function(x) {
  data.frame(info_gain = x, measure = "info_gain")
}))

# Unión de los resultados en un único data.frame
impurity_measures <- rbind(impurity_entropy, impurity_gini, impurity_info_gain)

# Visualización de los resultados
library(ggplot2)
ggplot(impurity_measures, aes(x = rownames(impurity_measures), y = value, fill = measure)) +
  geom_col(position = "dodge") +
  theme_bw() +
  labs(x = "Nodo", y = "Impureza", fill = "Método de impureza") +
  theme(legend.position = "bottom")


# Recursive feature elimination
# Carga del paquete caret
library(caret)

# Selección de características mediante Recursive Feature Elimination
set.seed(123)
control <- rfeControl(functions = rfFuncs,
                      method = "cv",
                      number = 10,
                      verbose = FALSE)
rfe_results <- rfe(df_train[, -1], df_train$Confianza_consumidor,
                   sizes = c(1:ncol(df_train)-1),
                   rfeControl = control)

# Variables seleccionadas
selected_vars_caret <- predict(rfe_results, newdata = df_train[, -1]) %>% as.logical()
selected_vars_names_caret <- names(df_train[, -1])[selected_vars_caret]


# TABLA ERRORES ARBOLES ---------------------------------------------------

# Calcula las métricas de error y guárdalas en variables

rmse_arbol1 <- sqrt(mean((test$Confianza_consumidor - pred1)**2))
mae_arbol1 <- mean(abs(test$Confianza_consumidor - pred1))
mape_arbol1 <- mean(abs((test$Confianza_consumidor - pred1)/test$Confianza_consumidor))

rmse_arbol2 <- sqrt(mean((test$Confianza_consumidor - pred2)**2))
mae_arbol2 <- mean(abs(test$Confianza_consumidor - pred2))
mape_arbol2 <- mean(abs((test$Confianza_consumidor - pred2)/test$Confianza_consumidor))

rmse_arbol3 <- sqrt(mean((test$Confianza_consumidor - pred3)**2))
mae_arbol3 <- mean(abs(test$Confianza_consumidor - pred3))
mape_arbol3 <- mean(abs((test$Confianza_consumidor - pred3)/test$Confianza_consumidor))

rmse_arbol4 <- sqrt(mean((test$Confianza_consumidor - pred4)**2))
mae_arbol4 <- mean(abs(test$Confianza_consumidor - pred4))
mape_arbol4 <- mean(abs((test$Confianza_consumidor - pred4)/test$Confianza_consumidor))

rmse_arbol5 <- sqrt(mean((test$Confianza_consumidor - pred5)**2))
mae_arbol5 <- mean(abs(test$Confianza_consumidor - pred5))
mape_arbol5 <- mean(abs((test$Confianza_consumidor - pred5)/test$Confianza_consumidor))

rmse_arbol6 <- sqrt(mean((test$Confianza_consumidor - pred6)**2))
mae_arbol6 <- mean(abs(test$Confianza_consumidor - pred6))
mape_arbol6 <- mean(abs((test$Confianza_consumidor - pred6)/test$Confianza_consumidor))

rmse_arbol7 <- sqrt(mean((test$Confianza_consumidor - pred7)**2))
mae_arbol7 <- mean(abs(test$Confianza_consumidor - pred7))
mape_arbol7 <- mean(abs((test$Confianza_consumidor - pred7)/test$Confianza_consumidor))

# Crea un dataframe con los valores de las métricas 
tabla_errores <- data.frame(metrica = c("RMSE", "MAE", "MAPE"),
                            valor_arbol1 = c(rmse_arbol1, mae_arbol1, mape_arbol1),
                            valor_arbol2 = c(rmse_arbol2, mae_arbol2, mape_arbol2),
                            valor_arbol3 = c(rmse_arbol3, mae_arbol3, mape_arbol3),
                            valor_arbol4 = c(rmse_arbol4, mae_arbol4, mape_arbol4),
                            valor_arbol5 = c(rmse_arbol5, mae_arbol5, mape_arbol5),
                            valor_arbol6 = c(rmse_arbol6, mae_arbol6, mape_arbol6),
                            valor_arbol7 = c(rmse_arbol7, mae_arbol7, mape_arbol7))

# Muestra el dataframe
view(tabla_errores)
print(tabla_errores)


# PRUEBA PARAMETROS -------------------------------------------------------
# definir los valores posibles de los hiperparámetros
cp_values <- seq(0.001, 0.1, by = 0.001)
minbucket_values <- seq(1, 5, by = 1)
maxdepth_values <- seq(1, 10, by = 2)
minsplit_values <- seq(1, 3, by = 1)

# inicializar variables para guardar los valores óptimos de los hiperparámetros y los errores
opt_cp <- NA
opt_minbucket <- NA
opt_maxdepth <- NA
opt_minsplit <- NA
min_rmse <- Inf
min_mae <- Inf
min_mape <- Inf

# realizar la búsqueda de hiperparámetros
for (cp in cp_values) {
  for (minbucket in minbucket_values) {
    for (maxdepth in maxdepth_values) {
      for (minsplit in minsplit_values) {
        control <- rpart.control(cp = cp, minbucket = minbucket, maxdepth = maxdepth, minsplit = minsplit)
        modelo_arbol1 <- rpart(Confianza_consumidor ~ ., data = train, control = control)
        predicciones <- predict(modelo_arbol1, newdata = test)
        rmse <- sqrt(mean((test$Confianza_consumidor - predicciones)^2))
        mae <- mean(abs(test$Confianza_consumidor - predicciones))
        mape <- mean(abs((test$Confianza_consumidor - predicciones)/test$Confianza_consumidor))
        if (rmse < min_rmse) {
          opt_cp <- cp
          opt_minbucket <- minbucket
          opt_maxdepth <- maxdepth
          opt_minsplit <- minsplit
          min_rmse <- rmse
          min_mae <- mae
          min_mape <- mape
        }
      }
    }
  }
}

# imprimir los valores óptimos de los hiperparámetros y los errores mínimos
cat("Optimal cp value:", opt_cp, "\n")
cat("Optimal minbucket value:", opt_minbucket, "\n")
cat("Optimal maxdepth value:", opt_maxdepth, "\n")
cat("Optimal minsplit value:", opt_minsplit, "\n")
cat("Minimum RMSE:", min_rmse, "\n")
cat("Minimum MAE:", min_mae, "\n")
cat("Minimum MAPE:", min_mape, "\n")


# Con eso que me ha salido voy a intentar hacer el arbol 
modelo_parametros_sin <- rpart(Confianza_consumidor ~ . -Year -MES -Variacion,
                               data = train,
                               control = rpart.control(maxdepth = 8,
                                                       cp=0.001, minbucket = 1,
                                                       minsplit = 1))
rpart.plot(modelo_parametros_sin)
summary(modelo_parametros_sin)

pred_modelo_parametros_sin <- predict(modelo_parametros_sin, newdata = test)

rpart.rules(modelo_parametros_sin, style = "tall")

# ERRORES
# RSME
sqrt(mean((test$Confianza_consumidor - pred_modelo_parametros_sin)**2))
# MAE
mean(abs(test$Confianza_consumidor - pred_modelo_parametros_sin))
# MAPE 
mean(abs((test$Confianza_consumidor - pred_modelo_parametros_sin)/test$Confianza_consumidor))


# ajustar el modelo con los valores óptimos
control_sin <- rpart.control(cp = opt_cp, minbucket = opt_minbucket, maxdepth = opt_maxdepth, minsplit = opt_minsplit)
modelo_arbol2 <- rpart(Confianza_consumidor ~ .-Year -MES -Variacion, data = train, control = control_sin)

# hacer predicciones en el conjunto de prueba
predicciones2 <- predict(modelo_arbol2, newdata = test)

# evaluar el rendimiento del modelo en el conjunto de prueba
rmse2 <- sqrt(mean((test$Confianza_consumidor - predicciones2)^2))
mae2 <- mean(abs(test$Confianza_consumidor - predicciones2))
mape2 <- mean(abs((test$Confianza_consumidor - predicciones2)/test$Confianza_consumidor))

cat("RMSE:", rmse2, "\n")
cat("MAE:", mae2, "\n")
cat("MAPE:", mape2, "\n")


# VALIDACION CRUZADA de prueba ------------------------------------------------------

# Selección de variables significativas
variables_significativas10 <- c("Year", "Valoracion_sit_ec_espana", "Evolucion_precio_vivienda", 
                                "Valoracion_encontrar_empleo", "Valoracion_prospectiva_espana", 
                                "Valoracion_sit_ec_hogar", "Valoracion_mejorar_empleo_esp_6m", 
                                "MES", "Variacion", "Evolucion_ahorro_personal")

# Modelo con variables significativas
mod_arbol2 <- rpart(Confianza_consumidor ~ .,
                data = train[, c(variables_significativas10, "Confianza_consumidor")],
                control = rpart.control(
                  minbucket = 2,
                  cp = 0.1,
                  maxdepth = 5))

# Validación cruzada con k-fold
library(caret)
set.seed(123)
folds <- createFolds(train$Confianza_consumidor, k = 5)

# Almacenar errores de validación cruzada
rmse_values <- c()
mae_values <- c()
mape_values <- c()

for (fold in folds) {
  train_fold <- train[-fold, ]
  test_fold <- train[fold, ]
  
  # Ajustar modelo en cada fold
  arbol <- rpart(Confianza_consumidor ~ .,
                 data = train_fold[, c(variables_significativas10, "Confianza_consumidor")],
                 control = rpart.control(
                   minbucket = 2,
                   cp = 0.01,
                   maxdepth = 5))
  
  # Realizar predicciones en el fold de prueba
  pred <- predict(arbol, newdata = test_fold)
  
  # Calcular errores
  rmse_values <- c(rmse_values, sqrt(mean((test_fold$Confianza_consumidor - pred)^2)))
  mae_values <- c(mae_values, mean(abs(test_fold$Confianza_consumidor - pred)))
  mape_values <- c(mape_values, mean(abs((test_fold$Confianza_consumidor - pred) / test_fold$Confianza_consumidor)))
}

# Calcular la media de los errores de validación cruzada
mean_rmse <- mean(rmse_values)
mean_mae <- mean(mae_values)
mean_mape <- mean(mape_values)

# Imprimir resultados
cat("RMSE (Validación cruzada):", mean_rmse, "\n")
cat("MAE (Validación cruzada):", mean_mae, "\n")
cat("MAPE (Validación cruzada):", mean_mape, "\n")

# Ajustar el modelo de árbol de decisión
arbol_final <- rpart(Confianza_consumidor ~ .,
               data = train[, c(variables_significativas10, "Confianza_consumidor")],
               control = rpart.control(minbucket = 2, cp = 0.001, maxdepth = 5))

# Visualizar el árbol de decisión
rpart.plot(arbol_final)


# PRUEBA FINAL SIN YEAR MES Y VARIACION
# Selección de variables significativas
variables_significativas11 <- c("Valoracion_sit_ec_espana", "Evolucion_precio_vivienda",
                                "Valoracion_encontrar_empleo", "Valoracion_prospectiva_espana",
                                "Valoracion_sit_ec_hogar", "Valoracion_mejorar_empleo_esp_6m",
                                "Evolucion_ahorro_personal", "Valoracion_prospectiva_hogar",
                                "Valoracion_inflacion", "Evolucion_paro_entorno_6m")

# Modelo con variables significativas
mod_arbol3 <- rpart(Confianza_consumidor ~ .,
                    data = train[, c(variables_significativas11, "Confianza_consumidor")],
                    control = rpart.control(
                      minbucket = 2,
                      cp = 0.1,
                      maxdepth = 5))

# Validación cruzada con k-fold
library(caret)
set.seed(123)
folds <- createFolds(train$Confianza_consumidor, k = 5)

# Almacenar errores de validación cruzada
rmse_values <- c()
mae_values <- c()
mape_values <- c()

for (fold in folds) {
  train_fold <- train[-fold, ]
  test_fold <- train[fold, ]
  
  # Ajustar modelo en cada fold
  arbol <- rpart(Confianza_consumidor ~ .,
                 data = train_fold[, c(variables_significativas11, "Confianza_consumidor")],
                 control = rpart.control(
                   minbucket = 2,
                   cp = 0.01,
                   maxdepth = 8))
  
  # Realizar predicciones en el fold de prueba
  pred <- predict(arbol, newdata = test_fold)
  
  # Calcular errores
  rmse_values <- c(rmse_values, sqrt(mean((test_fold$Confianza_consumidor - pred)^2)))
  mae_values <- c(mae_values, mean(abs(test_fold$Confianza_consumidor - pred)))
  mape_values <- c(mape_values, mean(abs((test_fold$Confianza_consumidor - pred) / test_fold$Confianza_consumidor)))
}

# Calcular la media de los errores de validación cruzada
mean_rmse <- mean(rmse_values)
mean_mae <- mean(mae_values)
mean_mape <- mean(mape_values)

# Imprimir resultados
cat("RMSE (Validación cruzada):", mean_rmse, "\n")
cat("MAE (Validación cruzada):", mean_mae, "\n")
cat("MAPE (Validación cruzada):", mean_mape, "\n")

# Ajustar el modelo de árbol de decisión
arbol_final2 <- rpart(Confianza_consumidor ~ .,
                     data = train[, c(variables_significativas10, "Confianza_consumidor")],
                     control = rpart.control(minbucket = 2, cp = 0.01, maxdepth = 8))

# Visualizar el árbol de decisión
rpart.plot(arbol_final2)


# VALIDACION CRUZADA ARBOL1 ----------------------------------------------------

# Validación cruzada con k-fold
library(caret)
set.seed(123)

# Selección de variables significativas
variables_significativas10 <- c("Year", "Valoracion_sit_ec_espana", "Evolucion_precio_vivienda", 
                                "Valoracion_encontrar_empleo", "Valoracion_prospectiva_espana", 
                                "Valoracion_sit_ec_hogar", "Valoracion_mejorar_empleo_esp_6m", 
                                "MES", "Variacion", "Evolucion_ahorro_personal")

# Modelo con variables significativas
mod_arbol2 <- rpart(Confianza_consumidor ~ .,
                    data = train[, c(variables_significativas10, "Confianza_consumidor")],
                    control = rpart.control(
                      minbucket = 2,
                      cp = 0.1,
                      maxdepth = 5))

# Filtrar datos de entrenamiento (2018-2021)
train <- df_completo[df_completo$Year %in% c(2018:2021), ]

# Crear los índices para validación cruzada con k-folds
set.seed(123)
folds <- createFolds(train$Confianza_consumidor, k = 5)

# Almacenar errores de validación cruzada
rmse_values <- c()
mae_values <- c()
mape_values <- c()

# Realizar validación cruzada con k-folds
for (fold in folds) {
  train_fold <- train[-fold, ]
  test_fold <- train[fold, ]
  
  # Modelo con variables significativas
  mod_arbol <- rpart(Confianza_consumidor ~ .,
                     data = train_fold[, c(variables_significativas10, "Confianza_consumidor")],
                     control = rpart.control(minbucket = 2, cp = 0.1, maxdepth = 5))
  
  # Realizar predicciones en el fold de prueba
  pred <- predict(mod_arbol, newdata = test_fold)
  
  # Calcular errores
  rmse_values <- c(rmse_values, sqrt(mean((test_fold$Confianza_consumidor - pred)^2)))
  mae_values <- c(mae_values, mean(abs(test_fold$Confianza_consumidor - pred)))
  mape_values <- c(mape_values, mean(abs((test_fold$Confianza_consumidor - pred) / test_fold$Confianza_consumidor)))
}

# Calcular la media de los errores de validación cruzada
mean_rmse <- mean(rmse_values)
mean_mae <- mean(mae_values)
mean_mape <- mean(mape_values)

# Imprimir resultados
cat("RMSE (Validación cruzada):", mean_rmse, "\n")
cat("MAE (Validación cruzada):", mean_mae, "\n")
cat("MAPE (Validación cruzada):", mean_mape, "\n")

# Ajustar el modelo de árbol de decisión
arbol_final <- rpart(Confianza_consumidor ~ .,
                     data = train[, c(variables_significativas10, "Confianza_consumidor")],
                     control = rpart.control(minbucket = 2, cp = 0.001, maxdepth = 5))

# Visualizar el árbol de decisión
rpart.plot(arbol_final)


# Modelo final con todas las variables de entrenamiento
mod_arbol_final <- rpart(Confianza_consumidor ~ .,
                         data = train[, c(variables_significativas10, "Confianza_consumidor")],
                         control = rpart.control(minbucket = 2, cp = 0.1, maxdepth = 5))

# Realizar predicciones en los datos de prueba (2022)
pred_test <- predict(mod_arbol_final, newdata = test)

# Calcular errores en los datos de prueba
rmse_test <- sqrt(mean((test$Confianza_consumidor - pred_test)^2))
mae_test <- mean(abs(test$Confianza_consumidor - pred_test))
mape_test <- mean(abs((test$Confianza_consumidor - pred_test) / test$Confianza_consumidor))

# Imprimir resultados del modelo final en los datos de prueba
cat("RMSE (Prueba):", rmse_test, "\n")
cat("MAE (Prueba):", mae_test, "\n")
cat("MAPE (Prueba):", mape_test, "\n")
pred_arbol_final <- predict(arbol_final, newdata = test_fold)

# ERRORES
# RSME
sqrt(mean((test_fold$Confianza_consumidor - pred_arbol_final)**2))
# MAE
mean(abs(test$Confianza_consumidor - pred_arbol_final))
# MAPE 
mean(abs((test$Confianza_consumidor - pred_arbol_final)/test$Confianza_consumidor))


# VALIDACION CRUZADA ARBOL 2 ----------------------------------------------

# PRUEBA FINAL SIN YEAR MES Y VARIACION
# Validación cruzada con k-fold
library(caret)
set.seed(123)

# Selección de variables significativas
variables_significativas11 <- c("Valoracion_sit_ec_espana", "Evolucion_precio_vivienda",
                                "Valoracion_encontrar_empleo", "Valoracion_prospectiva_espana",
                                "Valoracion_sit_ec_hogar", "Valoracion_mejorar_empleo_esp_6m",
                                "Evolucion_ahorro_personal", "Valoracion_prospectiva_hogar",
                                "Valoracion_inflacion", "Evolucion_paro_entorno_6m")

# Modelo con variables significativas
mod_arbol3 <- rpart(Confianza_consumidor ~ .,
                    data = train[, c(variables_significativas11, "Confianza_consumidor")],
                    control = rpart.control(
                      minbucket = 2,
                      cp = 0.01,
                      maxdepth = 5))

# Filtrar datos de entrenamiento (2018-2021)
train <- df_completo[df_completo$Year %in% c(2018:2021), ]

# Crear los índices para validación cruzada con k-folds
set.seed(123)
folds <- createFolds(train$Confianza_consumidor, k = 5)

# Almacenar errores de validación cruzada
rmse_values <- c()
mae_values <- c()
mape_values <- c()

# Realizar validación cruzada con k-folds
for (fold in folds) {
  train_fold <- train[-fold, ]
  test_fold <- train[fold, ]
  
  # Modelo con variables significativas
  mod_arbol <- rpart(Confianza_consumidor ~ .,
                     data = train_fold[, c(variables_significativas11, "Confianza_consumidor")],
                     control = rpart.control(minbucket = 2, cp = 0.01, maxdepth = 5))
  
  # Realizar predicciones en el fold de prueba
  pred <- predict(mod_arbol, newdata = test_fold)
  
  # Calcular errores
  rmse_values <- c(rmse_values, sqrt(mean((test_fold$Confianza_consumidor - pred)^2)))
  mae_values <- c(mae_values, mean(abs(test_fold$Confianza_consumidor - pred)))
  mape_values <- c(mape_values, mean(abs((test_fold$Confianza_consumidor - pred) / test_fold$Confianza_consumidor)))
}

# Calcular la media de los errores de validación cruzada
mean_rmse <- mean(rmse_values)
mean_mae <- mean(mae_values)
mean_mape <- mean(mape_values)

# Imprimir resultados
cat("RMSE (Validación cruzada):", mean_rmse, "\n")
cat("MAE (Validación cruzada):", mean_mae, "\n")
cat("MAPE (Validación cruzada):", mean_mape, "\n")
# NO es un buen modelo RMSE 14

# Modelo final con todas las variables de entrenamiento
mod_arbol_final <- rpart(Confianza_consumidor ~ .,
                         data = train[, c(variables_significativas11, "Confianza_consumidor")],
                         control = rpart.control(minbucket = 2, cp = 0.1, maxdepth = 5))

# Realizar predicciones en los datos de prueba (2022)
pred_test <- predict(mod_arbol_final, newdata = test)

# Calcular errores en los datos de prueba
rmse_test <- sqrt(mean((test$Confianza_consumidor - pred_test)^2))
mae_test <- mean(abs(test$Confianza_consumidor - pred_test))
mape_test <- mean(abs((test$Confianza_consumidor - pred_test) / test$Confianza_consumidor))

# Imprimir resultados del modelo final en los datos de prueba
cat("RMSE (Prueba):", rmse_test, "\n")
cat("MAE (Prueba):", mae_test, "\n")
cat("MAPE (Prueba):", mape_test, "\n")
pred_arbol_final <- predict(arbol_final, newdata = test_fold)


# VALIDACION CRUZADA ARBOL 3 ----------------------------------------------

# COn las 20 variables importantes en correlacion
set.seed(123)

# Definir variables significativas
variables_significativas13 <- c("Valoracion_prospectiva_hogar", "Evolucion_ahorro_personal",
                              "MES", "Valoracion_mejorar_empleo_esp_6m", "Valoracion_sit_ec_hogar",
                              "Valoracion_prospectiva_espana", "Valoracion_encontrar_empleo", "Year",
                              "Evolucion_precio_vivienda", "Valoracion_sit_ec_espana", "Variacion",
                              "Evolucion_paro_entorno_6m", "Adquisicion_electrodomestico_grande",
                              "Adquisicion_electrodomestico_pequeño", "Adquisicion_mueble",
                              "Personas_paro_entorno", "Situacion_laboral", "Intencion_comprar_vivienda",
                              "Sexo")

# Almacenar errores de validación cruzada
rmse_values <- c()
mae_values <- c()
mape_values <- c()

# Filtrar datos de entrenamiento (2018-2021)
train <- df_completo[df_completo$Year %in% c(2018:2021), ]

# Crear los índices para validación cruzada con k-folds
set.seed(123)
folds <- createFolds(train$Confianza_consumidor, k = 5)

for (fold in folds) {
  train_fold <- train[-fold, ]
  test_fold <- train[fold, ]
  
  # Modelo con variables significativas
  mod_arbol <- rpart(Confianza_consumidor ~ .,
                     data = train_fold[, c(variables_significativas13, "Confianza_consumidor")],
                     control = rpart.control(minbucket = 2, cp = 0.01, maxdepth = 5))
  
  # Realizar predicciones en el fold de prueba
  pred <- predict(mod_arbol, newdata = test_fold)
  
  # Calcular errores
  rmse_values <- c(rmse_values, sqrt(mean((test_fold$Confianza_consumidor - pred)^2)))
  mae_values <- c(mae_values, mean(abs(test_fold$Confianza_consumidor - pred)))
  mape_values <- c(mape_values, mean(abs((test_fold$Confianza_consumidor - pred) / test_fold$Confianza_consumidor)))
}

# Calcular la media de los errores de validación cruzada
mean_rmse <- mean(rmse_values)
mean_mae <- mean(mae_values)
mean_mape <- mean(mape_values)

# Imprimir resultados
cat("RMSE (Validación cruzada):", mean_rmse, "\n")
cat("MAE (Validación cruzada):", mean_mae, "\n")
cat("MAPE (Validación cruzada):", mean_mape, "\n")

pred <- predict(mod_arbol, newdata = test_fold)
tabla_predicciones_arboles_folds <- data.frame(MES = test_fold$MES, Year = test_fold$Year,
                                          Valor_Real = test_fold$Confianza_consumidor,
                                          Prediccion = pred)
print(tabla_predicciones_arboles_folds)
table_result_arboles_folds <- aggregate(cbind(Valor_Real, Prediccion) ~ MES + Year, data = tabla_predicciones_arboles_folds, mean)
print(table_result_arboles_folds)

# VALIDACION CRUZADA ARBOL 4 ----------------------------------------------

# Con las variables del informe
set.seed(123)

# Definir variables significativas
variables_significativas14 <- c("Situacion_economia_hogar",
                                "Valoracion_mejorar_empleo_esp_6m", "Valoracion_sit_ec_espana",
                                "Valoracion_prospectiva_hogar", "Valoracion_encontrar_empleo", 
                                "Valoracion_prospectiva_espana")

# Almacenar errores de validación cruzada
rmse_values <- c()
mae_values <- c()
mape_values <- c()

# Filtrar datos de entrenamiento (2018-2021)
train <- df_completo[df_completo$Year %in% c(2018:2021), ]

# Crear los índices para validación cruzada con k-folds
set.seed(123)
folds <- createFolds(train$Confianza_consumidor, k = 5)

for (fold in folds) {
  train_fold <- train[-fold, ]
  test_fold <- train[fold, ]
  
  # Modelo con variables significativas
  mod_arbol <- rpart(Confianza_consumidor ~ .,
                     data = train_fold[, c(variables_significativas14, "Confianza_consumidor")],
                     control = rpart.control(minbucket = 2, cp = 0.01, maxdepth = 5))
  
  # Realizar predicciones en el fold de prueba
  pred <- predict(mod_arbol, newdata = test_fold)
  
  # Calcular errores
  rmse_values <- c(rmse_values, sqrt(mean((test_fold$Confianza_consumidor - pred)^2)))
  mae_values <- c(mae_values, mean(abs(test_fold$Confianza_consumidor - pred)))
  mape_values <- c(mape_values, mean(abs((test_fold$Confianza_consumidor - pred) / test_fold$Confianza_consumidor)))
}

# Calcular la media de los errores de validación cruzada
mean_rmse <- mean(rmse_values)
mean_mae <- mean(mae_values)
mean_mape <- mean(mape_values)

# Imprimir resultados
cat("RMSE (Validación cruzada):", mean_rmse, "\n")
cat("MAE (Validación cruzada):", mean_mae, "\n")
cat("MAPE (Validación cruzada):", mean_mape, "\n")

# Error bastante alto, no nos interesa
# 15.46

# COMPARACION DE PREDICCIONES  -------------------------------------------------------------


# PARA ARBOL CON VARIABLES SIGNIFICATIVAS 14
# Filtrar datos de prueba (2022)
test_2022 <- df_completo[df_completo$Year == 2022, ]

# Modelo con variables significativas
mod_arbol <- rpart(Confianza_consumidor ~ .,
                   data = train[, c(variables_significativas14, "Confianza_consumidor")],
                   control = rpart.control(minbucket = 2, cp = 0.01, maxdepth = 5))

# Realizar predicciones en los datos de prueba de 2022
pred_2022 <- predict(mod_arbol, newdata = test_2022)

# Comparar predicciones con datos reales
comparacion <- data.frame(Confianza_real = test_2022$Confianza_consumidor, Prediccion = pred_2022)

# Imprimir comparación
print(comparacion)

# Agregar columna MES a comparacion
comparacion$MES <- month(test_2022$MES)

# Calcular la media de las predicciones por mes
prediccion_media <- aggregate(Prediccion ~ MES, data = comparacion, FUN = mean)

# Calcular la media del valor real por mes
valor_real_media <- aggregate(Confianza_real ~ MES, data = comparacion, FUN = mean)

# Imprimir las medias de las predicciones y el valor real
print(prediccion_media)
print(valor_real_media)
# Combinar las medias de las predicciones y el valor real en una misma tabla
resultados <- merge(prediccion_media, valor_real_media, by = "MES", suffixes = c("_Prediccion", "_ValorReal"))

# Imprimir los resultados combinados
print(resultados)



# PERO PARA PREDICCIONES ÚNICAS PARA CADA MES
# Filtrar datos de prueba (2022)
test_2022 <- df_completo[df_completo$Year == 2022, ]

# Agregar datos por mes para obtener un único valor de confianza por mes
test_agregado <- aggregate(Confianza_consumidor ~ MES, data = test_2022, FUN = function(x) unique(x))

# Crear vector para almacenar las predicciones por mes
predicciones_por_mes <- vector("numeric", length = nrow(test_agregado))

# Modelo con variables significativas
mod_arbol <- rpart(Confianza_consumidor ~ .,
                   data = train[, c(variables_significativas14, "Confianza_consumidor")],
                   control = rpart.control(minbucket = 2, cp = 0.01, maxdepth = 5))

# Realizar predicciones por mes
for (i in 1:nrow(test_agregado)) {
  confianza_mes <- test_agregado$Confianza_consumidor[i]
  
  # Realizar la predicción con el valor único del índice de confianza
  pred_mes <- predict(mod_arbol, newdata = data.frame(Confianza_consumidor = confianza_mes))
  
  # Almacenar la predicción en el vector de predicciones por mes
  predicciones_por_mes[i] <- pred_mes
}

# Combinar predicciones con datos reales
predicciones_completas <- cbind(test_agregado, Prediccion = predicciones_por_mes)

# Imprimir predicciones por mes
print(predicciones_completas)


# Agregar datos por mes para obtener un único valor de confianza por mes
test_agregado <- aggregate(Confianza_consumidor ~ MES, data = test_2022, FUN = function(x) unique(x))

# Crear vector para almacenar las predicciones por mes
predicciones_por_mes <- vector("numeric", length = nrow(test_agregado))

# Modelo con variables significativas
mod_arbol <- rpart(formula = Confianza_consumidor ~ Situacion_economia_hogar +
                     Valoracion_mejorar_empleo_esp_6m + Valoracion_sit_ec_espana +
                     Valoracion_prospectiva_hogar + Valoracion_encontrar_empleo +
                     Valoracion_prospectiva_espana,
                   data = train,
                   control = rpart.control(minbucket = 2, cp = 0.01, maxdepth = 5))

# Realizar predicciones por mes
for (i in 1:nrow(test_agregado)) {
  confianza_mes <- test_agregado$Confianza_consumidor[i]
  
  # Realizar la predicción con el valor único del índice de confianza
  pred_mes <- predict(mod_arbol, newdata = data.frame(Situacion_economia_hogar = confianza_mes,
                                                      Valoracion_mejorar_empleo_esp_6m = confianza_mes,
                                                      Valoracion_sit_ec_espana = confianza_mes,
                                                      Valoracion_prospectiva_hogar = confianza_mes,
                                                      Valoracion_encontrar_empleo = confianza_mes,
                                                      Valoracion_prospectiva_espana = confianza_mes))
  
  # Almacenar la predicción en el vector de predicciones por mes
  predicciones_por_mes[i] <- pred_mes
}

# Combinar predicciones con datos reales
predicciones_completas <- cbind(test_agregado, Prediccion = predicciones_por_mes)

# Imprimir predicciones por mes
print(predicciones_completas)

# MODELO FINAL ------------------------------------------------------------
# Modelo final con todas las variables significativas
mod_arbol_final <- rpart(Confianza_consumidor ~ .,
                         data = train_fold[, c(variables_significativas13, "Confianza_consumidor")],
                         control = rpart.control(minbucket = 2, cp = 0.01, maxdepth = 5))

# Gráfico del árbol de decisión final
rpart.plot(mod_arbol_final)

# Realizar predicciones en los datos de prueba del fold actual
predicciones <- predict(mod_arbol_final, newdata = test_fold)

# Calcular errores
rmse <- sqrt(mean((test_fold$Confianza_consumidor - predicciones)^2))
mae <- mean(abs(test_fold$Confianza_consumidor - predicciones))
mape <- mean(abs((test_fold$Confianza_consumidor - predicciones) / test_fold$Confianza_consumidor))

# Imprimir errores
cat("RMSE:", rmse, "\n")
cat("MAE:", mae, "\n")
cat("MAPE:", mape, "\n")

# Crear un data frame con los valores reales y predichos
df_predicciones <- data.frame(Confianza_real = test$Confianza_consumidor, Predicciones = predicciones, MES = test$MES)

# Crear el gráfico de líneas
ggplot(data = df_predicciones, aes(x = MES)) +
  geom_line(aes(y = Confianza_real, color = "Reales")) +
  geom_line(aes(y = Predicciones, color = "Predicciones")) +
  labs(colour = "", title = "Comparación de Datos Reales y Predicciones", x = "MES", y = "Confianza Consumidor") +
  scale_color_manual(values = c("Reales" = "blue", "Predicciones" = "red")) +
  theme_bw()


# VALIDACION CRUZADA CON PREDICCIONES -------------------------------------

train_data <- df_completo[df_completo$Year %in% 2018:2021, ]
test_data <- df_completo[df_completo$Year == 2022, ]

predictions_matrix <- matrix(NA, nrow = nrow(test_data), ncol = 1)

k <- 5  # número de kfolds
folds <- cut(seq(1, nrow(train_data)), breaks = k, labels = FALSE)  # asignar los kfolds a las filas de train_data

for (i in 1:k) {
  # Obtener el conjunto de entrenamiento y prueba para el k-fold actual
  train_fold <- train_data[folds != i, ]
  test_fold <- train_data[folds == i, ]
  
  # Entrenar el modelo en el conjunto de entrenamiento
  model <- rpart(Confianza_consumidor ~ ., data = train_fold, control = rpart.control(minbucket = 2, cp = 0.1, maxdepth = 5))
  
  # Realizar las predicciones en el conjunto de prueba
  predictions <- predict(model, newdata = test_fold)
  
  # Almacenar las predicciones en la matriz
  predictions_matrix[folds == i, ] <- predictions
}

resultados_nuevos <- data.frame(MES = test_data$MES,
                     Valor_Real = test_data$Confianza_consumidor,
                     Prediccion = predictions_matrix)

print(resultados_nuevos)



# ARBOL SIN VARIABLES MULTICOLINEALIDAD -----------------------------------

# Definir variables significativas
variables_significativas13 <- c("Valoracion_prospectiva_hogar", "Evolucion_ahorro_personal",
                                "MES", "Valoracion_mejorar_empleo_esp_6m", "Valoracion_sit_ec_hogar",
                                "Valoracion_prospectiva_espana", "Valoracion_encontrar_empleo", "Year",
                                "Evolucion_precio_vivienda", "Valoracion_sit_ec_espana", "Variacion",
                                "Evolucion_paro_entorno_6m", "Adquisicion_electrodomestico_grande",
                                "Adquisicion_electrodomestico_pequeño", "Adquisicion_mueble",
                                "Personas_paro_entorno", "Situacion_laboral", "Intencion_comprar_vivienda",
                                "Sexo")

arbol8 <- rpart(Confianza_consumidor ~ Year + 
                  Evolucion_precio_vivienda + Valoracion_encontrar_empleo +
                  Valoracion_sit_ec_hogar + Valoracion_prospectiva_hogar +
                  Valoracion_mejorar_empleo_esp_6m + MES + Variacion +
                  Evolucion_paro_entorno_6m + Adquisicion_electrodomestico_grande +
                  Situacion_laboral,
                data = train,
                control = rpart.control(maxdepth = 5,
                                        cp=0.01, minbucket = 2,
                                        minsplit = 1)) 
rpart.plot(arbol8)
summary(arbol8)
rpart.rules(arbol8, style = "tall")

pred8 <- predict(arbol8, newdata = test)

# ERRORES
# RSME
sqrt(mean((test$Confianza_consumidor - pred8)**2))
# MAE
mean(abs(test$Confianza_consumidor - pred8))
# MAPE 
mean(abs((test$Confianza_consumidor - pred8)/test$Confianza_consumidor))
# RMSE 28

# Gráfico de líneas con datos reales y predicciones de Arbol
df_lineas8 <- data.frame(
  MES = test$MES,
  Reales = test$Confianza_consumidor,
  Predicciones = as.vector(pred8)
)

# Crear el gráfico de líneas
ggplot(data = df_lineas8, aes(x = MES)) +
  geom_line(aes(y = Reales, color = "Reales")) +
  geom_line(aes(y = Predicciones, color = "Predicciones")) +
  labs(colour = "", title = "Comparación de Datos Reales y Predicciones", x = "MES", y = "Confianza Consumidor") +
  scale_color_manual(values = c("Reales" = "blue", "Predicciones" = "red")) +
  theme_bw()

tabla_predicciones_arboles8 <- data.frame(MES = test_2022$MES, Year = test$Year,
                                          Valor_Real = test_2022$Confianza_consumidor,
                                          Prediccion = pred8)
print(tabla_predicciones_arboles8)
table_result_arboles8 <- aggregate(cbind(Valor_Real, Prediccion) ~ MES + Year, data = tabla_predicciones_arboles8, mean)
print(table_result_arboles8)

