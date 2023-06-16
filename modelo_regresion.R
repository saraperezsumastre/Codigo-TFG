# MODELOS REGRESION
# LIBRERIAS ---------------------------------------------------------------

library(ISLR)
library(dslabs)
library(MASS)
library(caTools)
library(GGally)
library(stats)
library(ggplot2)
library(tseries)
library(dplyr)
library(foreign)
library(lmtest)
library(stats)
library(Metrics)

# TRAIN Y TEST ------------------------------------------------------------
# TRAIN
# ES 2018-2021
set.seed(123)
train_regresion <- df_completo %>%
  filter(Year %in% c(2018,2019, 2020, 2021))
# TEST
# ES 2022
test_regresion <- df_completo %>%
  filter(Year == 2022)

# REDUCIDO
train_regresion_reducido <- df_reducido %>%
  filter(Year %in% c(2018,2019, 2020, 2021))
# TEST
# ES 2022
test_regresion_reducido <- df_reducido %>%
  filter(Year == 2022)

# PREVIO ------------------------------------------------------------------
#1
# Correlacion
cor(train_regresion[, -which(names(train_regresion) == "Confianza_consumidor")], train_regresion$Confianza_consumidor)
cor(train_regresion_reducido[, -which(names(train_regresion_reducido) == "Confianza_consumidor")], train_regresion_reducido$Confianza_consumidor)

#2
# Analisis PCA
pca_regresion <- prcomp(train_regresion[, -which(names(train_regresion) == "Confianza_consumidor")])
summary(pca_regresion)
pca_regresion_reducido <- prcomp(train_regresion_reducido[, -which(names(train_regresion_reducido) == "Confianza_consumidor")])
summary(pca_regresion_reducido)

#3
# Seleccion de caracteristicas
model_reg <- lm(Confianza_consumidor ~ ., data = train_regresion)
reduced_model_reg <- step(model_reg, direction = "backward")
summary(reduced_model_reg)

# SIN 
train_sin <- subset(train_regresion, select = -c(Year, MES, Variacion))
test_sin <- subset(test_regresion, select = -c(Year, MES, Variacion))

model_reg_sin <- lm(Confianza_consumidor ~ ., data = train_sin)
reduced_model_reg_sin <- step(model_reg_sin, direction = "backward")
summary(reduced_model_reg_sin)

# Seleccion de caracteristicas REDUCIDO
model_reg_reducido <- lm(Confianza_consumidor ~ ., data = train_regresion_reducido)
reduced_model_reg_reducido <- step(model_reg_reducido, direction = "backward")
summary(reduced_model_reg_reducido)

# SIN REDUCIDO
train_sin_reducido <- subset(train_regresion_reducido, select = -c(Year, MES, Variacion))
test_sin_reducido <- subset(test_regresion_reducido, select = -c(Year, MES, Variacion))

model_reg_sin_reducido <- lm(Confianza_consumidor ~ ., data = train_sin_reducido)
reduced_model_reg_sin_reducido <- step(model_reg_sin_reducido, direction = "backward")
summary(reduced_model_reg_sin_reducido)

# REGRESION prueba -------------------------------------------------------------
# Con el codigo de ceci 
## Ajuste del modelo de regresión lineal simple con train
modelo_reg_prueba <- lm(Confianza_consumidor ~ ., data=train_regresion)

# Comprobar el patrón lineal de los residuos
#plot(modelo, which = 1)
plot(modelo_reg_prueba, which = 1)

# Comprobar la homocedasticidad de los residuos
bartlett.test(resid(modelo_reg_prueba) ~ train_regresion$Confianza_consumidor)

# Comprobar la normalidad de los residuos
shapiro.test(resid(modelo_reg_prueba[1:5000])) #ERROR

# Comprobar las hipótesis sobre las que se construye el modelo
# Hipótesis de homocedasticidad
residuos <- resid(modelo_reg_prueba)
residuos_estandarizados <- rstandard(modelo_reg_prueba)
print(residuos)
print(residuos_estandarizados)
plot(fitted(modelo_reg_prueba), residuos_estandarizados, main = "Gráfico de residuos estandarizados vs valores ajustados")

#Normalidad de los residuos
abline(h = 0)
hist(residuos_estandarizados, main = "Histograma de residuos estandarizados")
qqnorm(residuos_estandarizados, main = "Gráfico QQ de residuos estandarizados")
qqline(residuos_estandarizados)

#Patrón lineal de los residuos
plot(fitted(modelo_reg_prueba), residuos, main = "Gráfico de residuos vs valores ajustados") +
  abline(h = 0)

## Predicciones con test
predicciones_reg_prueba <- predict(modelo_reg_prueba, newdata=test_regresion)
print(predicciones_reg_prueba)
plot(predicciones_reg_prueba)

# # Predicción de IPC_ESP para un valor de PIB específico
# nuevo_valor_CC <- 94
# prediccion_CC_ESP <- predict(modelo_reg_prueba, data.frame(Confianza_consumidor = nuevo_valor_CC))
# print(prediccion_CC_ESP)
# # Resultado de la predicción
# cat("La predicción de IPC_ESP para un valor de ICC de", nuevo_valor_CC, "es:", prediccion_CC_ESP)

# Evaluación del modelo
MSE <- mean((predicciones_reg_prueba - test$Confianza_consumidor)^2)
print(MSE)

RMSE <- sqrt(MSE)
print(RMSE)

r_cuadrado <- summary(modelo_reg_prueba)$r.squared
print(r_cuadrado)

MAPE <- mean(abs((test$Confianza_consumidor - predicciones_reg_prueba)/test$Confianza_consumidor))*100
print(MAPE)

MAE <- mean(abs(test$Confianza_consumidor - predicciones_reg_prueba))
print(MAE)


# REGRESION 1 -------------------------------------------------------------

# Con las conclusiones modelo backwards
r1 <- lm(Confianza_consumidor ~ ., 
         data = train_regresion)

# Hacer predicciones sobre los datos de prueba
pred_r1 <- predict(r1, newdata = test_regresion)

# Calcular el coeficiente de determinación R-cuadrado
r_squared_r1 <- summary(r1)$r.squared
print(r_squared_r1)
# Calcular la raíz del error cuadrático medio (RMSE)
rmse_r1 <- sqrt(mean((test_regresion$Confianza_consumidor - pred_r1)^2))
print(rmse_r1)
# Calcular el error absoluto medio (MAE)
mae_r1 <- mean(abs(test_regresion$Confianza_consumidor - pred_r1))
print(mae_r1)
# rmse da 12

# Test shappiro-wilk normality del modelo
residuos_modelo1 <- residuals(r1)
shapiro.test(residuos_modelo1[1:5000])

# REGRESION 2 -------------------------------------------------------------

# Sin año mes y variacion
# Ajustar el modelo de regresión lineal
r2 <- lm(Confianza_consumidor ~ .-Year -MES -Variacion, data = train_regresion)
r2 <- lm(Confianza_consumidor ~ ., data = train_sin)

# Hacer predicciones sobre los datos de prueba
pred_r2 <- predict(r2, newdata = test_regresion)

# Calcular el coeficiente de determinación R-cuadrado
r_squared_r2 <- summary(r2)$r.squared
print(r_squared_r2)
# Calcular la raíz del error cuadrático medio (RMSE)
rmse_r2 <- sqrt(mean((test_regresion$Confianza_consumidor - pred_r2)^2))
print(rmse_r2)
# Calcular el error absoluto medio (MAE)
mae_r2 <- mean(abs(test_regresion$Confianza_consumidor - pred_r2))
print(mae_r2)

# Calcular los residuos
residuos <- test$Confianza_consumidor - pred_r1

# Realizar la prueba de Anderson-Darling
library(nortest)
ad_test <- ad.test(residuos)
print(ad_test)

# Gráfico de los residuos
plot(residuos, type = "p")

# Comprobar la homocedasticidad
ggplot(data = data.frame(residuos = residuos, predicciones = pred_r2),
       aes(x = predicciones, y = residuos)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Crear un gráfico de residuos vs. valores ajustados
plot(pred_r2, residuals(r2), pch = 20, col = "blue", main = "Residuos vs. valores ajustados", xlab = "Valores ajustados", ylab = "Residuos")
abline(h = 0, lty = 2, col = "red")

# Crear un gráfico Q-Q
qqnorm(residuals(r2), main = "Gráfico Q-Q")
qqline(residuals(r2), col = "red")

# Test shappiro-wilk normality del modelo
residuos_modelo2 <- residuals(r2)
shapiro.test(residuos_modelo2[1:5000])

# REGRESION 3 -------------------------------------------------------------

# Con las conclusiones modelo backwards
r3 <- lm(Confianza_consumidor ~ Year + MES + Variacion + 
           Sexo + Situacion_economia_hogar + Valoracion_sit_ec_hogar + 
           Personas_paro_entorno + Evolucion_paro_entorno_6m + Valoracion_encontrar_empleo + 
           Valoracion_sit_ec_espana + Valoracion_mejorar_empleo_esp_6m + 
           Valoracion_prospectiva_hogar + Valoracion_prospectiva_espana + 
           Valoracion_inflacion + Evolucion_tipo_interes + Evolucion_precio_vivienda + 
           Intencion_comprar_vivienda + Estado_civil + Tamano_hogar + 
           Ingresos_hogar + Adquisicion_mueble + Adquisicion_electrodomestico_grande + 
           Adquisicion_electrodomestico_pequeño + Composicion_hogar, 
         data = train_regresion)


r3 <- lm(Confianza_consumidor ~ Year + MES + Variacion + 
           Sexo + Situacion_economia_hogar + Valoracion_sit_ec_hogar + 
           Personas_paro_entorno + Evolucion_paro_entorno_6m + Valoracion_encontrar_empleo + 
           Valoracion_sit_ec_espana + Valoracion_mejorar_empleo_esp_6m + 
           Valoracion_prospectiva_hogar + Valoracion_prospectiva_espana + 
           Valoracion_inflacion + Evolucion_tipo_interes + Evolucion_precio_vivienda + 
           Intencion_comprar_vivienda + Estado_civil + Tamano_hogar + 
           Ingresos_hogar + Adquisicion_mueble + Adquisicion_electrodomestico_grande + 
           Adquisicion_electrodomestico_pequeño + Composicion_hogar, 
         data = train_regresion)

# Hacer predicciones sobre los datos de prueba
pred_r3 <- predict(r3, newdata = test_regresion)

# Calcular el coeficiente de determinación R-cuadrado
r_squared_r3 <- summary(r3)$r.squared
print(r_squared_r3)
# Calcular la raíz del error cuadrático medio (RMSE)
rmse_r3 <- sqrt(mean((test_regresion$Confianza_consumidor - pred_r3)^2))
print(rmse_r3)
# Calcular el error absoluto medio (MAE)
mae_r3 <- mean(abs(test_regresion$Confianza_consumidor - pred_r3))
print(mae_r3)
# Mucho mejor el rmse da 12

# Test shappiro-wilk normality del modelo
residuos_modelo3 <- residuals(r3)
shapiro.test(residuos_modelo3[1:5000])

# REGRESION 4 -------------------------------------------------------------

# Modelo regresion con modelo backwards sin 
r4 <- lm(Confianza_consumidor ~ Tamano_Habitat + Sexo + Edad + 
           Situacion_economia_hogar + Valoracion_sit_ec_hogar + Personas_paro_entorno + 
           Evolucion_paro_entorno_6m + Valoracion_encontrar_empleo + 
           Valoracion_sit_ec_espana + Valoracion_mejorar_empleo_esp_6m + 
           Valoracion_prospectiva_hogar + Valoracion_prospectiva_espana + 
           Valoracion_inflacion + Evolucion_tipo_interes + Evolucion_precio_vivienda + 
           Ideologia + Estado_civil + Tamano_hogar + Ingresos_hogar + 
           Situacion_laboral + Tenencia_vivienda + Adquisicion_mueble + 
           Adquisicion_electrodomestico_grande + Adquisicion_electrodomestico_pequeño, 
         data = train_sin)

# Hacer predicciones sobre los datos de prueba
pred_r4 <- predict(r4, newdata = test_sin)

# Calcular el coeficiente de determinación R-cuadrado
r_squared_r4 <- summary(r4)$r.squared
print(r_squared_r4)
# Calcular la raíz del error cuadrático medio (RMSE)
rmse_r4 <- sqrt(mean((test_sin$Confianza_consumidor - pred_r4)^2))
print(rmse_r4)
# Calcular el error absoluto medio (MAE)
mae_r4 <- mean(abs(test_sin$Confianza_consumidor - pred_r4))
print(mae_r4)
# VUELVE A SER 20, muy alto

# Test shappiro-wilk normality del modelo
residuos_modelo4 <- residuals(r4)
shapiro.test(residuos_modelo4[1:5000])

# REGRESION 5 -------------------------------------------------------------

# Con las variables del informe
r5 <- lm(Confianza_consumidor ~ Situacion_economia_hogar +
                     Valoracion_mejorar_empleo_esp_6m + Valoracion_sit_ec_espana + 
                     Valoracion_encontrar_empleo + Valoracion_prospectiva_hogar +
                     Valoracion_prospectiva_espana,
                   data = train_regresion)

summary(r5)

# Intervalo de confianza de los coeficientes
confint(r5)

# Hacer predicciones sobre los datos de prueba
pred_r5 <- predict(r5, newdata = test_regresion)

# Calcular el coeficiente de determinación R-cuadrado
r_squared_r5 <- summary(r5)$r.squared
print(r_squared_r5)
# Calcular la raíz del error cuadrático medio (RMSE)
rmse_r5 <- sqrt(mean((test_regresion$Confianza_consumidor - pred_r5)^2))
print(rmse_r5)
# Calcular el error absoluto medio (MAE)
mae_r5 <- mean(abs(test_regresion$Confianza_consumidor - pred_r5))
print(mae_r5)
# RMSE 19.78

# Test shappiro-wilk normality del modelo
residuos_modelo5 <- residuals(r5)
shapiro.test(residuos_modelo5[1:5000])

# RESIDUOS ----------------------------------------------------------------

# MODELO 1
plot(r1, which = 1) # gráfico de residuos estandarizados vs valores ajustados
plot(r1, which = 2) # gráfico de residuos estandarizados vs índice de observación
plot(r1, which = 3) # gráfico de cuantiles normales de los residuos estandarizados
plot(r1, which = 5) # gráfico de residuos vs variables explicativas

# library(car)
# # Calculamos los residuos estandarizados
# r1_1 <- residuals(r1) / sqrt(mean((residuals(r1))^2))
# # Gráfico de residuos estandarizados vs valores ajustados
# plot(r1_1, which = 1)
# # Identificamos los 5 puntos con los mayores residuos estandarizados
# mayores_residuos <- order(r1_1, decreasing = TRUE)[1:5]
# identif <- identify(r1_1, n = 5)

# MODELO 2
plot(r2, which = 1) # gráfico de residuos estandarizados vs valores ajustados
plot(r2, which = 2) # gráfico de residuos estandarizados vs índice de observación
plot(r2, which = 3) # gráfico de cuantiles normales de los residuos estandarizados
plot(r2, which = 5) # gráfico de residuos vs variables explicativas

# MODELO 3
plot(r3, which = 1) # gráfico de residuos estandarizados vs valores ajustados
plot(r3, which = 2) # gráfico de residuos estandarizados vs índice de observación
plot(r3, which = 3) # gráfico de cuantiles normales de los residuos estandarizados
plot(r3, which = 5) # gráfico de residuos vs variables explicativas

# MODELO 4
plot(r4, which = 1) # gráfico de residuos estandarizados vs valores ajustados
plot(r4, which = 2) # gráfico de residuos estandarizados vs índice de observación
plot(r4, which = 3) # gráfico de cuantiles normales de los residuos estandarizados
plot(r4, which = 5) # gráfico de residuos vs variables explicativas

# Test de Breusch-Pagan 
# para evaluar la homocedasticidad de los residuos:
library(lmtest)
bptest(r1)
bptest(r2)
bptest(r3)
bptest(r4)

# ERRORES -----------------------------------------------------------------

# MODELO 1
# Error estándar de la estimación
rse_modelo_1 <- summary(r1)$sigma
# R-cuadrado ajustado
rsq_adj_modelo_1 <- summary(r1)$adj.r.squared
# Residuos
residuos_modelo1 <- residuals(r1)
# Error absoluto medio (MAE)
MAE_modelo_1 <- mean(abs(residuos_modelo1))
MAE_modelo_5 <- mean(abs(residuos_modelo5))
# Error cuadrático medio (MSE)
MSE_modelo_1 <- mean(residuos_modelo1^2)
# Raíz del error cuadrático medio (RMSE)
RMSE_modelo_1 <- sqrt(mean(residuos_modelo1^2))

# MODELO 2
# Error estándar de la estimación
rse_modelo_2 <- summary(r2)$sigma
# R-cuadrado ajustado
rsq_adj_modelo_2 <- summary(r2)$adj.r.squared
# Residuos
residuos_modelo2 <- residuals(r2)
# Error absoluto medio (MAE)
MAE_modelo_2 <- mean(abs(residuos_modelo2))
# Error cuadrático medio (MSE)
MSE_modelo_2 <- mean(residuos_modelo2^2)
# Raíz del error cuadrático medio (RMSE)
RMSE_modelo_2 <- sqrt(mean(residuos_modelo2^2))

# MODELO 3
# Error estándar de la estimación
rse_modelo_3 <- summary(r3)$sigma
# R-cuadrado ajustado
rsq_adj_modelo_3 <- summary(r3)$adj.r.squared
# Residuos
residuos_modelo3 <- residuals(r3)
# Error absoluto medio (MAE)
MAE_modelo_3 <- mean(abs(residuos_modelo3))
# Error cuadrático medio (MSE)
MSE_modelo_3 <- mean(residuos_modelo3^2)
# Raíz del error cuadrático medio (RMSE)
RMSE_modelo_3 <- sqrt(mean(residuos_modelo3^2))

# MODELO 4
# Error estándar de la estimación
rse_modelo_4 <- summary(r4)$sigma
# R-cuadrado ajustado
rsq_adj_modelo_4 <- summary(r4)$adj.r.squared
# Residuos
residuos_modelo4 <- residuals(r4)
# Error absoluto medio (MAE)
MAE_modelo_4 <- mean(abs(residuos_modelo4))
# Error cuadrático medio (MSE)
MSE_modelo_4 <- mean(residuos_modelo4^2)
# Raíz del error cuadrático medio (RMSE)
RMSE_modelo_4 <- sqrt(mean(residuos_modelo4^2))

# MODELO 5
# Error estándar de la estimación
rse_modelo_5 <- summary(r5)$sigma
# R-cuadrado ajustado
rsq_adj_modelo_5 <- summary(r5)$adj.r.squared
# Residuos
residuos_modelo5 <- residuals(r5)
# Error absoluto medio (MAE)
MAE_modelo_5 <- mean(abs(residuos_modelo5))
# Error cuadrático medio (MSE)
MSE_modelo_5 <- mean(residuos_modelo5^2)
# Raíz del error cuadrático medio (RMSE)
RMSE_modelo_5 <- sqrt(mean(residuos_modelo5^2))


# TABLA ERRORES
# Crear un data frame vacío para almacenar los errores
tabla_errores_regresion <- data.frame(matrix(ncol = 5, nrow = 5))

# Asignar nombres de columnas
colnames(tabla_errores_regresion) <- c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4", "Modelo5")
rownames(tabla_errores_regresion) <- c("RSE", "R-cuadrado ajustado", "MAE", "MSE", "RMSE")

# Rellenar el data frame con los errores de cada modelo  
tabla_errores_regresion[1,] <- c(rse_modelo_1, rse_modelo_2, rse_modelo_3, rse_modelo_4, rse_modelo_5)
tabla_errores_regresion[2,] <- c(rsq_adj_modelo_1, rsq_adj_modelo_2, rsq_adj_modelo_3, rsq_adj_modelo_4, rsq_adj_modelo_5)
tabla_errores_regresion[3,] <- c(MAE_modelo_1, MAE_modelo_2, MAE_modelo_3, MAE_modelo_4, MAE_modelo_5)
tabla_errores_regresion[4,] <- c(MSE_modelo_1, MSE_modelo_2, MSE_modelo_3, MSE_modelo_4, MSE_modelo_5)
tabla_errores_regresion[5,] <- c(RMSE_modelo_1, RMSE_modelo_2, RMSE_modelo_3, RMSE_modelo_4, RMSE_modelo_5)

# Imprimir el data frame
view(tabla_errores_regresion)


# PRUEBAS MODELO REGRESION  -----------------------------------------------
# Carga de librerías necesarias
library(stats)

# Creación del modelo inicial de regresión lineal
modelo <- lm(Confianza_consumidor ~ ., data = df_completo)
# Aplicación de la función step() para selección de variables
modelo_seleccionado <- step(modelo)
# Variables más importantes según el modelo seleccionado
variables_importantes <- names(coef(modelo_seleccionado))
print(variables_importantes)

# Ajustar el modelo de regresión lineal
lm_model <- lm(Confianza_consumidor ~ Year + MES + Variacion + Sexo + Edad +
                 Situacion_economia_hogar + Valoracion_sit_ec_hogar +
                 Personas_paro_entorno + Evolucion_paro_entorno_6m +
                 Valoracion_encontrar_empleo + Valoracion_sit_ec_espana +
                 Valoracion_mejorar_empleo_esp_6m + Valoracion_prospectiva_espana +
                 Valoracion_inflacion + Evolucion_tipo_interes +
                 Evolucion_precio_vivienda + Intencion_comprar_vivienda +
                 Estado_civil + Tamano_hogar + Ingresos_hogar +
                 Adquisicion_mueble + Adquisicion_electrodomestico_grande +
                 Adquisicion_electrodomestico_pequeño + Composicion_hogar,
               data = df_completo)

# Imprimir los resultados del modelo
summary(lm_model)
# Realizar predicciones en el conjunto de prueba
predict1 <- predict(lm_model, newdata = test)

# Calcular las métricas de error
errors1 <- predict1 - test$Confianza_consumidor
mse1 <- mean(errors1^2)  # Error cuadrático medio
rmse1 <- sqrt(mse1)  # Raíz del error cuadrático medio
mae1 <- mean(abs(errors1))  # Error absoluto medio

# Mostrar las métricas de error
cat("Error cuadrático medio (MSE):", mse1, "\n")
cat("Raíz del error cuadrático medio (RMSE):", rmse1, "\n")
cat("Error absoluto medio (MAE):", mae1, "\n")

# Sin variacion ni year ni mes
# Creación del modelo inicial de regresión lineal
modelo2 <- lm(Confianza_consumidor ~ .-Year -MES -Variacion, data = df_completo)
# Aplicación de la función step() para selección de variables
modelo_seleccionado2 <- step(modelo2)
# Variables más importantes según el modelo seleccionado
variables_importantes2 <- names(coef(modelo_seleccionado2))
print(variables_importantes2)

# MODELO REGRESION CON STEP -----------------------------------------------

# REDUCIDO
train_reducido <- df_reducido %>%
  filter(Year %in% c(2018,2019, 2020, 2021))
# TEST
# ES 2022
test_reducido <- df_reducido %>%
  filter(Year == 2022)

# MODELO
# Cargar el paquete de regresión lineal
library(stats)

# Crear el modelo de regresión lineal
rmodel1 <- lm(Confianza_consumidor ~ Edad + Situacion_economia_hogar + Valoracion_sit_ec_hogar +
                Personas_paro_entorno + Evolucion_paro_entorno_6m + Valoracion_encontrar_empleo +
                Valoracion_sit_ec_espana + Valoracion_mejorar_empleo_esp_6m + Evolucion_ahorro_personal +
                Valoracion_prospectiva_espana + Valoracion_inflacion + Evolucion_tipo_interes +
                Evolucion_precio_vivienda + Estado_civil + Ingresos_hogar + Situacion_laboral +
                Tenencia_vivienda + Adquisicion_mueble + Adquisicion_electrodomestico_grande +
                Adquisicion_electrodomestico_pequeño + Composicion_hogar,
              data = train)

# Realizar predicciones en el conjunto de prueba
predictions <- predict(rmodel1, newdata = test)

# Calcular las métricas de error
errors <- predictions - test$Confianza_consumidor
mse <- mean(errors^2)  # Error cuadrático medio
rmse <- sqrt(mse)  # Raíz del error cuadrático medio
mae <- mean(abs(errors))  # Error absoluto medio

# Mostrar las métricas de error
cat("Error cuadrático medio (MSE):", mse, "\n")
cat("Raíz del error cuadrático medio (RMSE):", rmse, "\n")
cat("Error absoluto medio (MAE):", mae, "\n")

# Crear el modelo de regresión lineal 
# CON MES YEAR Y VARIACION
rmodel2 <- lm(Confianza_consumidor ~ Year + MES + Variacion + Comunidad + Sexo +
                Provincia + Situacion_economia_hogar + Valoracion_sit_ec_hogar +
                Personas_paro_entorno + Evolucion_paro_entorno_6m +
                Valoracion_encontrar_empleo + Valoracion_sit_ec_espana +
                Valoracion_mejorar_empleo_esp_6m + Valoracion_prospectiva_espana +
                Valoracion_inflacion + Evolucion_tipo_interes +
                Evolucion_precio_vivienda + Intencion_comprar_vivienda +
                Estado_civil + Tamano_hogar + Ingresos_hogar +
                Adquisicion_electrodomestico_grande + Adquisicion_electrodomestico_pequeño +
                Composicion_hogar,
              data = train)

# Realizar predicciones en el conjunto de prueba
predictions2 <- predict(rmodel2, newdata = test)

# Calcular las métricas de error
errors2 <- predictions2 - test$Confianza_consumidor
mse2 <- mean(errors2^2)  # Error cuadrático medio
rmse2 <- sqrt(mse2)  # Raíz del error cuadrático medio
mae2 <- mean(abs(errors2))  # Error absoluto medio

# Mostrar las métricas de error
cat("Error cuadrático medio (MSE):", mse2, "\n")
cat("Raíz del error cuadrático medio (RMSE):", rmse2, "\n")
cat("Error absoluto medio (MAE):", mae2, "\n")


# PRUEBA CON VARIABLES DEL INFORME
rmodel3 <- lm(Confianza_consumidor ~ Situacion_economia_hogar +
           Valoracion_mejorar_empleo_esp_6m + Valoracion_sit_ec_espana + 
           Valoracion_encontrar_empleo + Valoracion_prospectiva_hogar +
           Valoracion_prospectiva_espana,
         data = train)

# Realizar predicciones en el conjunto de prueba
predictions3 <- predict(rmodel3, newdata = test)

# Calcular las métricas de error
errors3 <- predictions3 - test$Confianza_consumidor
mse3 <- mean(errors3^2)  # Error cuadrático medio
rmse3 <- sqrt(mse3)  # Raíz del error cuadrático medio
mae3 <- mean(abs(errors3))  # Error absoluto medio

# Mostrar las métricas de error
cat("Error cuadrático medio (MSE):", mse3, "\n")
cat("Raíz del error cuadrático medio (RMSE):", rmse3, "\n")
cat("Error absoluto medio (MAE):", mae3, "\n")

