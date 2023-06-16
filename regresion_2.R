
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


# CARGA DE DATOS ----------------------------------------------------------
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

# PREVIO ---------------------------------------------------------------
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



# MODELO 1 ----------------------------------------------------------------

# Configurar la semilla para reproducibilidad
set.seed(123)

# Crear un modelo de regresión
lm_model <- lm(Confianza_consumidor ~ ., data = train_regresion)

# Realizar predicciones en los datos de prueba
predictions <- predict(lm_model, newdata = test_regresion)

# Calcular los errores
rmse <- sqrt(mean((test_regresion$Confianza_consumidor - predictions)^2))
mae <- mean(abs(test_regresion$Confianza_consumidor - predictions))

# Mostrar los resultados
cat("RMSE:", rmse, "\n")
cat("MAE:", mae, "\n")

# Gráfico de comparación entre los valores reales y las predicciones
comparison <- data.frame(Confianza_Real = test_regresion$Confianza_consumidor, Predicciones = predictions)
ggplot(comparison, aes(x = Confianza_Real, y = Predicciones)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(x = "Confianza Real", y = "Predicciones", title = "Comparación: Confianza Real vs. Predicciones") +
  theme_minimal()

# Gráfico de residuos
residuals <- test_regresion$Confianza_consumidor - predictions
ggplot() +
  geom_histogram(aes(x = residuals), bins = 30, fill = "lightblue", color = "black") +
  labs(x = "Residuos", y = "Frecuencia", title = "Distribución de Residuos") +
  theme_minimal()

# Gráfico de QQ plot para verificar la normalidad de los residuos
qqplot(residuals, main = "QQ Plot: Residuos")

# Gráfico de dispersión de los valores predichos vs. residuos
scatter <- data.frame(Predicciones = predictions, Residuos = residuals)
ggplot(scatter, aes(x = Predicciones, y = Residuos)) +
  geom_point() +
  labs(x = "Predicciones", y = "Residuos", title = "Dispersión: Predicciones vs. Residuos") +
  theme_minimal()

# Gráfico de línea de referencia en los residuos
ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_line(aes(x = 1:length(residuals), y = residuals), color = "blue") +
  labs(x = "Observaciones", y = "Residuos", title = "Residuos a lo largo de las observaciones") +
  theme_minimal()


# MODELO REGRESION 1 ------------------------------------------------------

## Ajuste del modelo de regresión lineal simple con train
modelo_reg_prueba <- lm(Confianza_consumidor ~ ., data=train_regresion)

# Comprobar el patrón lineal de los residuos
#plot(modelo, which = 1)
plot(modelo_reg_prueba, which = 1)

# Comprobar la homocedasticidad de los residuos
bartlett.test(resid(modelo_reg_prueba) ~ train_regresion$Confianza_consumidor)

# Comprobar la normalidad de los residuos
residuos_modelo2 <- residuals(modelo_reg_prueba)
shapiro.test(residuos_modelo2[1:5000])

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

# Realizar la prueba de Anderson-Darling
library(nortest)
ad_test <- ad.test(residuos)
print(ad_test)

## Predicciones con test
predicciones_reg_prueba <- predict(modelo_reg_prueba, newdata=test_regresion)
print(predicciones_reg_prueba)
plot(predicciones_reg_prueba)

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

# Test de Breusch-Pagan 
# para evaluar la homocedasticidad de los residuos:
bptest(modelo_reg_prueba)

tabla_predicciones_regresion <- data.frame(MES = test_regresion$MES, Year = test_regresion$Year,
                                               Valor_Real = test_regresion$Confianza_consumidor,
                                               Prediccion = predicciones_reg_prueba)
print(tabla_predicciones_regresion)
table_result_regresion <- aggregate(cbind(Valor_Real, Prediccion) ~ MES + Year, data = tabla_predicciones_regresion, mean)
print(table_result_regresion)
view(table_result_regresion)

# MODELO REGRESION 2 ------------------------------------------------------

# VARIABLES INFORME
## Ajuste del modelo de regresión lineal simple con train
modelo_reg_prueba2 <- lm(Confianza_consumidor ~ Situacion_economia_hogar +
                          Valoracion_mejorar_empleo_esp_6m + Valoracion_sit_ec_espana + 
                          Valoracion_encontrar_empleo + Valoracion_prospectiva_hogar +
                          Valoracion_prospectiva_espana, data=train_regresion)

# Comprobar el patrón lineal de los residuos
#plot(modelo, which = 1)
plot(modelo_reg_prueba2, which = 1)

# Comprobar la homocedasticidad de los residuos
bartlett.test(resid(modelo_reg_prueba2) ~ train_regresion$Confianza_consumidor)

# Comprobar la normalidad de los residuos
residuos_modelo2 <- residuals(modelo_reg_prueba2)
shapiro.test(residuos_modelo2[1:5000])

# Comprobar las hipótesis sobre las que se construye el modelo
# Hipótesis de homocedasticidad
residuos <- resid(modelo_reg_prueba2)
residuos_estandarizados <- rstandard(modelo_reg_prueba2)
print(residuos)
print(residuos_estandarizados)
plot(fitted(modelo_reg_prueba2), residuos_estandarizados, main = "Gráfico de residuos estandarizados vs valores ajustados")

#Normalidad de los residuos
abline(h = 0)
hist(residuos_estandarizados, main = "Histograma de residuos estandarizados")
qqnorm(residuos_estandarizados, main = "Gráfico QQ de residuos estandarizados")
qqline(residuos_estandarizados)

#Patrón lineal de los residuos
plot(fitted(modelo_reg_prueba2), residuos, main = "Gráfico de residuos vs valores ajustados") +
  abline(h = 0)

# Realizar la prueba de Anderson-Darling
library(nortest)
ad_test <- ad.test(residuos)
print(ad_test)

## Predicciones con test
predicciones_reg_prueba2 <- predict(modelo_reg_prueba2, newdata=test_regresion)
print(predicciones_reg_prueba2)
plot(predicciones_reg_prueba2)

# Evaluación del modelo
MSE <- mean((predicciones_reg_prueba2 - test_regresion$Confianza_consumidor)^2)
print(MSE)
RMSE <- sqrt(MSE)
print(RMSE)
r_cuadrado <- summary(modelo_reg_prueba2)$r.squared
print(r_cuadrado)
MAPE <- mean(abs((test_regresion$Confianza_consumidor - predicciones_reg_prueba2)/test_regresion$Confianza_consumidor))*100
print(MAPE)
MAE <- mean(abs(test_regresion$Confianza_consumidor - predicciones_reg_prueba2))
print(MAE)

#CON ESTO SUBE MUCHO RMSE DE 19.78


# MODELO REGRESION 3 ------------------------------------------------------

# VARIABLES INFORME
## Ajuste del modelo de regresión lineal simple con train
# Sin año mes y variacion
modelo_reg_prueba3 <- lm(Confianza_consumidor ~ .-Year -MES - Variacion, data=train_regresion)

# Comprobar el patrón lineal de los residuos
#plot(modelo, which = 1)
plot(modelo_reg_prueba3, which = 1)

# Comprobar la homocedasticidad de los residuos
bartlett.test(resid(modelo_reg_prueba3) ~ train_regresion$Confianza_consumidor)

# Comprobar la normalidad de los residuos
residuos_modelo3 <- residuals(modelo_reg_prueba3)
shapiro.test(residuos_modelo3[1:5000])

# Comprobar las hipótesis sobre las que se construye el modelo
# Hipótesis de homocedasticidad
residuos <- resid(modelo_reg_prueba3)
residuos_estandarizados <- rstandard(modelo_reg_prueba3)
print(residuos)
print(residuos_estandarizados)
plot(fitted(modelo_reg_prueba3), residuos_estandarizados, main = "Gráfico de residuos estandarizados vs valores ajustados")

#Normalidad de los residuos
abline(h = 0)
hist(residuos_estandarizados, main = "Histograma de residuos estandarizados")
qqnorm(residuos_estandarizados, main = "Gráfico QQ de residuos estandarizados")
qqline(residuos_estandarizados)

#Patrón lineal de los residuos
plot(fitted(modelo_reg_prueba3), residuos, main = "Gráfico de residuos vs valores ajustados") +
  abline(h = 0)

# Realizar la prueba de Anderson-Darling
library(nortest)
ad_test <- ad.test(residuos)
print(ad_test)

## Predicciones con test
predicciones_reg_prueba3 <- predict(modelo_reg_prueba3, newdata=test_regresion)
print(predicciones_reg_prueba3)
plot(predicciones_reg_prueba3)

# Evaluación del modelo
MSE <- mean((predicciones_reg_prueba3 - test_regresion$Confianza_consumidor)^2)
print(MSE)
RMSE <- sqrt(MSE)
print(RMSE)
r_cuadrado <- summary(modelo_reg_prueba3)$r.squared
print(r_cuadrado)
MAPE <- mean(abs((test_regresion$Confianza_consumidor - predicciones_reg_prueba3)/test_regresion$Confianza_consumidor))*100
print(MAPE)
MAE <- mean(abs(test_regresion$Confianza_consumidor - predicciones_reg_prueba3))
print(MAE)

#CON ESTO SUBE MUCHO RMSE DE 19.78

# MODELO REGRESION 4 ------------------------------------------------------

## Ajuste del modelo de regresión lineal simple con train
modelo_reg_prueba4 <- lm(Confianza_consumidor ~ Year + Valoracion_sit_ec_espana +
                           Evolucion_precio_vivienda + Valoracion_encontrar_empleo +
                           Valoracion_prospectiva_espana + Valoracion_sit_ec_hogar +
                           Valoracion_mejorar_empleo_esp_6m + MES + Variacion +
                           Evolucion_ahorro_personal, data=train_regresion)

# Comprobar el patrón lineal de los residuos
#plot(modelo, which = 1)
plot(modelo_reg_prueba4, which = 1)

# Comprobar la homocedasticidad de los residuos
bartlett.test(resid(modelo_reg_prueba4) ~ train_regresion$Confianza_consumidor)

# Comprobar la normalidad de los residuos
residuos_modelo4 <- residuals(modelo_reg_prueba4)
shapiro.test(residuos_modelo4[1:5000])

# Comprobar las hipótesis sobre las que se construye el modelo
# Hipótesis de homocedasticidad
residuos <- resid(modelo_reg_prueba4)
residuos_estandarizados <- rstandard(modelo_reg_prueba4)
print(residuos)
print(residuos_estandarizados)
plot(fitted(modelo_reg_prueba4), residuos_estandarizados, main = "Gráfico de residuos estandarizados vs valores ajustados")

#Normalidad de los residuos
abline(h = 0)
hist(residuos_estandarizados, main = "Histograma de residuos estandarizados")
qqnorm(residuos_estandarizados, main = "Gráfico QQ de residuos estandarizados")
qqline(residuos_estandarizados)

#Patrón lineal de los residuos
plot(fitted(modelo_reg_prueba4), residuos, main = "Gráfico de residuos vs valores ajustados") +
  abline(h = 0)

## Predicciones con test
predicciones_reg_prueba4 <- predict(modelo_reg_prueba4, newdata=test_regresion)
print(predicciones_reg_prueba4)
plot(predicciones_reg_prueba4)

# Realizar la prueba de Anderson-Darling
library(nortest)
ad_test <- ad.test(residuos)
print(ad_test)

# Evaluación del modelo
MSE <- mean((predicciones_reg_prueba4 - test_regresion$Confianza_consumidor)^2)
print(MSE)
RMSE <- sqrt(MSE)
print(RMSE)
r_cuadrado <- summary(modelo_reg_prueba4)$r.squared
print(r_cuadrado)
MAPE <- mean(abs((test_regresion$Confianza_consumidor - predicciones_reg_prueba4)/test_regresion$Confianza_consumidor))*100
print(MAPE)
MAE <- mean(abs(test_regresion$Confianza_consumidor - predicciones_reg_prueba4))
print(MAE)

# VUELVE A REDUCIR BASTANTE 12.31


# MODELO REGRESION 5 STEP -------------------------------------------------

# MODELO STEP
# Carga de librerías necesarias
library(stats)

# Creación del modelo inicial de regresión lineal
modelo_step <- lm(Confianza_consumidor ~ ., data = df_completo)
# Aplicación de la función step() para selección de variables
modelo_seleccionado <- step(modelo_step)
# Variables más importantes según el modelo seleccionado
variables_importantes <- names(coef(modelo_seleccionado))
print(variables_importantes)

# step en train 
# Creación del modelo inicial de regresión lineal
modelo_step <- lm(Confianza_consumidor ~ ., data = train_regresion)
# Aplicación de la función step() para selección de variables
modelo_seleccionado <- step(modelo_step)
# Variables más importantes según el modelo seleccionado
variables_importantes <- names(coef(modelo_seleccionado))
print(variables_importantes)

## Ajuste del modelo de regresión lineal simple con train
modelo_reg_prueba5 <- lm(Confianza_consumidor ~ Year + MES + Variacion + Sexo + Edad +
                           Situacion_economia_hogar + Valoracion_sit_ec_hogar +
                           Personas_paro_entorno + Evolucion_paro_entorno_6m +
                           Valoracion_encontrar_empleo + Valoracion_sit_ec_espana +
                           Valoracion_mejorar_empleo_esp_6m + Valoracion_prospectiva_espana +
                           Valoracion_inflacion + Evolucion_tipo_interes +
                           Evolucion_precio_vivienda + Intencion_comprar_vivienda +
                           Estado_civil + Tamano_hogar + Ingresos_hogar +
                           Adquisicion_mueble + Adquisicion_electrodomestico_grande +
                           Adquisicion_electrodomestico_pequeño + Composicion_hogar, 
                         data=train_regresion)

# Comprobar el patrón lineal de los residuos
#plot(modelo, which = 1)
plot(modelo_reg_prueba5, which = 1)

# Comprobar la homocedasticidad de los residuos
bartlett.test(resid(modelo_reg_prueba5) ~ train_regresion$Confianza_consumidor)

# Comprobar la normalidad de los residuos
residuos_modelo5 <- residuals(modelo_reg_prueba5)
shapiro.test(residuos_modelo5[1:5000])

# Comprobar las hipótesis sobre las que se construye el modelo
# Hipótesis de homocedasticidad
residuos <- resid(modelo_reg_prueba5)
residuos_estandarizados <- rstandard(modelo_reg_prueba5)
print(residuos)
print(residuos_estandarizados)
plot(fitted(modelo_reg_prueba5), residuos_estandarizados, main = "Gráfico de residuos estandarizados vs valores ajustados")

#Normalidad de los residuos
abline(h = 0)
hist(residuos_estandarizados, main = "Histograma de residuos estandarizados")
qqnorm(residuos_estandarizados, main = "Gráfico QQ de residuos estandarizados")
qqline(residuos_estandarizados)

#Patrón lineal de los residuos
plot(fitted(modelo_reg_prueba5), residuos, main = "Gráfico de residuos vs valores ajustados") +
  abline(h = 0)

## Predicciones con test
predicciones_reg_prueba5 <- predict(modelo_reg_prueba5, newdata=test_regresion)
print(predicciones_reg_prueba5)
plot(predicciones_reg_prueba5)

# Realizar la prueba de Anderson-Darling
library(nortest)
ad_test <- ad.test(residuos)
print(ad_test)

# Evaluación del modelo
MSE <- mean((predicciones_reg_prueba5 - test_regresion$Confianza_consumidor)^2)
print(MSE)
RMSE <- sqrt(MSE)
print(RMSE)
r_cuadrado <- summary(modelo_reg_prueba5)$r.squared
print(r_cuadrado)
MAPE <- mean(abs((test_regresion$Confianza_consumidor - predicciones_reg_prueba5)/test_regresion$Confianza_consumidor))*100
print(MAPE)
MAE <- mean(abs(test_regresion$Confianza_consumidor - predicciones_reg_prueba5))
print(MAE)
# VUELVE A SER UN RMSE EN LOS MISMOS VALORES, 12,48

# TABLA DE ERRORES REGRESION ----------------------------------------------
# MODELO 1
# Error estándar de la estimación
rse_modelo_1 <- summary(modelo_reg_prueba)$sigma
# R-cuadrado ajustado
rsq_adj_modelo_1 <- summary(modelo_reg_prueba)$adj.r.squared
# Residuos
residuos_modelo1 <- residuals(modelo_reg_prueba)
# Error absoluto medio (MAE)
MAE_modelo_1 <- mean(abs(test_regresion$Confianza_consumidor - predicciones_reg_prueba))
# Error cuadrático medio (MSE)
MSE_modelo_1 <- mean((predicciones_reg_prueba - test_regresion$Confianza_consumidor)^2)
# Raíz del error cuadrático medio (RMSE)
RMSE_modelo_1 <- sqrt(MSE_modelo_1)
#MAPE
MAPE_modelo_1 <- mean(abs((test_regresion$Confianza_consumidor - predicciones_reg_prueba)/test_regresion$Confianza_consumidor))*100

# MODELO 2
rse_modelo_2 <- summary(modelo_reg_prueba2)$sigma
# R-cuadrado ajustado
rsq_adj_modelo_2 <- summary(modelo_reg_prueba2)$adj.r.squared
# Residuos
residuos_modelo2 <- residuals(modelo_reg_prueba2)
# Error absoluto medio (MAE)
MAE_modelo_2 <- mean(abs(test_regresion$Confianza_consumidor - predicciones_reg_prueba2))
# Error cuadrático medio (MSE)
MSE_modelo_2 <- mean((predicciones_reg_prueba2 - test_regresion$Confianza_consumidor)^2)
# Raíz del error cuadrático medio (RMSE)
RMSE_modelo_2 <- sqrt(MSE_modelo_2)
#MAPE
MAPE_modelo_2 <- mean(abs((test_regresion$Confianza_consumidor - predicciones_reg_prueba2)/test_regresion$Confianza_consumidor))*100

# MODELO 3
rse_modelo_3 <- summary(modelo_reg_prueba3)$sigma
# R-cuadrado ajustado
rsq_adj_modelo_3 <- summary(modelo_reg_prueba3)$adj.r.squared
# Residuos
residuos_modelo3 <- residuals(modelo_reg_prueba3)
# Error absoluto medio (MAE)
MAE_modelo_3 <- mean(abs(test_regresion$Confianza_consumidor - predicciones_reg_prueba3))
# Error cuadrático medio (MSE)
MSE_modelo_3 <- mean((predicciones_reg_prueba3 - test_regresion$Confianza_consumidor)^2)
# Raíz del error cuadrático medio (RMSE)
RMSE_modelo_3 <- sqrt(MSE_modelo_3)
#MAPE
MAPE_modelo_3 <- mean(abs((test_regresion$Confianza_consumidor - predicciones_reg_prueba3)/test_regresion$Confianza_consumidor))*100

# MODELO 4
rse_modelo_4 <- summary(modelo_reg_prueba4)$sigma
# R-cuadrado ajustado
rsq_adj_modelo_4 <- summary(modelo_reg_prueba4)$adj.r.squared
# Residuos
residuos_modelo4 <- residuals(modelo_reg_prueba4)
# Error absoluto medio (MAE)
MAE_modelo_4 <- mean(abs(test_regresion$Confianza_consumidor - predicciones_reg_prueba4))
# Error cuadrático medio (MSE)
MSE_modelo_4 <- mean((predicciones_reg_prueba4 - test_regresion$Confianza_consumidor)^2)
# Raíz del error cuadrático medio (RMSE)
RMSE_modelo_4 <- sqrt(MSE_modelo_4)
#MAPE
MAPE_modelo_4 <- mean(abs((test_regresion$Confianza_consumidor - predicciones_reg_prueba4)/test_regresion$Confianza_consumidor))*100


# MODELO 5
# MODELO 5
rse_modelo_5 <- summary(modelo_reg_prueba5)$sigma
# R-cuadrado ajustado
rsq_adj_modelo_5 <- summary(modelo_reg_prueba5)$adj.r.squared
# Residuos
residuos_modelo5 <- residuals(modelo_reg_prueba5)
# Error absoluto medio (MAE)
MAE_modelo_5 <- mean(abs(test_regresion$Confianza_consumidor - predicciones_reg_prueba5))
# Error cuadrático medio (MSE)
MSE_modelo_5 <- mean((predicciones_reg_prueba5 - test_regresion$Confianza_consumidor)^2)
# Raíz del error cuadrático medio (RMSE)
RMSE_modelo_5 <- sqrt(MSE_modelo_5)
#MAPE
MAPE_modelo_5 <- mean(abs((test_regresion$Confianza_consumidor - predicciones_reg_prueba5)/test_regresion$Confianza_consumidor))*100

# TABLA ERRORES
# Crear un data frame vacío para almacenar los errores
tabla_errores_regresion <- data.frame(matrix(ncol = 5, nrow = 5))

# Asignar nombres de columnas
colnames(tabla_errores_regresion) <- c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4", "Modelo5")
rownames(tabla_errores_regresion) <- c("RSE", "R-cuadrado ajustado", "MAE", "MAPE", "RMSE")

# Rellenar el data frame con los errores de cada modelo  
tabla_errores_regresion[1,] <- c(rse_modelo_1, rse_modelo_2, rse_modelo_3, rse_modelo_4, rse_modelo_5)
tabla_errores_regresion[2,] <- c(rsq_adj_modelo_1, rsq_adj_modelo_2, rsq_adj_modelo_3, rsq_adj_modelo_4, rsq_adj_modelo_5)
tabla_errores_regresion[3,] <- c(MAE_modelo_1, MAE_modelo_2, MAE_modelo_3, MAE_modelo_4, MAE_modelo_5)
tabla_errores_regresion[4,] <- c(MAPE_modelo_1, MAPE_modelo_2, MAPE_modelo_3, MAPE_modelo_4, MAPE_modelo_5)
tabla_errores_regresion[5,] <- c(RMSE_modelo_1, RMSE_modelo_2, RMSE_modelo_3, RMSE_modelo_4, RMSE_modelo_5)

# Imprimir el data frame
view(tabla_errores_regresion)
print(tabla_errores_regresion)

# Test de Breusch-Pagan 
# para evaluar la homocedasticidad de los residuos:
library(lmtest)
bptest(modelo_reg_prueba)
bptest(modelo_reg_prueba2)
bptest(modelo_reg_prueba3)
bptest(modelo_reg_prueba4)
bptest(modelo_reg_prueba5)

# HETEROCEDASTICIDAD
# Cargar el paquete sandwich para acceder a la función vcovHC()
library(sandwich)
# Obtener los residuos del modelo de regresión (modelo_regresion es tu modelo de regresión)
residuos <- residuals(modelo_reg_prueba)

# Obtener la matriz de covarianza robusta de los coeficientes
vcov <- vcovHC(modelo_reg_prueba)

# Realizar el test de White
white_test <- coeftest(modelo_reg_prueba, vcov = vcov)

# Imprimir los resultados
print(white_test)

# STEP HELEN --------------------------------------------------------------

# * en este modelo se eliminar las variables que no aportan al modelo

# Seleccion de los mejores predictores
# Creación del modelo inicial de regresión lineal
modelo_step <- lm(Confianza_consumidor ~ ., data = df_completo)
# Aplicación de la función step() para selección de variables
modelo_seleccionado <- step(object = modelo_step, direction = "both", trace = 1)
# Variables más importantes según el modelo seleccionado
variables_importantes <- names(coef(modelo_seleccionado))
print(variables_importantes)

## vemos que hay que eliminar algunas variables que no aportan al modelo y queda el siguiente:

# estas son las variables significativas que quedan del modelo 


modelosinvar<- lm(Confianza_consumidor ~ Year + MES + Variacion + Comunidad + Sexo + Edad +
                    Situacion_economia_hogar + Valoracion_sit_ec_hogar +
                    Personas_paro_entorno + Evolucion_paro_entorno_6m +
                    Valoracion_encontrar_empleo + Valoracion_sit_ec_espana + Valoracion_prospectiva_hogar +
                    Valoracion_mejorar_empleo_esp_6m + Valoracion_prospectiva_espana +
                    Valoracion_inflacion + Evolucion_tipo_interes +
                    Evolucion_precio_vivienda + Intencion_comprar_vivienda +
                    Estado_civil + Tamano_hogar + Ingresos_hogar +
                    Adquisicion_mueble + Adquisicion_electrodomestico_grande +
                    Adquisicion_electrodomestico_pequeño + Composicion_hogar, 
                  data=train_regresion)

summary(modelosinvar)$coefficients
summary(modelosinvar)

# METRICAS DE VALORACION DEL MODELO

# Obtener el coeficiente de determinación (R-cuadrado)
r_squared <- summary(modelosinvar)$r.squared

# Obtener el error estándar de la estimación (SEE)
see <- summary(modelosinvar)$sigma

# Realizar la prueba de significancia de los coeficientes
coef_test <- coef(summary(modelosinvar))

# Obtener los p-valores de los coeficientes individuales
p_values <- coef_test[, "Pr(>|t|)"]
# Obtener el valor p de la prueba F (para evaluar la significancia conjunta de los coeficientes)
f_p_value <- coef(summary(modelosinvar))["Residuals", "Pr(>F)"]
# Obtener el valor p de la prueba F (para evaluar la significancia conjunta de los coeficientes)
f_p_value <- coef_test["Residuals", "Pr(>|t|)"]

# Imprimir los resultados
cat("Coeficiente de determinación (R-cuadrado):", r_squared, "\n")
cat("Error estándar de la estimación (SEE):", see, "\n")
cat("P-valores de los coeficientes:\n")
print(p_values)
cat("Valor p de la prueba F:", f_p_value, "\n")



# Crear un data frame con las observaciones y los valores ajustados del modelo
datos_grafico <- data.frame(Observado = modelosinvar$model$Confianza_consumidor,
                            Ajustado = predict(modelosinvar))

# Crear un gráfico de dispersión con los valores observados y los valores ajustados
ggplot(datos_grafico, aes(x = Observado, y = Ajustado)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(x = "Valores observados", y = "Valores ajustados", title = "Valores observados vs. Valores ajustados")

# Realizar predicciones con el modelo
predicciones2 <- predict(modelosinvar, newdata = test_regresion)

# Calcular los residuos
residuos2 <- test_regresion$Confianza_consumidor - predicciones2

# Calcular el RMSE
rmse2 <- sqrt(mean(residuos2^2))

rmse2  # Mostrar el valor del RMSE


# PRUEBA DE NORMALIDAD DE LOS RESIDUOS
# Calcular los residuos
residuos <- resid(modelosinvar)
residuos_invar <- residuals(modelosinvar)
shapiro.test(residuos_invar[1:5000])

# Realizar la prueba de normalidad de Shapiro-Wilk
resultado_normalidad <- shapiro.test(residuos_invar)

# Imprimir el resultado
print(resultado_normalidad)

# Crear un histograma de los residuos
hist(residuos, breaks = 10, col = "steelblue", xlab = "Residuos", main = "Histograma de los residuos")
# Crear un gráfico QQ de los residuos
qqnorm(residuos, col = "steelblue", main = "Gráfico de probabilidad normal (QQ-plot)")
qqline(residuos, col = "red")


# PRUEBA DE AUTOCORRELACION DE LOS RESIDUOS
# Calcular los residuos
residuos <- resid(modelosinvar)
# Calcular y graficar la función de autocorrelación de los residuos
acf(residuos)

# PREDICCION A FUTURO -----------------------------------------------------

modelosinvar<- lm(Confianza_consumidor ~ Year + MES + Variacion + Comunidad + Sexo + Edad +
                    Situacion_economia_hogar + Valoracion_sit_ec_hogar +
                    Personas_paro_entorno + Evolucion_paro_entorno_6m +
                    Valoracion_encontrar_empleo + Valoracion_sit_ec_espana + Valoracion_prospectiva_hogar +
                    Valoracion_mejorar_empleo_esp_6m + Valoracion_prospectiva_espana +
                    Valoracion_inflacion + Evolucion_tipo_interes +
                    Evolucion_precio_vivienda + Intencion_comprar_vivienda +
                    Estado_civil + Tamano_hogar + Ingresos_hogar +
                    Adquisicion_mueble + Adquisicion_electrodomestico_grande +
                    Adquisicion_electrodomestico_pequeño + Composicion_hogar, 
                  data=train_regresion)

nuevos.datos <- data.frame(año = 2025, 
                           `poblacion_activa` = 24000, 
                           `numero_parados` = 3100000, 
                           `poblacion_total` = 47400000,
                           `esperanza_vida` = 84,
                           `evolucion_PIB` = 3,
                           `salario_medio` = 25300.00,
                           `tasa_natalidad` = 7, 
                           `inflacion` = 2.3, 
                           `tasa_inmigracion` = 12, 
                           `gasto_publico` = 620000, 
                           `numero_pensionistas` = 10.7)


names(dfinal)

view(nuevos.datos)


prediccion <- predict(modelosinvar, nuevos.datos)

print(prediccion)

# Transformacion logaritmica ----------------------------------------------

# Ejemplo de transformación logarítmica de una variable
df_completoA$log_sit_ec_hogar <- log(df_completoA$Situacion_economia_hogar)
df_completoA$log_prosp <- log(df_completoA$Valoracion_prospectiva_hogar)
df_completoA$log_empleo <- log(df_completoA$Valoracion_encontrar_empleo)
df_completoA$log_paro <- log(df_completoA$Evolucion_paro_entorno_6m)



# Crear un nuevo modelo con la variable transformada
modelo_transformado <- lm(Confianza_consumidor ~ log_sit_ec_hogar + log_prosp +
                            log_empleo + log_paro, data = df_completoA)
summary(modelo_transformado)

# Realizar predicciones con el modelo en el conjunto de prueba
predicciones_log <- predict(modelo_transformado, newdata = df_completoA)

# Calcular los residuos
residuos <- df_completoA$Confianza_consumidor - predicciones_log

# Calcular el RMSE
rmse <- sqrt(mean(residuos^2))
print(rmse)


# Mejora modelos ----------------------------------------------------------

# Paso 1: Normalización de variables
variables_significativas13 <- c("Valoracion_prospectiva_hogar", "Evolucion_ahorro_personal",
                                "MES", "Valoracion_mejorar_empleo_esp_6m", "Valoracion_sit_ec_hogar",
                                "Valoracion_prospectiva_espana", "Valoracion_encontrar_empleo", "Year",
                                "Evolucion_precio_vivienda", "Valoracion_sit_ec_espana", "Variacion",
                                "Evolucion_paro_entorno_6m", "Adquisicion_electrodomestico_grande",
                                "Adquisicion_electrodomestico_pequeño", "Adquisicion_mueble",
                                "Personas_paro_entorno", "Situacion_laboral", "Intencion_comprar_vivienda",
                                "Sexo")
preproc <- preProcess(train_regresion[, variables_significativas13], method = c("center", "scale"))
train_norm <- predict(preproc, newdata = train_regresion[, variables_significativas13])
test_norm <- predict(preproc, newdata = test_regresion[, variables_significativas13])

# Paso 2: Selección de características con regresión lasso
x <- train_norm[, -ncol(train_norm)]
x <- as.matrix(train_norm[, -ncol(train_norm)])
y <- train_regresion$Confianza_consumidor

lasso_model <- cv.glmnet(x, y, alpha = 1)
lambda_opt <- lasso_model$lambda.min

lasso_coefs <- as.matrix(lasso_coefs)
selected_features <- rownames(lasso_coefs)[lasso_coefs[, 1] != 0]

# Paso 3: Regularización con regresión ridge
# Obtener los índices de las columnas seleccionadas
selected_indices <- which(colnames(x) %in% selected_features)

# Seleccionar las columnas correspondientes en la matriz x
x_selected <- x[, selected_indices]

ridge_model <- glmnet(x_selected, y, alpha = 0)
lambda_opt_ridge <- cv.glmnet(x_selected, y, alpha = 0)$lambda.min

ridge_model_final <- glmnet(x_selected, y, alpha = 0, lambda = lambda_opt_ridge)

# Paso 4: Validación cruzada
x_test <- test_norm[, selected_indices]
y_test <- test_regresion$Confianza_consumidor
# Convertir x_test a matriz
x_test_matrix <- as.matrix(x_test)

# Realizar predicciones
predicciones <- predict(ridge_model_final, newx = x_test_matrix)
rmse_cv <- sqrt(mean((y_test - predicciones)^2))

# Imprimir los resultados
cat("Variables seleccionadas por lasso:", selected_features, "\n")
cat("Valor óptimo de lambda para ridge:", lambda_opt_ridge, "\n")
cat("RMSE por validación cruzada:", rmse_cv, "\n")
