
# REGRESION LOGISTICA MULTINOMIAL -----------------------------------------
library(nnet)
train_data <- df_completo[df_completo$Year %in% 2018:2021, ]
test_data <- df_completo[df_completo$Year == 2022, ]

model_reg_log <- multinom(Confianza_consumidor ~ ., data = train_data)



# Convertir variables predictoras a factores si no lo están
train_data[, -1] <- lapply(train_data[, -1], as.factor)
test_data[, -1] <- lapply(test_data[, -1], as.factor)

# Ajustar el modelo de regresión logística multinomial
model <- multinom(Confianza_consumidor ~ ., data = train_data)

# Realizar predicciones en el conjunto de prueba
predictions <- predict(model, newdata = test_data, type = "class")

# Calcular la matriz de confusión
confusion_matrix <- table(predictions, test_data$Confianza_consumidor)
print(confusion_matrix)

# Calcular la precisión global
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Precisión global:", accuracy))

# Calcular otros errores de clasificación
errors <- caret::confusionMatrix(predictions, test_data$Confianza_consumidor)$overall["Accuracy"]
print(paste("Error de clasificación:", 1 - errors))

# Crear una gráfica de barras para visualizar la matriz de confusión
ggplot(data = as.data.frame.table(confusion_matrix), aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = c("#2E9FDF", "#E37334", "#3BBE58", "#E6C817")) +
  labs(title = "Matriz de Confusión",
       x = "Predicción",
       y = "Frecuencia") +
  theme_minimal()


# DA TODO ERROR PORQUE 
# need two or more classes to fit a multinom model


# GRADIENT BOOSTING -------------------------------------------------------

# Dividir los datos en conjuntos de entrenamiento y prueba
set.seed(123)
train_data <- df_completo[df_completo$Year %in% 2018:2021, ]
test_data <- df_completo[df_completo$Year == 2022, ]

# Convertir variables predictoras a factores si no lo están
train_data[, -1] <- lapply(train_data[, -1], as.factor)
test_data[, -1] <- lapply(test_data[, -1], as.factor)

# Preparar los datos para el modelo
X_train <- data.matrix(train_data[, -1])  # Variables predictoras de entrenamiento
y_train <- train_data$Confianza_consumidor  # Variable objetivo de entrenamiento

X_test <- data.matrix(test_data[, -1])  # Variables predictoras de prueba
y_test <- test_data$Confianza_consumidor  # Variable objetivo de prueba

# Ajustar el modelo de Gradient Boosting
model <- xgboost(data = X_train, label = y_train, nrounds = 100, objective = "reg:squarederror")

# Realizar predicciones en el conjunto de prueba
predictions <- predict(model, newdata = X_test)

# Convertir la variable objetivo a tipo numérico
y_train <- as.numeric(as.character(y_train))
y_test <- as.numeric(as.character(y_test))

# Calcular el error cuadrático medio (MSE)
mse <- mean((predictions - y_test)^2)
print(paste("Error cuadrático medio:", mse))
# Calcular el Error Cuadrático Medio (RMSE)
rmse <- sqrt(mean((predictions - y_test)^2))
# Calcular el Error Absoluto Medio (MAE)
mae <- mean(abs(predictions - y_test))
# Calcular el Error Porcentual Absoluto Medio (MAPE)
mape <- mean(abs((predictions - y_test) / y_test)) * 100

# Imprimir los resultados
print(paste("RMSE:", rmse))
print(paste("MAE:", mae))
print(paste("MAPE:", mape))

# Calcular el coeficiente de determinación (R²)
ss_total <- sum((y_test - mean(y_test))^2)
ss_residual <- sum((y_test - predictions)^2)
r_squared <- 1 - (ss_residual/ss_total)
print(paste("Coeficiente de determinación (R²):", r_squared))


# MODELO II GRADIENT BOOSTING ---------------------------------------------
# Seleccionar las variables predictoras deseadas
variables <- c("Confianza_consumidor","Year", "Valoracion_sit_ec_espana", "Evolucion_precio_vivienda", "Valoracion_encontrar_empleo", 
               "Valoracion_prospectiva_espana", "Valoracion_sit_ec_hogar", "Valoracion_mejorar_empleo_esp_6m", 
               "MES", "Variacion", "Evolucion_ahorro_personal")

# Filtrar el dataframe con las variables predictoras seleccionadas
df_predictoras <- df_completo[, variables]

# Dividir los datos en conjuntos de entrenamiento y prueba
set.seed(123)
train_data <- df_predictoras[df_predictoras$Year %in% 2018:2021, ]
test_data <- df_predictoras[df_predictoras$Year == 2022, ]

# Convertir variables predictoras a factores si no lo están
train_data <- lapply(train_data, as.factor)
test_data <- lapply(test_data, as.factor)

# Preparar los datos para el modelo
X_train <- do.call(cbind, train_data)  # Variables predictoras de entrenamiento
y_train <- df_predictoras[df_predictoras$Year %in% 2018:2021, "Confianza_consumidor"]  # Variable objetivo de entrenamiento

X_test <- do.call(cbind, test_data)  # Variables predictoras de prueba
y_test <- df_predictoras[df_predictoras$Year == 2022, "Confianza_consumidor"]  # Variable objetivo de prueba

# Ajustar el modelo de Gradient Boosting
library(xgboost)
model <- xgboost(data = as.matrix(X_train), label = y_train, nrounds = 100, objective = "reg:squarederror")

# Realizar predicciones en el conjunto de prueba
predictions <- predict(model, newdata = as.matrix(X_test))

# Calcular el error cuadrático medio (MSE)
mse <- mean((predictions - y_test)^2)
print(paste("Error cuadrático medio:", mse))

# Calcular el Error Cuadrático Medio (RMSE)
rmse <- sqrt(mean((predictions - y_test)^2))
print(paste("RMSE:", rmse))

# Calcular el Error Absoluto Medio (MAE)
mae <- mean(abs(predictions - y_test))
print(paste("MAE:", mae))

# Calcular el Error Porcentual Absoluto Medio (MAPE)
mape <- mean(abs((predictions - y_test) / y_test)) * 100
print(paste("MAPE:", mape))

# Calcular el coeficiente de determinación (R²)
ss_total <- sum((y_test - mean(y_test))^2)
ss_residual <- sum((y_test - predictions)^2)
r_squared <- 1 - (ss_residual/ss_total)
print(paste("Coeficiente de determinación (R²):", r_squared))


# Gráfico de dispersión de valores reales y predicciones
plot(y_test, predictions, main = "Valores reales vs. Predicciones", xlab = "Valores reales", ylab = "Predicciones")
abline(0, 1, col = "red")


# Crear un dataframe con los valores reales y predichos
df_resultados <- data.frame(MES = test_data$MES, Valor_Real = y_test, Prediccion = predictions)
# Calcular la media de los valores reales y predichos por mes
tabla <- aggregate(. ~ MES, data = df_resultados, mean)
tabla <- tabla[, c("MES", "Valor_Real", "Prediccion")]
print(tabla)

# MODELO III GRADIENT BOOSTING --------------------------------------------
# Definir variables significativas
variables <- c("Confianza_consumidor", "Valoracion_prospectiva_hogar", "Evolucion_ahorro_personal",
                                "MES", "Valoracion_mejorar_empleo_esp_6m", "Valoracion_sit_ec_hogar",
                                "Valoracion_prospectiva_espana", "Valoracion_encontrar_empleo", "Year",
                                "Evolucion_precio_vivienda", "Valoracion_sit_ec_espana", "Variacion",
                                "Evolucion_paro_entorno_6m", "Adquisicion_electrodomestico_grande",
                                "Adquisicion_electrodomestico_pequeño", "Adquisicion_mueble",
                                "Personas_paro_entorno", "Situacion_laboral", "Intencion_comprar_vivienda",
                                "Sexo")

# Filtrar el dataframe con las variables predictoras seleccionadas
df_predictoras <- df_completo[, variables]

# Dividir los datos en conjuntos de entrenamiento y prueba
set.seed(123)
train_data <- df_predictoras[df_predictoras$Year %in% 2018:2021, ]
test_data <- df_predictoras[df_predictoras$Year == 2022, ]

# Convertir variables predictoras a factores si no lo están
train_data <- lapply(train_data, as.factor)
test_data <- lapply(test_data, as.factor)

# Preparar los datos para el modelo
X_train <- do.call(cbind, train_data)  # Variables predictoras de entrenamiento
y_train <- df_predictoras[df_predictoras$Year %in% 2018:2021, "Confianza_consumidor"]  # Variable objetivo de entrenamiento

X_test <- do.call(cbind, test_data)  # Variables predictoras de prueba
y_test <- df_predictoras[df_predictoras$Year == 2022, "Confianza_consumidor"]  # Variable objetivo de prueba

# Ajustar el modelo de Gradient Boosting
library(xgboost)
model <- xgboost(data = as.matrix(X_train), label = y_train, nrounds = 200, objective = "reg:squarederror",
                 maxdepth = 5)

# Realizar predicciones en el conjunto de prueba
predictions <- predict(model, newdata = as.matrix(X_test))

# Calcular el error cuadrático medio (MSE)
mse <- mean((predictions - y_test)^2)
print(paste("Error cuadrático medio:", mse))

# Calcular el Error Cuadrático Medio (RMSE)
rmse <- sqrt(mean((predictions - y_test)^2))
print(paste("RMSE:", rmse))

# Calcular el Error Absoluto Medio (MAE)
mae <- mean(abs(predictions - y_test))
print(paste("MAE:", mae))

# Calcular el Error Porcentual Absoluto Medio (MAPE)
mape <- mean(abs((predictions - y_test) / y_test)) * 100
print(paste("MAPE:", mape))

# Calcular el coeficiente de determinación (R²)
ss_total <- sum((y_test - mean(y_test))^2)
ss_residual <- sum((y_test - predictions)^2)
r_squared <- 1 - (ss_residual/ss_total)
print(paste("Coeficiente de determinación (R²):", r_squared))


# Gráfico de dispersión de valores reales y predicciones
plot(y_test, predictions, main = "Valores reales vs. Predicciones", xlab = "Valores reales", ylab = "Predicciones")
abline(0, 1, col = "red")


# Crear un dataframe con los valores reales y predichos
df_resultados <- data.frame(MES = test_data$MES, Valor_Real = y_test, Prediccion = predictions)
# Calcular la media de los valores reales y predichos por mes
tabla <- aggregate(. ~ MES, data = df_resultados, mean)
tabla <- tabla[, c("MES", "Valor_Real", "Prediccion")]
print(tabla)
view(tabla)


# MODELO GRADIENT BOOSTING IV ---------------------------------------------


# Definir variables significativas
variables <- c("Confianza_consumidor", "Situacion_economia_hogar",
                                "Valoracion_mejorar_empleo_esp_6m", "Valoracion_sit_ec_espana",
                                "Valoracion_prospectiva_hogar", "Valoracion_encontrar_empleo", 
                                "Valoracion_prospectiva_espana", "Year", "MES")

# Filtrar el dataframe con las variables predictoras seleccionadas
df_predictoras <- df_completo[, variables]

# Dividir los datos en conjuntos de entrenamiento y prueba
set.seed(123)
train_data <- df_predictoras[df_predictoras$Year %in% 2018:2021, ]
test_data <- df_predictoras[df_predictoras$Year == 2022, ]

# Convertir variables predictoras a factores si no lo están
train_data <- lapply(train_data, as.factor)
test_data <- lapply(test_data, as.factor)

# Preparar los datos para el modelo
X_train <- do.call(cbind, train_data)  # Variables predictoras de entrenamiento
y_train <- df_predictoras[df_predictoras$Year %in% 2018:2021, "Confianza_consumidor"]  # Variable objetivo de entrenamiento

X_test <- do.call(cbind, test_data)  # Variables predictoras de prueba
y_test <- df_predictoras[df_predictoras$Year == 2022, "Confianza_consumidor"]  # Variable objetivo de prueba

# Ajustar el modelo de Gradient Boosting
model <- xgboost(data = as.matrix(X_train), label = y_train, nrounds = 100, objective = "reg:squarederror")

# Realizar predicciones en el conjunto de prueba
predictions <- predict(model, newdata = as.matrix(X_test))

# Calcular el error cuadrático medio (MSE)
mse <- mean((predictions - y_test)^2)
print(paste("Error cuadrático medio:", mse))

# Calcular el Error Cuadrático Medio (RMSE)
rmse <- sqrt(mean((predictions - y_test)^2))
print(paste("RMSE:", rmse))

# Calcular el Error Absoluto Medio (MAE)
mae <- mean(abs(predictions - y_test))
print(paste("MAE:", mae))

# Calcular el Error Porcentual Absoluto Medio (MAPE)
mape <- mean(abs((predictions - y_test) / y_test)) * 100
print(paste("MAPE:", mape))

# Calcular el coeficiente de determinación (R²)
ss_total <- sum((y_test - mean(y_test))^2)
ss_residual <- sum((y_test - predictions)^2)
r_squared <- 1 - (ss_residual/ss_total)
print(paste("Coeficiente de determinación (R²):", r_squared))


# Gráfico de dispersión de valores reales y predicciones
plot(y_test, predictions, main = "Valores reales vs. Predicciones", xlab = "Valores reales", ylab = "Predicciones")
abline(0, 1, col = "red")


# Crear un dataframe con los valores reales y predichos
df_resultados <- data.frame(MES = test_data$MES, Valor_Real = y_test, Prediccion = predictions)
# Calcular la media de los valores reales y predichos por mes
tabla <- aggregate(. ~ MES, data = df_resultados, mean)
tabla <- tabla[, c("MES", "Valor_Real", "Prediccion")]
print(tabla)



# GRADIENT BOOSTING PARAMS ------------------------------------------------

# Definir los parámetros del modelo
params <- list(
  objective = "reg:squarederror",
  max_depth = 2,
  eta = 0.1,
  subsample = 0.5,
  colsample_bytree = 0.5
)

# Definir variables significativas
variables <- c("Confianza_consumidor", "Situacion_economia_hogar",
               "Valoracion_mejorar_empleo_esp_6m", "Valoracion_sit_ec_espana",
               "Valoracion_prospectiva_hogar", "Valoracion_encontrar_empleo", 
               "Valoracion_prospectiva_espana", "Year", "MES")

# Dividir los datos en conjuntos de entrenamiento y prueba
set.seed(123)
train_data <- df_completo[df_completo$Year %in% 2018:2021, ]
test_data <- df_completo[df_completo$Year == 2022, ]


# Filtrar el dataframe con las variables predictoras seleccionadas
train_data <- train_data[, c("Confianza_consumidor", variables)]
test_data <- test_data[, c("Confianza_consumidor", variables)]

# Convertir variables predictoras a factores si no lo están
train_data[, variables] <- lapply(train_data[, variables], as.factor)
test_data[, variables] <- lapply(test_data[, variables], as.factor)

# Preparar los datos para el modelo
X_train <- data.matrix(train_data[, variables])  # Variables predictoras de entrenamiento
y_train <- train_data$Confianza_consumidor  # Variable objetivo de entrenamiento

X_test <- data.matrix(test_data[, variables])  # Variables predictoras de prueba
y_test <- test_data$Confianza_consumidor  # Variable objetivo de prueba

# Ajustar el modelo de Gradient Boosting
model <- xgboost(data = X_train, label = y_train, params = params, nrounds = 200)

# Realizar predicciones en el conjunto de prueba
predictions <- predict(model, newdata = X_test)
predictions <- as.numeric(as.character(predictions))
y_test <- as.numeric(as.character(y_test))

# Calcular el error cuadrático medio (MSE)
mse <- mean((predictions - y_test)^2)
print(paste("Error cuadrático medio:", mse))

# Calcular el Error Cuadrático Medio (RMSE)
rmse <- sqrt(mean((predictions - y_test)^2))
print(paste("RMSE:", rmse))

# Calcular el Error Absoluto Medio (MAE)
mae <- mean(abs(predictions - y_test))
print(paste("MAE:", mae))

# Calcular el Error Porcentual Absoluto Medio (MAPE)
mape <- mean(abs((predictions - y_test) / y_test)) * 100
print(paste("MAPE:", mape))

# Calcular el coeficiente de determinación (R²)
ss_total <- sum((y_test - mean(y_test))^2)
ss_residual <- sum((y_test - predictions)^2)
r_squared <- 1 - (ss_residual/ss_total)
print(paste("Coeficiente de determinación (R²):", r_squared))


# Gráfico de dispersión de valores reales y predicciones
plot(y_test, predictions, main = "Valores reales vs. Predicciones", xlab = "Valores reales", ylab = "Predicciones")
abline(0, 1, col = "red")


# Crear un dataframe con los valores reales y predichos
df_resultados <- data.frame(MES = test_data$MES, Valor_Real = y_test, Prediccion = predictions)
# Calcular la media de los valores reales y predichos por mes
tabla <- aggregate(. ~ MES, data = df_resultados, mean)
tabla <- tabla[, c("MES", "Valor_Real", "Prediccion")]
print(tabla)

# GRADIENT BOOSTING PARAMS II ---------------------------------------------
# Definir los parámetros del modelo
params <- list(
  objective = "reg:squarederror",
  max_depth = 2,
  eta = 0.1,
  subsample = 0.5,
  colsample_bytree = 0.5
)

# Definir variables significativas
variables <- c("Confianza_consumidor", "Valoracion_prospectiva_hogar", "Evolucion_ahorro_personal",
               "MES", "Valoracion_mejorar_empleo_esp_6m", "Valoracion_sit_ec_hogar",
               "Valoracion_prospectiva_espana", "Valoracion_encontrar_empleo", "Year",
               "Evolucion_precio_vivienda", "Valoracion_sit_ec_espana", "Variacion",
               "Evolucion_paro_entorno_6m", "Adquisicion_electrodomestico_grande",
               "Adquisicion_electrodomestico_pequeño", "Adquisicion_mueble",
               "Personas_paro_entorno", "Situacion_laboral", "Intencion_comprar_vivienda",
               "Sexo")

# Dividir los datos en conjuntos de entrenamiento y prueba
set.seed(123)
train_data <- df_completo[df_completo$Year %in% 2018:2021, ]
test_data <- df_completo[df_completo$Year == 2022, ]


# Filtrar el dataframe con las variables predictoras seleccionadas
train_data <- train_data[, c("Confianza_consumidor", variables)]
test_data <- test_data[, c("Confianza_consumidor", variables)]

# Convertir variables predictoras a factores si no lo están
train_data[, variables] <- lapply(train_data[, variables], as.factor)
test_data[, variables] <- lapply(test_data[, variables], as.factor)

# Preparar los datos para el modelo
X_train <- data.matrix(train_data[, variables])  # Variables predictoras de entrenamiento
y_train <- train_data$Confianza_consumidor  # Variable objetivo de entrenamiento

X_test <- data.matrix(test_data[, variables])  # Variables predictoras de prueba
y_test <- test_data$Confianza_consumidor  # Variable objetivo de prueba

# Ajustar el modelo de Gradient Boosting
model <- xgboost(data = X_train, label = y_train, params = params, nrounds = 200)

# Realizar predicciones en el conjunto de prueba
predictions <- predict(model, newdata = X_test)
predictions <- as.numeric(as.character(predictions))
y_test <- as.numeric(as.character(y_test))

# Calcular el error cuadrático medio (MSE)
mse <- mean((predictions - y_test)^2)
print(paste("Error cuadrático medio:", mse))

# Calcular el Error Cuadrático Medio (RMSE)
rmse <- sqrt(mean((predictions - y_test)^2))
print(paste("RMSE:", rmse))

# Calcular el Error Absoluto Medio (MAE)
mae <- mean(abs(predictions - y_test))
print(paste("MAE:", mae))

# Calcular el Error Porcentual Absoluto Medio (MAPE)
mape <- mean(abs((predictions - y_test) / y_test)) * 100
print(paste("MAPE:", mape))

# Calcular el coeficiente de determinación (R²)
ss_total <- sum((y_test - mean(y_test))^2)
ss_residual <- sum((y_test - predictions)^2)
r_squared <- 1 - (ss_residual/ss_total)
print(paste("Coeficiente de determinación (R²):", r_squared))


# Gráfico de dispersión de valores reales y predicciones
plot(y_test, predictions, main = "Valores reales vs. Predicciones", xlab = "Valores reales", ylab = "Predicciones")
abline(0, 1, col = "red")


# Crear un dataframe con los valores reales y predichos
df_resultados <- data.frame(MES = test_data$MES, Valor_Real = y_test, Prediccion = predictions)
# Calcular la media de los valores reales y predichos por mes
tabla <- aggregate(. ~ MES, data = df_resultados, mean)
tabla <- tabla[, c("MES", "Valor_Real", "Prediccion")]
print(tabla)

# REGRESION BRIDGE --------------------------------------------------------


library(glmnet)

# Preparar los datos de entrenamiento
x <- as.matrix(predictor_vars)  # Variables predictoras
y <- as.matrix(df_completo$Confianza_consumidor)  # Variable respuesta

# Obtener índices para datos de entrenamiento (años 2018-2021)
train_indices <- which(df_completo$Year %in% c(2018, 2019, 2020, 2021))

# Obtener índices para datos de prueba (año 2022)
test_indices <- which(df_completo$Year == 2022)

# Dividir los datos en conjunto de entrenamiento y prueba
x_train <- x[train_indices, ]
y_train <- y[train_indices, ]
x_test <- x[test_indices, ]
y_test <- y[test_indices, ]

# Ajustar un modelo de regresión Ridge
ridge_model <- glmnet(x_train, y_train, alpha = 0, lambda = 0.1)

# Ajustar un modelo de regresión LASSO
lasso_model <- glmnet(x_train, y_train, alpha = 1, lambda = 0.1)

# Obtener las predicciones para el conjunto de prueba
ridge_predictions <- predict(ridge_model, newx = x_test)
lasso_predictions <- predict(lasso_model, newx = x_test)

# Calcular el error cuadrático medio (MSE) para cada modelo
ridge_mse <- mean((ridge_predictions - y_test)^2)
lasso_mse <- mean((lasso_predictions - y_test)^2)

# Imprimir los resultados
print("Error cuadrático medio (MSE) - Regresión Ridge:")
print(ridge_mse)
print("Error cuadrático medio (MSE) - Regresión LASSO:")
print(lasso_mse)

# Definir variables significativas
variables_significativas13 <- c("Valoracion_prospectiva_hogar", "Evolucion_ahorro_personal",
                                "MES", "Valoracion_mejorar_empleo_esp_6m", "Valoracion_sit_ec_hogar",
                                "Valoracion_prospectiva_espana", "Valoracion_encontrar_empleo", "Year",
                                "Evolucion_precio_vivienda", "Valoracion_sit_ec_espana", "Variacion",
                                "Evolucion_paro_entorno_6m", "Adquisicion_electrodomestico_grande",
                                "Adquisicion_electrodomestico_pequeño", "Adquisicion_mueble",
                                "Personas_paro_entorno", "Situacion_laboral", "Intencion_comprar_vivienda",
                                "Sexo")

# Definir variables significativas
variables_significativas13 <- c("Valoracion_prospectiva_hogar","MES", "Valoracion_mejorar_empleo_esp_6m", "Valoracion_sit_ec_hogar",
                                "Valoracion_encontrar_empleo", "Year",
                                "Evolucion_precio_vivienda", "Variacion",
                                "Evolucion_paro_entorno_6m", "Adquisicion_electrodomestico_grande",
                                "Adquisicion_electrodomestico_pequeño", "Adquisicion_mueble",
                                "Personas_paro_entorno", "Situacion_laboral", "Intencion_comprar_vivienda")

# CON VARIABLES PREDICTORAS
# Definir las variables predictoras
x_train <- x[train_indices, variables_significativas13]
x_test <- x[test_indices, variables_significativas13]

# Ajustar un modelo de regresión Ridge
ridge_model <- glmnet(x_train, y_train, alpha = 0, lambda = 0.1)

# Ajustar un modelo de regresión LASSO
lasso_model <- glmnet(x_train, y_train, alpha = 1, lambda = 0.1)

# Obtener las predicciones para el conjunto de prueba
ridge_predictions <- predict(ridge_model, newx = x_test)
lasso_predictions <- predict(lasso_model, newx = x_test)

# Calcular el error cuadrático medio (RMSE) para cada modelo
ridge_rmse <- sqrt(mean((ridge_predictions - y_test)^2))
lasso_rmse <- sqrt(mean((lasso_predictions - y_test)^2))

# Calcular el error absoluto medio (MAE) para cada modelo
ridge_mae <- mean(abs(ridge_predictions - y_test))
lasso_mae <- mean(abs(lasso_predictions - y_test))

# Calcular el error porcentual absoluto medio (MAPE) para cada modelo
ridge_mape <- mean(abs((ridge_predictions - y_test) / y_test)) * 100
lasso_mape <- mean(abs((lasso_predictions - y_test) / y_test)) * 100

# Imprimir los resultados
print("RMSE - Regresión Ridge:")
print(ridge_rmse)
print("RMSE - Regresión LASSO:")
print(lasso_rmse)
print("MAE - Regresión Ridge:")
print(ridge_mae)
print("MAE - Regresión LASSO:")
print(lasso_mae)
print("MAPE - Regresión Ridge:")
print(ridge_mape)
print("MAPE - Regresión LASSO:")
print(lasso_mape)
