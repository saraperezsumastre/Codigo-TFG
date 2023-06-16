# BAGGINGGGGG
# Librerias ---------------------------------------------------------------

library(tidyverse)
library(rpart)
library(ipred)
library(FNN)

# TRAIN Y TEST ------------------------------------------------------------
# Dividir train y test
# Como me habia colapsado alguna vez, lo voy a hacer sobre el dataframe reducido

# Semilla aleatoria para la reproducibilidad
set.seed(123)
#TRAIN
train_bagging <- df_reducido %>%
  filter(Year %in% c(2018,2019, 2020, 2021))
# TEST
test_bagging <- df_reducido %>%
  filter(Year == 2022)
# Se queda train en 24.000 y test en 5.500


# MODELO BAGGING SCRIPT CLASE --------------------------------------------

# Modelo bagging
mod_bag <- bagging(Confianza_consumidor ~ ., data = train_bagging)
pred_bag <- predict(mod_bag, newdata = test_bagging)

# Calcular el RMSE
rmse <- function(real, pred) {
  sqrt(mean((real-pred)^2))
}

rmse(test_bagging$Confianza_consumidor, pred_bag)
# Sale 27.86

# por comodidad vamos a añadir una nueva columna 
# en test que sea predicciones
test_bagging$pred_bag <- pred_bag

ggplot(test_bagging, aes(x = Confianza_consumidor, y = Situacion_economia_hogar))+
  geom_point(alpha=0.25)+
  geom_line(aes(y = pred_bag), color = "firebrick")
# Este grafico????

# qué podemos configurar de un bagging? la B 
# queremos que sea un modelo mas flexible
# queremos elegir el numero de muestras sobre el que 
# se construye nuestro modelo
# He elegido 5000
mod_bag2 <- bagging(Confianza_consumidor ~ ., data = train_bagging, nbagg = 200)
pred_bag2 <- predict(mod_bag2, newdata = test_bagging)

# Nueva columna
test_bagging$pred_bag2 <- pred_bag2

rmse(test_bagging$Confianza_consumidor, pred_bag2)
# Mismo error basicamente, 28.87

ggplot(test_bagging, aes(x = Confianza_consumidor, y = Situacion_economia_hogar))+
  geom_point(alpha=0.25)+
  geom_line(aes(y = pred_bag2), color = "firebrick")

names(test_bagging)
# con esto el resultado queda bascimanete igual
# osea que el parametro B no nos ha ayudado a 
# conseguir un modelo mas flexible

# probar un modelo B diferente y que el modelo tiene 
# un error pequeño, lo hacemos con B (parecido KNN)

B <- seq(1, 200, by = 5)

# vector donde voy a almacenar los errores
# para cada valor de B
rmse_test_bagging <- c()

# Y TENDRE QUE GENERAR UN BUCLE 
# en cada iteracion va a generar un modelo y lo que cambia en cada iteracion es el numero
# de muestras boostrap la primera iteracion del bucle es 1 y que entre
# en un bagging con el valor de B que queremos probar
for (i in 1:length(B)){
  mod <- bagging(Confianza_consumidor ~ ., data = train_bagging, nbagg= B[i])
  pred <- predict(mod, newdata = test_bagging)
  rmse(test_bagging$Confianza_consumidor, pred)
}

# en vez del vector vacio de arriba, como antes de que empiece el bucle 
# va a dar tantas vueltas como elementos tenga B
# lo que hago es defino un vector de longitud de B (length(B)) que es 40
# cada vuelta que da el bucle es independiente a la anterior
length(B)
# ES MAS EFICIENTE QUE UN VECTOR VACIO 

B <- seq(1, 100, by = 5)

rmse_test_bagging <- numeric(length(B))
# TENGO QUE DECIR EN RMSE que linea es la que va a rellenar (posicion)
for (i in 1:length(B)){
  mod <- bagging(Confianza_consumidor ~ ., data = train_bagging, nbagg= B[i])
  pred <- predict(mod, newdata = test_bagging)
  rmse_test_bagging[i] <- rmse(test_bagging$Confianza_consumidor, pred)
}

# Representar 
# construir un dataframe que va a contener 2 variable
# la primera el valor de b y la segunda el valor del error asociado a esa B

test_error <- data.frame(B=B, RMSE=rmse_test_bagging)

# grafico
ggplot(test_error, aes(x = B, y = RMSE))+
  geom_line(color = "orange3")

# tunear 

mod_bag1 <- bagging(Confianza_consumidor ~ ., data = train_bagging, nbagg = 50, 
                    control = rpart.control(cp = 0.001))
pred_bag1 <- predict(mod_bag1, newdata = test_bagging)

test_bagging$pred_bag1 <- pred_bag1

ggplot(test_bagging, aes(x = Confianza_consumidor, y = Evolucion_paro_entorno_6m))+
  geom_point(alpha=0.25)+
  geom_line(aes(y = pred_bag1), color = "firebrick")


ggplot(test_bagging, aes(x = Confianza_consumidor, y = pred_bag1))+
  geom_point(alpha=0.25)


# cambiar sequencia de B para que se ajuste al cp 
# nos vamos a fijar en cp en vez de en B
CP <- 10**(-(1:10))

rmse_test_2 <- numeric(length(CP))

# este bucle tiene 10 iteraciones
for (i in 1:length(CP)){
  mod <- bagging(Confianza_consumidor ~ ., data = train_bagging, nbagg= 55, 
                 control = rpart.control(cp = CP[i]))
  pred <- predict(mod, newdata = test_bagging)
  rmse_test_2[i] <- rmse(test_bagging$Confianza_consumidor, pred)
}

# volver a construir el dtaframe para ver el error con cp y luego grafico
test_error <- data.frame(CP = CP, RMSE=rmse_test_2)

# grafico
ggplot(test_error, aes(x = CP, y = RMSE))+
  geom_line(color = "orange3")

# NO NOS VALE Y QUEREMOS EXPONENTES
ggplot(test_error, aes(x = log10(CP), y = RMSE))+
  geom_line(color = "orange3")

# voy a entrenar otro modelo de bagging 
# bagging funcion por defecto 25 muestras
# coob para calcular el error esta en false por defecto asi que poner true
mod_bag_oob <- bagging(Confianza_consumidor ~ ., data=train_bagging,
                       coob = TRUE)

mod_bag_oob$err
# ESTO NO LO ENTIENDO 



# MODELO BAGGING 3 --------------------------------------------------------

mod_bag3 <- bagging(Confianza_consumidor ~ .-Year -MES -Variacion, data = train_bagging, nbagg = 200)
pred_bag3 <- predict(mod_bag3, newdata = test_bagging)

# Nueva columna
test_bagging$pred_bag3 <- pred_bag3

rmse(test_bagging$Confianza_consumidor, pred_bag3)
# Mismo error basicamente, 28.87


# MODELO BAGGING 4 --------------------------------------------------------
# ESTE MODELO ME DA MUY BIEN 
# Filtrar los datos de entrenamiento y prueba
train_data <- df_completo[df_completo$Year %in% 2018:2021, ]
test_data <- df_completo[df_completo$Year == 2022, ]

# Seleccionar las variables predictoras deseadas
variables <- c("Situacion_economia_hogar", "Valoracion_mejorar_empleo_esp_6m",
               "Valoracion_sit_ec_espana", "Valoracion_prospectiva_hogar",
               "Valoracion_encontrar_empleo", "Valoracion_prospectiva_espana", "Confianza_consumidor",
               "Year", "MES")

# Crear matriz de variables predictoras
X_train <- train_data[, variables]
X_test <- test_data[, variables]

# Convertir la variable objetivo a tipo numérico
y_train <- as.numeric(as.character(train_data$Confianza_consumidor))
y_test <- as.numeric(as.character(test_data$Confianza_consumidor))

library(ipred)

# Entrenar el modelo de Bagging
model <- bagging(y_train ~ ., data = X_train, nbagg = 100)
model <- bagging(y_train ~ ., data = X_train, B = B, coob = TRUE, boottree = TRUE, mfinal = 50, allowParallel = TRUE, importance = TRUE)


# Realizar predicciones en el conjunto de prueba
predictions <- predict(model, newdata = X_test)

# Calcular el RMSE
rmse <- sqrt(mean((predictions - y_test)^2))
print(paste("RMSE:", rmse))

# Calcular el MAE
mae <- mean(abs(predictions - y_test))
print(paste("MAE:", mae))

# Calcular el MAPE
mape <- mean(abs((predictions - y_test) / y_test)) * 100
print(paste("MAPE:", mape))

# Crear tabla de comparación de valores reales y predichos por mes de cada año
result <- data.frame(MES = test_data$MES,
                     Year = test_data$Year,
                     Valor_Real = y_test,
                     Prediccion = predictions)

table_result <- aggregate(cbind(Valor_Real, Prediccion) ~ MES + Year, data = result, mean)
print(table_result)

# Crear un dataframe con los valores reales y las predicciones
result_plot <- data.frame(Valor_Real = y_test, Prediccion = predictions)

# Graficar los valores reales vs. predicciones
ggplot(result_plot, aes(x = Valor_Real, y = Prediccion)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(x = "Valor Real", y = "Predicción") +
  ggtitle("Comparación de Valores Reales vs. Predicciones (Bagging)")


# MODELO BAGGING 5 --------------------------------------------------------

# Filtrar los datos de entrenamiento y prueba
set.seed(123)
train_data <- df_completo[df_completo$Year %in% 2018:2021, ]
test_data <- df_completo[df_completo$Year == 2022, ]

# Seleccionar las variables predictoras deseadas
variables <- c("Confianza_consumidor", "Valoracion_prospectiva_hogar", "Evolucion_ahorro_personal",
               "MES", "Valoracion_mejorar_empleo_esp_6m", "Valoracion_sit_ec_hogar",
               "Valoracion_prospectiva_espana", "Valoracion_encontrar_empleo", "Year",
               "Evolucion_precio_vivienda", "Valoracion_sit_ec_espana", "Variacion",
               "Evolucion_paro_entorno_6m", "Adquisicion_electrodomestico_grande",
               "Adquisicion_electrodomestico_pequeño", "Adquisicion_mueble",
               "Personas_paro_entorno", "Situacion_laboral", "Intencion_comprar_vivienda",
               "Sexo")

# Crear matriz de variables predictoras
X_train <- train_data[, variables]
X_test <- test_data[, variables]

# Convertir la variable objetivo a tipo numérico
y_train <- as.numeric(as.character(train_data$Confianza_consumidor))
y_test <- as.numeric(as.character(test_data$Confianza_consumidor))

library(ipred)

# Entrenar el modelo de Bagging
model <- bagging(y_train ~ ., data = X_train, nbagg = 100)

# Realizar predicciones en el conjunto de prueba
predictions <- predict(model, newdata = X_test)

# Calcular el RMSE
rmse <- sqrt(mean((predictions - y_test)^2))
print(paste("RMSE:", rmse))

# Calcular el MAE
mae <- mean(abs(predictions - y_test))
print(paste("MAE:", mae))

# Calcular el MAPE
mape <- mean(abs((predictions - y_test) / y_test)) * 100
print(paste("MAPE:", mape))

# Crear tabla de comparación de valores reales y predichos por mes de cada año
result <- data.frame(MES = test_data$MES,
                     Year = test_data$Year,
                     Valor_Real = y_test,
                     Prediccion = predictions)

table_result <- aggregate(cbind(Valor_Real, Prediccion) ~ MES + Year, data = result, mean)
print(table_result)
view(table_result)

# Crear un dataframe con los valores reales y las predicciones
result_plot <- data.frame(Valor_Real = y_test, Prediccion = predictions)

# Graficar los valores reales vs. predicciones
ggplot(result_plot, aes(x = Valor_Real, y = Prediccion)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(x = "Valor Real", y = "Predicción") +
  ggtitle("Comparación de Valores Reales vs. Predicciones (Bagging)")


# MODELO BAGGING 6 --------------------------------------------------------

# Filtrar los datos de entrenamiento y prueba
set.seed(123)
train_data <- df_completo[df_completo$Year %in% 2018:2021, ]
test_data <- df_completo[df_completo$Year == 2022, ]

variables <- c("Confianza_consumidor","Year", "Valoracion_sit_ec_espana", "Evolucion_precio_vivienda", "Valoracion_encontrar_empleo", 
               "Valoracion_prospectiva_espana", "Valoracion_sit_ec_hogar", "Valoracion_mejorar_empleo_esp_6m", 
               "MES", "Variacion", "Evolucion_ahorro_personal")


# Crear matriz de variables predictoras
X_train <- train_data[, variables]
X_test <- test_data[, variables]

# Convertir la variable objetivo a tipo numérico
y_train <- as.numeric(as.character(train_data$Confianza_consumidor))
y_test <- as.numeric(as.character(test_data$Confianza_consumidor))

library(ipred)

# Entrenar el modelo de Bagging
model <- bagging(y_train ~ ., data = X_train, nbagg = 100)

# Realizar predicciones en el conjunto de prueba
predictions <- predict(model, newdata = X_test)

# Calcular el RMSE
rmse <- sqrt(mean((predictions - y_test)^2))
print(paste("RMSE:", rmse))

# Calcular el MAE
mae <- mean(abs(predictions - y_test))
print(paste("MAE:", mae))

# Calcular el MAPE
mape <- mean(abs((predictions - y_test) / y_test)) * 100
print(paste("MAPE:", mape))

# Crear tabla de comparación de valores reales y predichos por mes de cada año
result <- data.frame(MES = test_data$MES,
                     Year = test_data$Year,
                     Valor_Real = y_test,
                     Prediccion = predictions)

table_result <- aggregate(cbind(Valor_Real, Prediccion) ~ MES + Year, data = result, mean)
print(table_result)

# Crear un dataframe con los valores reales y las predicciones
result_plot <- data.frame(Valor_Real = y_test, Prediccion = predictions)

# Graficar los valores reales vs. predicciones
ggplot(result_plot, aes(x = Valor_Real, y = Prediccion)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(x = "Valor Real", y = "Predicción") +
  ggtitle("Comparación de Valores Reales vs. Predicciones (Bagging)")



# PARAMETROS --------------------------------------------------------------
library(ipred)
library(dplyr)

# Establecer la semilla de aleatorización
set.seed(123)

# Definir el número de modelos base y la proporción de muestras de entrenamiento
B <- 100  # Número de modelos base
subsample <- 0.8  # Proporción de muestras de entrenamiento

# Entrenar el modelo de Bagging
model <- bagging(y_train ~ ., data = X_train, B = B, coob = TRUE, boottree = TRUE)
model <- bagging(y_train ~ ., data = X_train, B = B, coob = TRUE, boottree = TRUE, mfinal = 50, allowParallel = TRUE, importance = TRUE)


# Realizar predicciones en el conjunto de prueba
predictions <- predict(model, newdata = X_test)

# Calcular el RMSE, MAE y MAPE
rmse <- sqrt(mean((predictions - y_test)^2))
mae <- mean(abs(predictions - y_test))
mape <- mean(abs((predictions - y_test) / y_test)) * 100

# Crear tabla de comparación de valores reales y predichos por mes de cada año
result <- data.frame(MES = test_data$MES, Year = test_data$Year, Valor_Real = y_test, Prediccion = predictions)
table_result <- aggregate(cbind(Valor_Real, Prediccion) ~ MES + Year, data = result, mean)

# Imprimir los resultados
print(paste("RMSE:", rmse))
print(paste("MAE:", mae))
print(paste("MAPE:", mape))
print(table_result)
View(table_result)

# Crear un dataframe con los valores reales y las predicciones
result_plot <- data.frame(Valor_Real = y_test, Prediccion = predictions)

# Graficar los valores reales vs. predicciones
ggplot(result_plot, aes(x = Valor_Real, y = Prediccion)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(x = "Valor Real", y = "Predicción") +
  ggtitle("Comparación de Valores Reales vs. Predicciones (Bagging)")

