
# LIBRERIAS ---------------------------------------------------------------

# Cargar la biblioteca de análisis de series de tiempo
library(forecast)

# Crear una serie de tiempo a partir de los datos
serie_tiempo <- ts(df_reducido$Confianza_consumidor, start = c(2018, 1), frequency = 12)

# Ajustar el modelo ARIMA
modelo_arima <- arima(serie_tiempo, order = c(p, d, q))
plot(serie_tiempo)

# PRUEBA 2 ----------------------------------------------------------------

# Cargar bibliotecas necesarias
library(forecast)
library(ggplot2)

# Crear una serie de tiempo a partir de los datos
serie_tiempo <- ts(df_reducido$Confianza_consumidor, start = c(2018, 1), frequency = 12)
serie_tiempo <- ts(indice3$Confianza_consumidor, start = c(2005, 1), frequency = 12)

# Ajustar el modelo ARIMA
modelo_arima <- auto.arima(serie_tiempo)

# Obtener pronóstico
pronostico <- forecast(modelo_arima, h = 12)  # Pronóstico de 12 periodos (meses) hacia adelante

# Visualizar el pronóstico
plot(pronostico, main = "Pronóstico de la confianza del consumidor")

# Obtener las métricas de error
error <- accuracy(pronostico)
print(error)

# Graficar los residuos del modelo
residuos <- residuals(modelo_arima)
ggplot() +
  geom_line(aes(x = time(serie_tiempo), y = residuos), color = "blue") +
  labs(x = "Tiempo", y = "Residuos", title = "Residuos del modelo ARIMA")

# Graficar el ajuste del modelo
ajuste <- fitted(modelo_arima)
ggplot() +
  geom_line(aes(x = time(serie_tiempo), y = serie_tiempo), color = "black") +
  geom_line(aes(x = time(serie_tiempo), y = ajuste), color = "red") +
  labs(x = "Tiempo", y = "Confianza del consumidor", title = "Ajuste del modelo ARIMA")

# Obtener las últimas predicciones
ultimas_predicciones <- tail(pronostico$mean, 12)
print(ultimas_predicciones)


# PRUEBA 3 ----------------------------------------------------------------

# Crear una serie de tiempo a partir de los datos
serie_tiempo <- ts(df_reducido$Confianza_consumidor, start = c(2018, 1), frequency = 12)
serie_tiempo <- ts(indice3$Confianza_consumidor, start = c(2005, 1), frequency = 12)

# Filtrar los datos para el período de 2005-2021
serie_tiempo <- ts(indice3$Confianza_consumidor, start = c(2005, 1), end = c(2021, 12), frequency = 12)


p <- 1
d <- 0
q <- 1

# Ajustar el modelo ARIMA con parámetros específicos
modelo_arima <- arima(serie_tiempo, order = c(p, d, q))

# Obtener pronóstico
pronostico <- forecast(modelo_arima, h = 12)  # Pronóstico de 12 periodos (meses) hacia adelante

# Visualizar el pronóstico
plot(pronostico, main = "Pronóstico de la confianza del consumidor")

# Obtener las métricas de error
error <- accuracy(pronostico)
print(error)

# Graficar los residuos del modelo
residuos <- residuals(modelo_arima)
print(residuos)
ggplot() +
  geom_line(aes(x = time(serie_tiempo), y = residuos), color = "blue") +
  labs(x = "Tiempo", y = "Residuos", title = "Residuos del modelo ARIMA")

# Graficar el ajuste del modelo
ajuste <- fitted(modelo_arima)
print(ajuste)
ggplot() +
  geom_line(aes(x = time(serie_tiempo), y = serie_tiempo), color = "black") +
  geom_line(aes(x = time(serie_tiempo), y = ajuste), color = "red") +
  labs(x = "Tiempo", y = "Confianza del consumidor", title = "Ajuste del modelo ARIMA")

# Obtener las últimas predicciones
ultimas_predicciones <- tail(pronostico$mean, 12)
print(ultimas_predicciones)

# MODELO SARIMA -----------------------------------------------------------

# Cargar paquetes necesarios
library(forecast)

# Convertir los datos de serie de tiempo en una serie temporal en R
serie_tiempo <- ts(df_reducido$Confianza_consumidor, start = c(2018, 1), frequency = 12)

# Ajustar un modelo SARIMA
modelo_sarima <- auto.arima(serie_tiempo)

# Generar predicciones con el modelo ajustado
predicciones <- forecast(modelo_sarima, h = 12, start = c(2022, 1))

# Imprimir las predicciones
print(predicciones)


# MODELO 5 ----------------------------------------------------------------


# INES --------------------------------------------------------------------
library(zoo)

# Representamos la serie con ‘timeseries’
dt <- seq(from=as.Date("2018-01-01"), by='month', length.out=60) # Son 5 años en total
s <- zoo(x=df_completo$Confianza_consumidor, order.by=dt)
autoplot(s) + ggtitle('Confianza del consumidor entre 2018 y 2022') + xlab("Meses") + ylab("ICC")


# Crear secuencia de fechas mensuales
dt <- seq(as.Date("2018-01-01"), by = "month", length.out = 60)

# Crear objeto zoo con la variable de confianza del consumidor
s <- zoo(x = df_completo$Confianza_consumidor, order.by = dt)

# Graficar la serie temporal
autoplot(s) +
  ggtitle("Confianza del Consumidor entre 2007 y 2011") +
  xlab("Meses") +
  ylab("Confianza del Consumidor")


# Crear secuencia de fechas mensuales
dt <- seq(as.Date("2007-07-01"), by = "month", length.out = 60)

# Crear objeto zoo con la variable de confianza del consumidor
s <- zoo(x = as.numeric(as.character(df_reducido$Confianza_consumidor)), order.by = dt)

# Graficar la serie temporal
autoplot.zoo(s) +
  ggtitle("Confianza del Consumidor entre 2007 y 2011") +
  xlab("Meses") +
  ylab("Confianza del Consumidor")

# Crear secuencia de fechas mensuales
dt <- seq(as.Date("2018-01-01"), by = "month", length.out = 60)

# Crear objeto zoo con la variable de confianza del consumidor
s <- zoo(x = df_reducido$Confianza_consumidor, order.by = dt)

# Graficar la serie temporal
autoplot(s) +
  ggtitle("Confianza del Consumidor entre 2007 y 2011") +
  xlab("Meses") +
  ylab("Confianza del Consumidor")

# no sale

# Crear secuencia de fechas mensuales
dt <- seq(as.Date("2018-01-01"), by = "day", length.out = 365*5)

# Crear objeto zoo con la variable de confianza del consumidor
s <- zoo(x = df_completo$Confianza_consumidor, order.by = dt)

# Graficar la serie temporal
autoplot.zoo(s) +
  ggtitle("Confianza del Consumidor entre 2007 y 2011") +
  xlab("Meses") +
  ylab("Confianza del Consumidor")

# ESTO SOLO PARA EL GREAFICO 
df_completo2 <- df_completo
# Eliminar filas duplicadas basadas en la columna "MES"
df_completo2 <- distinct(df_completo2, MES, .keep_all = TRUE)
# Convertir la columna "MES" a formato de fecha
df_completo2$MES <- as.Date(paste(df_completo2$Year, df_completo2$MES, "1", sep = "-"))
# Crear la serie temporal con los datos de Confianza_consumidor
s <- zoo(df_completo2$Confianza_consumidor, order.by = df_completo2$MES)
# Plot de la serie temporal
autoplot(s) +
  ggtitle("Confianza del Consumidor de 2018 a 2022") +
  xlab("Fecha") +
  ylab("Confianza del Consumidor")

# Vemos que se produce una gran bajada de la confianza en 2019-2021
serie <- ts(df_completo2$MES[,2], start=c(2019,1), frequency=12)

# Convertir la columna "MES" a formato de fecha
df_completo2$MES <- as.Date(paste(df_completo2$Year, df_completo2$MES, "1", sep = "-"))

# Crear la serie temporal con los datos de Confianza_consumidor
s <- zoo(df_completo2$Confianza_consumidor, order.by = df_completo2$MES)

# Plot de la serie temporal
autoplot(s) +
  ggtitle("Confianza del Consumidor de 2018 a 2022") +
  xlab("Fecha") +
  ylab("Confianza del Consumidor")


autoplot(serie) + ggtitle('Precios de las casas en el barrio 2914 entre 2007 y 2019') +
  xlab("Meses") + ylab("Precios") + autolayer(ma(serie, 12)) + autolayer(ma(serie, 6)) +
  autolayer(ma(serie, 3))



# MODELO ARIMA  -----------------------------------------------------------

# CON la valoracion de la situacion de españa, el precio de la vivienda
#y la valoracion de encontrar empleo 

# Paso 2: Crear una serie de tiempo
serie_tiempo <- ts(df_completo$Confianza_consumidor, frequency = 12)

# Paso 3: Dividir los datos en conjuntos de entrenamiento y prueba
datos_entrenamiento <- window(serie_tiempo, start = c(2018, 1), end = c(2021, 12))
datos_prueba <- window(serie_tiempo, start = c(2022, 1))

# Paso 4: Convertir las variables seleccionadas en una matriz numérica
variables_explicativas <- as.matrix(df_completo[1:48, c("Valoracion_sit_ec_espana", "Evolucion_precio_vivienda", "Valoracion_encontrar_empleo")])

# Paso 5: Cargar el paquete forecast
library(forecast)

# Paso 6: Crear un modelo ARIMA con las variables seleccionadas
modelo <- auto.arima(datos_entrenamiento, xreg = variables_explicativas)

# Paso 7: Realizar predicciones
predicciones <- forecast(modelo, xreg = as.matrix(df_completo[49:60, c("Valoracion_sit_ec_espana", "Evolucion_precio_vivienda", "Valoracion_encontrar_empleo")]))

# Paso 8: Analizar los resultados
print(predicciones)



# Paso 1: Importar los datos y prepararlos
# Supongamos que ya has importado los datos en el objeto df_completo

# Paso 2: Crear una serie de tiempo
serie_tiempo <- ts(df_completo$Confianza_consumidor, frequency = 12)

# Paso 3: Dividir los datos en conjuntos de entrenamiento y prueba
datos_entrenamiento <- window(serie_tiempo, start = c(2018, 1), end = c(2021, 12))
datos_prueba <- window(serie_tiempo, start = c(2022, 1))

# Paso 4: Cargar el paquete forecast
library(forecast)

# Paso 5: Crear un modelo ARIMA
modelo <- auto.arima(datos_entrenamiento)

# Paso 6: Realizar predicciones
predicciones <- forecast(modelo, h = 12)

# Paso 7: Analizar los resultados
print(predicciones)



library(forecast)

# Crear un dataframe con las variables explicativas y la variable objetivo
datos <- df_completo[, c("Confianza_consumidor", "Valoracion_sit_ec_espana", "Evolucion_precio_vivienda", "Valoracion_encontrar_empleo")]

# Dividir los datos en entrenamiento y prueba
datos_entrenamiento <- datos[1:48, ]
datos_prueba <- datos[49:60, ]

# Convertir las variables explicativas a numéricas
datos_entrenamiento$Valoracion_sit_ec_espana <- as.numeric(datos_entrenamiento$Valoracion_sit_ec_espana)
datos_entrenamiento$Evolucion_precio_vivienda <- as.numeric(datos_entrenamiento$Evolucion_precio_vivienda)
datos_entrenamiento$Valoracion_encontrar_empleo <- as.numeric(datos_entrenamiento$Valoracion_encontrar_empleo)

# Crear el modelo ARIMA con variables exógenas
modelo <- auto.arima(datos_entrenamiento$Confianza_consumidor, xreg = as.matrix(datos_entrenamiento[, c("Valoracion_sit_ec_espana", "Evolucion_precio_vivienda", "Valoracion_encontrar_empleo")]))

# Realizar predicciones utilizando las variables exógenas
predicciones <- forecast(modelo, xreg = datos_prueba[, c("Valoracion_sit_ec_espana", "Evolucion_precio_vivienda", "Valoracion_encontrar_empleo")])

# Imprimir las predicciones
print(predicciones)


# ARIMA CECI --------------------------------------------------------------
# CARGA DE DATOS
# Se cargan librerías
library(readxl)
library(forecast)
library(tseries)
library(dplyr)
library(foreign)
library(lmtest)
library(ggplot2)
indice2 <- read.csv("evolucion_de_la_confianza_del_consumidor.csv", sep = ";")
names (indice2)[1] = "Year"
names (indice2)[2] = "MES"
names (indice2)[3] = "Confianza_consumidor"

# añadimos columna que sea la variacion con respecto al año anterior
# Verificar el tipo de datos de la columna Confianza_consumidor
class(indice2$Confianza_consumidor)
# tipo character
# hay que hacer que el separador no sea , sino .

# Reemplazar las comas por puntos en la columna de datos de confianza del consumidor
indice2$Confianza_consumidor <- as.numeric(gsub(",", ".", indice2$Confianza_consumidor))
# Verificar que la variable "Confianza_consumidor" ahora es numérica
str(indice2)

# Calcular la variación del índice
Variacion <- c(NA, diff(indice2$Confianza_consumidor) / indice2$Confianza_consumidor[-length(indice2$Confianza_consumidor)])

# Crear una nueva columna con la variación
indice2$Variacion <- round(Variacion * 100, 2)
# Eliminar la antigua columna de Variacion y cambiar el nombre para que sea la actual 
indice2$Hola <- NULL

# El primer valor del indice da NA porque no estaba la observacion de 2017
# sustituir el NA por el valor que es la variacion
indice2$Variacion <- ifelse(is.na(indice2$Variacion), 0.64, indice2$Variacion)

#PARA SIN LA VARIACION
indice3 <- read_excel("Nuevo_indice_arima.xlsx")
indice3$Confianza_consumidor <- as.numeric(gsub(",", ".", indice3$Confianza_consumidor))
serie <- indice3

#Lee el archivo y los mete en la variable "serie"
serie <- indice2

#Se obtiene una serie temporal de la columna donde aparecen los datos
serie.ts = ts(serie$Confianza_consumidor, start = c(2005,1), end = c(2022,12), frequency = 12)
print(serie.ts)
plot(serie.ts, main = 'Evolución ICC (2018-2022)', xlab='Años', ylab='ICC')

#Descomponer una serie
serie.ts.desc = decompose(serie.ts)
plot(serie.ts.desc, xlab = 'Años')

#comprobar los estadísticos básicos
summary(serie.ts)

# Eliminar 2022 para usarlos luego en la predicción
serie_icc = ts(serie$Confianza_consumidor, start = c(2005,1), end = c(2021,12), frequency = 12)
print(serie_icc)
plot(serie_icc, main = 'Evolución IPC (2005-2021)', xlab='Años', ylab='ICC')

#se comprueba que se trata de una serie que no es estacionaria en media, 
#dado que presenta una tendencia claramente creciente. 

# Aplicar logaritmos a la serie para reducir la varianza de esta.
serie_icc_log <- log(serie_icc)


#Se vuelve a verificar si la serie transformada sigue siendo o no estacionaria en media.
#Para ello se grafica
plot(serie_icc_log, main = 'Serie ICC transformada en logaritmos', xlab='Años')

#Dickey-fuller para comprobarlo
#H0: la serie  posee raíces unitarias (no es estacionaria).
#H1: La serie es estacionaria
adf.test(serie_icc_log)

# p-valor(0.2074) > nivel de significación(0.05). Por tanto, no es estacionaria

#Se aplican primeras diferencias para ver si se consigue la estacionariedad en la media.
d.serie <- diff(serie_icc_log)
print(d.serie)
plot(d.serie, main = 'Serie aplicada diferencias', xlab='Años')


#Dickey Fuller
adf.test(d.serie)
# p-valor(0.01) < nivel de significación(0.05). Por tanto, se rechaza
# la hipótesis nula y la serie es estacionaria
kpss.test(serie_icc_log, null = "Trend")
kpss.test(d.serie, null = "Trend")


#IDENTIFICACIÓN DEL MODELO

#Correlogramas

par(mfrow=c(1,2))
# VOLVER
#par(mfrow = c(1, 1))

#Función de Autocorrelación (FAC)
acf(d.serie, main="FAC d.serie")

# Función de Autocorrelación Parcial (FACP)
pacf(d.serie, main="FACP d.serie")

#autoarima para que nos guie que modelo probar
arima1 <- auto.arima(d.serie, seasonal = FALSE)
modelo_arima <- auto.arima(d.serie)
print(arima1)
print(modelo_arima)

#Se utilizan criterios estadísticos de información para valorar el mejor modelo. 
modelo1_arima <- arima(d.serie, order=c(1,2,1))
modelo2_arima <- arima(d.serie, order=c(0,1,4))
modelo3_arima <- arima(d.serie, order=c(1,1,3))

#Criterio de Akaike (AIC)
AIC(modelo1_arima)
AIC(modelo2_arima)
AIC(modelo3_arima)

# Criterio Bayesiano (BIC)
BIC(modelo1_arima)
BIC(modelo2_arima)
BIC(modelo3_arima)

#Como los reultados son negativos, se elige modelo con el valor más bajo (es decir, el más negativo),
#ya que esto indica un mejor ajuste del modelo a los datos. 
#en este caso el modelo3. Por tanto, a partir de ahora se va a trabajar con un Arima(1,1,3)

# Estimación de los coeficientes de los modelos: Una vez elegido el modelo, se puede pasar a la
# siguiente fase, la estimación de los coeficientes del modelo

summary(modelo3_arima)
summary(modelo1_arima)
#Contraste de validez del modelo
# para valorar el modelo seleccionado se va a realizar el contraste de significación de los modelos.
# Para ello se formula la siguiente hipótesis:
# H0: El parámetro no es significativo.
# H1: El parámetro es significativo.

coef(modelo3_arima)
coeftest(modelo3_arima)

coef(modelo1_arima)
coeftest(modelo1_arima)

#ANALISIS DETALLADO DE LOS ERRORES
# Para que se pueda utilizar el modelo para hacer predicciones, se debe verificar que los
# residuos son un ruido blanco, es decir, que tienen de media 0 y no están correlacionados
# Diagnostico de residuos

error3 <- residuals(modelo3_arima)
mean(error3)

error1 <- residuals(modelo1_arima)
mean(error1)
# La media es prácticamente 0, por lo que se puede deducir que los residuos cumplen el
# primer requisito para ser ruido blanco. 


# Se realiza la prueba de Ljung-Box para saber si los errores
# están correlacionados y verificar el segundo criterio.
# Para este test se formula la siguiente hipótesis:
# H0: Los errores no están correlacionados
# H1: Los errores están correlacionados

Box.test(residuals(modelo3_arima), type="Ljung-Box")
# p-valor (0.8536) es menor a cualquier nivel de significación. 
Box.test(residuals(modelo1_arima), type="Ljung-Box")
# p-valor (0.002) es menor a cualquier nivel de significación. 

# Realiza la prueba de Jarque-Bera
test_jb3 <- jarque.bera.test(error3)
print(test_jb3)
test_jb1 <- jarque.bera.test(error1)
print(test_jb1)

# Prediccion
predict <- forecast(serie_icc, h=12)   # 1 año 
print(predict)
plot(predict, main = 'Predicción', xlab = 'Años', ylab = 'ICC' )
# Predicciones con el modelo ARIMA seleccionado (modelo3_arima en este caso)
predicciones <- predict(modelo3_arima, n.ahead = 12)  # Predicciones para 10 periodos futuros
print(predicciones)
plot(predicciones, main = 'Predicción', xlab = 'Años', ylab = 'ICC' )


predict <- forecast(serie_icc, h=12)   
print(predict)
plot(predict, main = 'Predicción', xlab = 'Años', ylab = 'ICC' )

plot(modelo3_arima)
plot(modelo1_arima)


# Ajustar el modelo ARIMA
modelo_arima <- auto.arima(d.serie)  # Ajustar automáticamente el modelo ARIMA
# Si deseas ajustar un modelo específico, puedes usar:
# modelo_arima <- arima(d.serie, order=c(p, d, q))  # Reemplaza p, d, q con los valores adecuados

# Obtener las predicciones del modelo ARIMA
predicciones <- forecast(modelo_arima, h=24)  # Predicciones para 2 periodos futuros 

# Imprimir las predicciones
print(predicciones)

# Graficar las predicciones
plot(predicciones, main='Predicción ARIMA', xlab='Años', ylab='ICC')

# EVALUACION CRUZADA DEL MODELO ARIMA -------------------------------------

# Crear índice de división
n <- length(serie_icc)
train_end <- floor(n * 0.8)
test_start <- train_end + 1

# Dividir los datos en entrenamiento y prueba
train_arima <- serie_icc[1:train_end]
test_arima <- serie_icc[test_start:n]

# Ajustar el modelo ARIMA en los datos de entrenamiento
modelo_arima <- auto.arima(train_arima)

# Evaluación cruzada (forecast accuracy)
accuracy <- forecast::accuracy(modelo_arima)

# Predicción en datos de prueba
predicciones <- forecast::forecast(modelo_arima, h = length(test_arima))

# Calcular los errores de predicción en los datos de prueba
errores <- test_arima - predicciones$mean

# Calcular las medidas de rendimiento en los datos de prueba
mae <- mean(abs(errores))
rmse <- sqrt(mean(errores^2))

# Imprimir los resultados
print(accuracy)
cat("MAE:", mae, "\n")
cat("RMSE:", rmse, "\n")
