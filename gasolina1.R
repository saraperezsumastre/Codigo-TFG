# LIMPIEZA DE DATOS FINAL DESDE 0
# LIBRERIAS ---------------------------------------------------------------
library(tidyverse)
library(dplyr)
library(tidyr) 
library(readxl)
library(ggplot2)
library(purrr)
library(readr)
library(skimr)

# CARGA DATOS -------------------------------------------------------------

datos_gasolina <- read_xlsx("datos_gasolina.xlsx")

# LIMPIEZA ----------------------------------------------------------------

# Convertir las columnas de fecha a formato de fecha
datos_gasolina$Fecha <- as.Date(paste(datos_gasolina$Día, datos_gasolina$Mes, datos_gasolina$Año), format = "%d %B %Y")

# Verificar y corregir los tipos de datos
datos_gasolina$A95 <- as.numeric(as.character(datos_gasolina$A95))
datos_gasolina$Diésel <- as.numeric(as.character(datos_gasolina$Diésel))
datos_gasolina$GLP <- as.numeric(as.character(datos_gasolina$GLP))

# Eliminar duplicados
datos_gasolina <- unique(datos_gasolina)

# Cambiar el nombre de las variables
# Para quitar tildesy la ñ
names(datos_gasolina)
names (datos_gasolina)[1] = "Dia"
names (datos_gasolina)[3] = "Year"
names (datos_gasolina)[5] = "Diesel"
names(datos_gasolina)

# Identificar filas con valores faltantes
datos_gasolina <- datos_gasolina[complete.cases(datos_gasolina), ]

# Si quisieramos redondear las variables 
# Pero no nos interesa porque nos interesan los cambios en los decimales
# datos_gasolina$A95 <- round(datos_gasolina$A95, 2)

# ANALISIS DE VARIABLES ---------------------------------------------------

# Resumen estadístico
summary(datos_gasolina)

# Histograma de A95
hist(datos_gasolina$A95, main = "Distribución de A95", xlab = "Precio A95")
# Histograma de Diesel
hist(datos_gasolina$Diesel, main = "Distribución de Diesel", xlab = "Precio Diesel")
# Histograma de GLP
hist(datos_gasolina$GLP, main = "Distribución de GLP", xlab = "Precio GLP")

# CALCULO DE ESTADISTICAS POR AÑO 
library(dplyr)
# A95
estadisticas_anuales_A95 <- datos_gasolina %>%
  group_by(Year) %>%
  summarise(PrecioMedio = mean(Diesel), DesviacionEst = sd(Diesel))
# DIESEL
estadisticas_anuales_Diesel <- datos_gasolina %>%
  group_by(Year) %>%
  summarise(PrecioMedio = mean(A95), DesviacionEst = sd(A95))
# GLP
estadisticas_anuales_GLP <- datos_gasolina %>%
  group_by(Year) %>%
  summarise(PrecioMedio = mean(GLP), DesviacionEst = sd(GLP))

# Grafico de lineas para precio medio 
# A95
ggplot(estadisticas_anuales_A95, aes(x = Year, y = PrecioMedio)) +
  geom_line() +
  labs(x = "Año", y = "Precio Medio", title = "Tendencia del precio medio de A95")
# Diesel
ggplot(estadisticas_anuales_Diesel, aes(x = Year, y = PrecioMedio)) +
  geom_line() +
  labs(x = "Año", y = "Precio Medio", title = "Tendencia del precio medio de Diesel")
# GLP
ggplot(estadisticas_anuales_GLP, aes(x = Year, y = PrecioMedio)) +
  geom_line() +
  labs(x = "Año", y = "Precio Medio", title = "Tendencia del precio medio de GLP")

# Analisis de variabilidad anual
estadisticas_anuales_A95$CoefVariacion <- (estadisticas_anuales_A95$DesviacionEst / estadisticas_anuales_A95$PrecioMedio) * 100
#C Comparacion de variabilidad entre años
ggplot(estadisticas_anuales_A95, aes(x = Year, y = DesviacionEst)) +
  geom_bar(stat = "identity", fill = "blue", width = 0.5) +
  labs(x = "Año", y = "Desviación Estándar", title = "Comparación de variabilidad anual de A95")

# COMPARACION DE PRECIOS DE DISTINTOS COMBUSTIBLES

precios_combustible <- datos_gasolina %>%
  group_by(Year) %>%
  summarise(PrecioA95 = mean(A95), PrecioDiesel = mean(Diesel), PrecioGLP = mean(GLP))

ggplot(precios_combustible, aes(x = Year)) +
  geom_bar(aes(y = PrecioA95), stat = "identity", fill = "blue", width = 0.3) +
  geom_bar(aes(y = PrecioDiesel), stat = "identity", fill = "red", width = 0.3) +
  geom_bar(aes(y = PrecioGLP), stat = "identity", fill = "green", width = 0.3) +
  labs(x = "Año", y = "Precio promedio", title = "Comparación de precios de combustible") +
  scale_fill_manual(values = c("blue", "red", "green"),
                    labels = c("A95", "Diésel", "GLP"))

# Correlacion entre a95 y siesel
correlacion <- cor(datos_gasolina$A95, datos_gasolina$Diesel, datos_gasolina)
# Matriz correlacion
matriz_correlacion <- cor(datos_gasolina[, c("A95", "Diesel", "GLP")])
library(reshape2)
library(corrplot)
# Configurar el mapa de correlación
corrplot(matriz_correlacion, method = "color", type = "lower", tl.col = "black", tl.srt = 45)

rm(estadisticas_anuales, df_correlacion_mod, gasolina, df_correlacion)

# VISUALIZACION -----------------------------------------------------------

library(ggplot2)

# Gráfico de línea de los precios de A95 a lo largo del tiempo
ggplot(datos_gasolina, aes(x = Fecha, y = A95)) +
  geom_line() +
  labs(x = "Fecha", y = "Precio A95", title = "Tendencia de precios de A95")

# Gráfico de línea de los precios de A95 a lo largo del tiempo
ggplot(datos_gasolina, aes(x = Fecha, y = A95)) +
  geom_line() +
  labs(x = "Fecha", y = "Precio A95", title = "Tendencia de precios de A95")

# Gráfico de línea de los precios de A95 a lo largo del tiempo
ggplot(datos_gasolina, aes(x = Fecha, y = A95)) +
  geom_line() +
  labs(x = "Fecha", y = "Precio A95", title = "Tendencia de precios de A95")


# Grafico de dispersion A95 Y Diesel
ggplot(datos_gasolina, aes(x = A95, y = Diesel)) +
  geom_point() +
  labs(x = "Precio A95", y = "Precio Diésel", title = "Relación entre precios de A95 y Diésel")

# Grafico de lineas multiples por mes 

ggplot(datos_gasolina, aes(x = Mes, y = A95, group = format(Fecha, "%Y"), color = factor(format(Fecha, "%Y")))) +
  geom_line() +
  labs(x = "Mes", y = "Precio A95", title = "Tendencia mensual de precios de A95") +
  scale_color_discrete(name = "Año")


# Grafico desviacion estandar
# A95
ggplot(estadisticas_anuales_A95, aes(x = Year, y = DesviacionEst)) +
  geom_bar(stat = "identity", fill = "blue", width = 0.5) +
  labs(x = "Año", y = "Desviación Estándar", title = "Desviación Estándar del precio de A95")
# Diesel
ggplot(estadisticas_anuales_Diesel, aes(x = Year, y = DesviacionEst)) +
  geom_bar(stat = "identity", fill = "red", width = 0.5) +
  labs(x = "Año", y = "Desviación Estándar", title = "Desviación Estándar del precio de Diesel")
# GLP
ggplot(estadisticas_anuales_GLP, aes(x = Year, y = DesviacionEst)) +
  geom_bar(stat = "identity", fill = "green", width = 0.5) +
  labs(x = "Año", y = "Desviación Estándar", title = "Desviación Estándar del precio de GLP")


# UNIR --------------------------------------------------------------------

names(indice)[1] = "Year"
names(datos_gasolina)[2] = "MES"
# Realizar la unión de las bases de datos por las columnas año y mes
datos_combinados <- merge(datos_gasolina, indice, by = c("Year", "MES"))

# Definir el orden deseado de los meses
orden_meses <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

# Convertir la columna "Mes" en un factor con el orden personalizado
datos_combinados$Mes <- factor(datos_combinados$Mes, levels = orden_meses)

# Ordenar los datos combinados por año, mes y dia
datos_combinados <- datos_combinados %>%
  arrange(Year, match(Mes, orden_meses), Dia)

# Verificar la estructura y contenido de los datos combinados
str(datos_combinados)
head(datos_combinados)


# EXCEL -------------------------------------------------------------------

# Crear un excel con los datos limpios 
library(openxlsx)
# crea un nuevo archivo de Excel
wb <- createWorkbook()
# agrega una hoja de cálculo al archivo
addWorksheet(wb, "Mi hoja de datos")
# agrega los datos a la hoja de cálculo
writeData(wb, "Mi hoja de datos", datos_combinados)
# aplica formato a las celdas
addStyle(wb, "Mi hoja de datos", 
         style = createStyle(fontColour = "white", fgFill = "#0072C6", halign = "center", textDecoration = "bold"), 
         rows = 1, cols = 1:ncol(datos_combinados))
addStyle(wb, "Mi hoja de datos", 
         style = createStyle(numFmt = "dd/mm/yyyy", halign = "center"), 
         rows = 2:nrow(datos_combinados), cols = "Fecha")
# guarda los cambios y cierra el archivo
saveWorkbook(wb, "Mi archivo.xlsx")

# MODELO AUTOARIMA --------------------------------------------------------

# Crear una serie temporal con los precios de la gasolina y el ICC
serie_temporal <- ts(datos_combinados$Confianza_consumidor, start = c(2018, 1), frequency = 12)

# Dividir la serie temporal en conjunto de entrenamiento y conjunto de prueba
train <- window(serie_temporal, end = c(2021, 12))
test <- window(serie_temporal, start = c(2022, 1))

# Obtener los precios de la gasolina correspondientes al conjunto de entrenamiento
train_gasolina <- datos_combinados$A95[1:length(train)]

# Ajustar un modelo autoarima con los precios de la gasolina como variables predictoras
modelo <- auto.arima(train, xreg = train_gasolina)

summary(modelo)

# Realizar pronósticos utilizando el modelo
pronosticos <- forecast(modelo, xreg = datos_combinados$A95[(length(train) + 1):length(serie_temporal)])

# Imprimir los pronósticos y comparar con los valores reales
print(pronosticos)
plot(pronosticos)
lines(test, col = "red")

# Crear una serie temporal con los precios de la gasolina y la confianza del consumidor
serie_temporal <- ts(datos_combinados$Confianza_consumidor, start = c(2018, 1), frequency = 12)

# Dividir la serie temporal en conjunto de entrenamiento y conjunto de prueba
train <- window(serie_temporal, end = c(2021, 12))
test <- window(serie_temporal, start = c(2022, 1))

# Obtener los precios de la gasolina Diesel correspondientes al conjunto de entrenamiento
train_diesel <- datos_combinados$Diesel[1:length(train)]

# Obtener los precios de la gasolina GLP correspondientes al conjunto de entrenamiento
train_glp <- datos_combinados$GLP[1:length(train)]

# Ajustar un modelo autoarima con los precios de la gasolina Diesel y GLP como variables predictoras
modelo_diesel <- auto.arima(train, xreg = cbind(train_diesel, train_glp))

# Obtener los precios de la gasolina Diesel correspondientes al conjunto de prueba
test_diesel <- datos_combinados$Diésel[(length(train) + 1):length(serie_temporal)]

# Obtener los precios de la gasolina GLP correspondientes al conjunto de prueba
test_glp <- datos_combinados$GLP[(length(train) + 1):length(serie_temporal)]

# Realizar pronósticos utilizando el modelo y las variables predictoras
pronosticos <- forecast(modelo, xreg = cbind(test_diesel, test_glp))

# Imprimir los pronósticos y comparar con los valores reales
print(pronosticos)
plot(pronosticos)
lines(test, col = "red")


# PRUEBA 2 ----------------------------------------------------------------

# Con todas
# Crear una base de datos combinada con las variables relevantes
datos_combinados2 <- inner_join(indice, datos_gasolina, by = c("Year", "Mes"))

# Crear una serie temporal con la variable objetivo (Confianza_consumidor)
serie_temporal <- ts(datos_combinados2$Confianza_consumidor, start = c(2018, 1), frequency = 12)

# Dividir la serie temporal en conjunto de entrenamiento y conjunto de prueba
train <- window(serie_temporal, end = c(2021, 12))
test <- window(serie_temporal, start = c(2022, 1))

# Obtener las variables predictoras correspondientes al conjunto de entrenamiento
train_predictores <- datos_combinados2 %>%
  filter(Year <= 2021) %>%
  select(A95, Diesel, GLP)

# Ajustar un modelo autoarima con las variables predictoras
modelo2 <- auto.arima(train, xreg = train_predictores)
modelo2 <- auto.arima(train, xreg = as.matrix(train_predictores))

# Obtener las variables predictoras correspondientes al conjunto de prueba
test_predictores <- datos_combinados %>%
  filter(Year >= 2022) %>%
  select(A95, Diesel, GLP)

# Realizar pronósticos utilizando el modelo y las variables predictoras
pronosticos2 <- forecast(modelo2, xreg = test_predictores)

# Imprimir los pronósticos y comparar con los valores reales
print(pronosticos2)
plot(pronosticos2)
lines(test, col = "red")


# PRUEBA 3 ----------------------------------------------------------------
library(forecast)
library(tseries)

# Convertir la columna "Fecha" a formato de fecha
# datos_combinados$Fecha <- as.Date(datos_combinados$Fecha)
# Convertir la columna "Confianza_consumidor" a numérica si es necesario
# datos_combinados$Confianza_consumidor <- as.numeric(datos_combinados$Confianza_consumidor)

# Crear una serie temporal usando el precio de la gasolina (por ejemplo, A95)
serie_gasolina <- ts(datos_combinados$A95, frequency = 365)
# ME HA SALIDO ESTE GRAFICO
# Graficar la serie temporal de precios de la gasolina
plot(serie_gasolina, main = "Serie temporal de precios de gasolina", ylab = "Precio de la gasolina", xlab = "Fecha")

# Ajustar el modelo autoARIMA
modelo <- auto.arima(serie_gasolina)
# Realizar predicciones
predicciones <- forecast(modelo, h = 6, level = c(80, 95))  
# Reemplaza 'n' con la cantidad de períodos a predecir

# Graficar las predicciones
plot(predicciones, main = "Predicciones del índice de confianza del consumidor", xlab = "Fecha", ylab = "Índice de confianza del consumidor")
lines(datos_combinados$Fecha, datos_combinados$Confianza_consumidor, col = "blue")
legend("topleft", legend = c("Predicciones", "Datos reales"), col = c("red", "blue"), lty = c(1, 1))

# Asegurar que las series temporales tengan la misma longitud
predicciones <- predicciones$mean
datos_reales <- datos_combinados$Confianza_consumidor[1:length(predicciones)]

# Calcular el error cuadrático medio (MSE)
mse <- mean((predicciones - datos_reales)^2)
mse

# Obtener los residuos del modelo
residuos <- residuals(modelo)

# Graficar los residuos
plot(residuos, main = "Residuos del modelo", ylab = "Residuos")



# NUEVAMENTE --------------------------------------------------------------

# Calcular la correlación entre los precios de la gasolina y la confianza del consumidor
cor_gasolina_confianza <- cor(datos_combinados[, c("A95", "Diesel", "GLP")], datos_combinados$Confianza_consumidor)

# Mostrar la matriz de correlación
cor_gasolina_confianza

# PRUEBA DE CAUSALIDAD
# CODIGO CAUSALIDAD !!!!!!!!!!!!!


# MATRIZ CORRELACION DE PEARSON
# Crear un subconjunto de datos con las variables de interés
datos_gasolina_confianza <- datos_combinados[, c("A95", "Diesel", "GLP", "Confianza_consumidor")]

# Calcular la matriz de correlación de Pearson
cor_gasolina_confianza <- cor(datos_gasolina_confianza, use = "pairwise.complete.obs", method = "pearson")

# Mostrar la matriz de correlación
cor_gasolina_confianza
# Graficar la matriz de correlación con colores
library(corrplot)
corrplot(cor_gasolina_confianza, method = "color", type = "upper", tl.col = "black")



# DATOS INDICE ------------------------------------------------------------

df_completo <- read.csv("df_completo.csv")
# Crear un subconjunto de datos con todas las variables excepto la variable objetivo
datos_interes <- df_completo[, !colnames(df_completo) %in% "Confianza_consumidor"]

# Calcular la matriz de correlación de Pearson
correlaciones <- cor(datos_interes, use = "pairwise.complete.obs", method = "pearson")

# Graficar la matriz de correlación con colores
corrplot(correlaciones, method = "color", type = "upper", tl.col = "black", tl.cex = 0.7)

# Graficar la matriz de correlación como un mapa de calor
heatmap(correlaciones, col = colorRampPalette(c("blue", "white", "red"))(100))

#Otras formas de graficar
# Calcular la matriz de correlación de Pearson
correlaciones1 <- cor(df_completo[, -which(names(df_completo) == "Confianza_consumidor")], method = "pearson")

# Graficar la matriz de correlación como un mapa de calor con etiquetas
heatmap(correlaciones1, col = colorRampPalette(c("blue", "white", "red"))(100),
        main = "Matriz de correlación", xlab = "Variables", ylab = "Variables",
        cex.main = 1.2, cex.axis = 0.8, cex.lab = 0.8)

# Convertir la matriz de correlación en un data frame
df_correlaciones <- as.data.frame(as.table(correlaciones))
names(df_correlaciones) <- c("Variable1", "Variable2", "Correlacion")

# Graficar la matriz de correlación como un gráfico de mosaico
ggplot(df_correlaciones, aes(Variable1, Variable2, fill = Correlacion)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", na.value = "gray") +
  labs(title = "Matriz de correlación", x = "Variables", y = "Variables") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# NUEVA PRUEBA AUTOARIMA --------------------------------------------------

# Crear una serie temporal con los precios de gasolina y el ICC
serie_gasolina <- ts(datos_combinados[, c("A95", "GLP", "Diesel")], frequency = 12)
serie_icc <- ts(datos_combinados$Confianza_consumidor, frequency = 12)

# Ajustar un modelo autoarima para cada serie de precios de gasolina
modelo_a95 <- auto.arima(serie_gasolina[, "A95"])
modelo_glp <- auto.arima(serie_gasolina[, "GLP"])
modelo_diesel <- auto.arima(serie_gasolina[, "Diesel"])

# Ajustar un modelo autoarima para el ICC
modelo_icc <- auto.arima(serie_icc)

# Obtener los residuos del modelo del ICC
residuos_icc <- residuals(modelo_icc)

# Visualizar los modelos ajustados
plot(modelo_a95, main = "Modelo ARIMA - A95")
plot(modelo_glp, main = "Modelo ARIMA - GLP")
plot(modelo_diesel, main = "Modelo ARIMA - Diesel")
plot(modelo_icc, main = "Modelo ARIMA - Confianza_consumidor")

# Verificar si los residuos del ICC están correlacionados con los precios de gasolina
ccf(residuos_icc, serie_gasolina[, "A95"], lag.max = 12)
ccf(residuos_icc, serie_gasolina[, "GLP"], lag.max = 12)
ccf(residuos_icc, serie_gasolina[, "Diesel"], lag.max = 12)

# Obtener los errores de los modelos ajustados
errores_a95 <- residuals(modelo_a95)
errores_glp <- residuals(modelo_glp)
errores_diesel <- residuals(modelo_diesel)
errores_icc <- residuals(modelo_icc)

# Visualizar los errores
plot(errores_a95, main = "Errores del modelo ARIMA - A95")
plot(errores_glp, main = "Errores del modelo ARIMA - GLP")
plot(errores_diesel, main = "Errores del modelo ARIMA - Diesel")
plot(errores_icc, main = "Errores del modelo ARIMA - ICC")

# Calcular el RMSE
rmse_icc <- sqrt(mean(errores_icc^2))
print(rmse_icc)
# Calcular el MAE
mae_icc <- mean(abs(errores_icc))
print(mae_icc)


# PARA REDUCIR EL ERROR ---------------------------------------------------

# SELECCION DE VARIABLES 
# Seleccionar solo las variables relevantes
serie_gasolina_reducida <- datos_combinados[, c("A95", "Diesel")]  # Seleccionar A95 y Diesel
serie_gasolina_reducida <- ts(serie_gasolina_reducida, frequency = 12)

# Crear una lista para almacenar los modelos autoarima de las variables de gasolina
modelos_gasolina <- list()

# Ajustar un modelo autoarima para cada variable de gasolina
for (variable in c("A95", "Diesel")) {
  serie_gasolina <- ts(datos_combinados[, variable], frequency = 12)
  modelo_gasolina <- auto.arima(serie_gasolina)
  modelos_gasolina[[variable]] <- modelo_gasolina
}

# Ajustar un modelo autoarima para la serie de precios de gasolina reducida
modelo_gasolina_reducida <- auto.arima(serie_gasolina_reducida)
modelo_gasolina1 <- modelo_gasolina
modelo_gasolina1 <- auto.arima(modelo_gasolina1)



# EVALUACION Y COMPARACION DE MODELOS
# Validación cruzada para evaluar el rendimiento del modelo
cv <- cv.ts(serie_gasolina, FUN = function(x) auto.arima(x)$aic)
best_model <- auto.arima(serie_gasolina, stepwise = FALSE, approximation = FALSE, xreg = NULL, order = c(2,1,0))

# Comparar modelos utilizando criterios de información
modelo_1 <- auto.arima(serie_gasolina, ic = "aic")
modelo_2 <- auto.arima(serie_gasolina, ic = "bic")



# ARIMA MANUAL ------------------------------------------------------------

# Ajustar manualmente los parámetros del modelo autoarima
# modelo_gasolina_manual <- Arima(serie_gasolina, order = c(p, d, q), seasonal = list(order = c(P, D, Q), period = 12))
modelo_gasolina_manual <- Arima(serie_gasolina, order = c(1, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12))

# Obtener un resumen del modelo ajustado
summary(modelo_gasolina_manual)
# Obtener los residuos del modelo
residuos_gasolina <- residuals(modelo_gasolina_manual)

# Graficar el histograma de los residuos
hist(residuos_gasolina, main = "Histograma de los residuos")
# Graficar los residuos estandarizados
plot(residuos_gasolina, which = 1)
# Graficar el gráfico de autocorrelación de los residuos
plot(modelo_gasolina_manual, which = 2)
# Graficar el histograma de los residuos
plot(modelo_gasolina_manual, which = 3)

# ESTO NO ESTA MAL 
# Generar pronósticos futuros
pronostico_gasolina1 <- forecast(modelo_gasolina_manual, h = 12)  # Pronóstico para los próximos 12 periodos
# Graficar los pronósticos
plot(pronostico_gasolina1)

# COEFICIENTES
# Obtener los coeficientes del modelo ARIMA
coeficientes <- coef(modelo_gasolina_manual)
# Visualizar los coeficientes y su significancia
print(coeficientes)
# Obtener los intervalos de confianza para los coeficientes
conf_int <- confint(modelo_gasolina_manual)
# Visualizar los intervalos de confianza
print(conf_int)

# COEFICIENTES
# Obtener los coeficientes del modelo ARIMA
coeficientes_icc <- coef(modelo_icc)
# Visualizar los coeficientes y su significancia
print(coeficientes_icc)
# Obtener los intervalos de confianza para los coeficientes
conf_int_icc <- confint(modelo_icc)
# Visualizar los intervalos de confianza
print(conf_int_icc)


# RPUBS -------------------------------------------------------------------

tsdata1 <-ts(datos_combinados,start=c(2018,1),frequency=12)
plot(tsdata1)
componentes.ts = decompose(tsdata1)
plot(componentes.ts)
library("fUnitRoots")
urkpssTest(tsdata1, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)
tsstationary = diff(tsdata1, differences=1)
plot(tsstationary)

# AUTOCORRELACION
# acf(tsdata1,lag.max=140)
# par(mar = c(5, 4, 4, 2) + 0.1)  # Ajustar los márgenes de la figura
# Calcular la autocorrelación manualmente
lags <- 1:140  # Define los lags que deseas calcular
autocorrelation <- sapply(lags, function(lag) cor(tsdata1[-length(tsdata1)], tsdata1[-1]))
# Visualizar los resultados
plot(lags, autocorrelation, type = "h", xlab = "Lag", ylab = "Autocorrelation", main = "Autocorrelation Plot")

modelo_rpubs <-auto.arima(tsdata1[,2])
summary(modelo_rpubs)

pronostico_rpubs <-forecast(modelo_rpubs,12,level=95)
plot(pronostico_rpubs,main="Pronóstico con auto.arima")
# ESTA LINEA DE CODIGO 
matriz.pronosticos<-data.frame(pronostico_rpubs$mean,pronostico_rpubs$lower,pronostico_rpubs$upper)


# MODELO 2
fitARIMA <- arima(tsdata1, order=c(1,1,1),seasonal = list(order = c(1,0,0), period = 12),method="ML")
#install.packages("lmtest")
library(lmtest)
coeftest(fitARIMA)


