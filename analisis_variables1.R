# Librerias ---------------------------------------------------------------
library(dplyr)
library(tidyverse)
library(tidyr) 
library(ggplot2)

# GRAFICOS ----------------------------------------------------------------
library(ggplot2)

# grafico de observaciones por mes y año 
ggplot(df_completo, aes(x = Year, fill = factor(Year))) +
  geom_bar(position = "dodge")
table(df_completo$Year, df_completo$MES)

ggplot(df_completo, aes(x = Confianza_consumidor, fill = factor(Year))) +
  geom_bar(position = "dodge")

# Crear gráfico de barras agrupadas para la confianza del consumidor por cada año
ggplot(df_completo, aes(x = factor(Year), y = Confianza_consumidor, fill = factor(Year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Año", y = "Confianza del consumidor") +
  ggtitle("Confianza del Consumidor por Año")

# ver frecuencia por diferentes variables 
ggplot(df_completo, aes(x = Edad)) +
  geom_histogram()

# Histograma de la variable "edad"
ggplot(df_completo, aes(x = Edad)) +
  geom_histogram(binwidth = 5, fill = "lightblue", col = "black") +
  labs(x = "Edad", y = "Frecuencia")
# Habria que hacerlo con todas las variables 

# Variable sexo
ggplot(df_completo, aes(x = Sexo)) +
  geom_histogram(binwidth = 3, fill = "lightblue", col = "black") +
  labs(x = "Sexo", y = "Frecuencia")

# Variable Comunidad
ggplot(df_completo, aes(x = Comunidad)) +
  geom_histogram(binwidth = 1, fill = "lightblue", col = "black") +
  labs(x = "Comunidad", y = "Frecuencia")

# Variable Tamaño del habitat
ggplot(df_completo, aes(x = Tamano_Habitat)) +
  geom_histogram(binwidth = 1, fill = "lightblue", col = "black") +
  labs(x = "Tamano_Habitat", y = "Frecuencia")

# Relacion entre variables numericas
# Gráfico de dispersión de "edad" vs "situacion economica"
ggplot(df_completo, aes(x = Edad, y = Valoracion_sit_ec_hogar)) +
  geom_point(alpha = 0.5, col = "darkred") +
  labs(x = "Edad", y = "Valoracion situacion economica")

# Relacion entre variable numerica y categorica
# Boxplot de "tamano_habitat" por "composicion_hogar"
ggplot(df_completo, aes(x = Composicion_hogar, y = Tamano_Habitat)) +
  geom_boxplot(fill = "lightblue", alpha = 0.5) +
  labs(x = "Composición del hogar", y = "Tamaño de hábitat")

# Grafico de variable a lo largo del tiempo 
# Gráfico de líneas de "tamano_habitat" por mes y año
ggplot(df_completo, aes(x = MES, y = Valoracion_sit_ec_hogar, group = Year, col = factor(Year))) +
  geom_line() +
  labs(x = "Mes", y = "Valoracion situacion economica del hogar", col = "Año")
# RARISIMO ESTE GRAFICO 

ggplot(df_completo, aes(x = Confianza_consumidor, y = Intencion_comprar_vivienda)) +
  geom_point(alpha = 0.5, col = "darkred") +
  labs(x = "Confianza del consumidor", y = "Intencion_comprar_vivienda")
# Esperar q tarda un poco

# Crear el gráfico de línea para la confianza del consumidor
ggplot(df_completoA, aes(x = fecha, y = Confianza_consumidor)) +
  geom_line() +
  labs(x = "Fecha", y = "Confianza del Consumidor") +
  ggtitle("Variación de la Confianza del Consumidor")


# JUNTAR GRAFICOS  --------------------------------------------------------
# LO ESTAS HACIENDO CON DF_REDUCIDO
library(ggplot2)
library(gridExtra)

# Histograma de la variable "edad"
hist_edad <- ggplot(df_reducido, aes(x = Edad)) +
  geom_histogram(binwidth = 5, fill = "lightblue", col = "black") +
  labs(x = "Edad", y = "Frecuencia")

# Otros histogramas para diferentes variables
hist_sexo <- ggplot(df_reducido, aes(x = Sexo)) +
  geom_histogram(binwidth = 3, fill = "lightblue", col = "black") +
  labs(x = "Sexo", y = "Frecuencia")

hist_Comunidad <- ggplot(df_reducido, aes(x = Comunidad)) +
  geom_histogram(binwidth = 1, fill = "lightblue", col = "black") +
  labs(x = "Comunidad", y = "Frecuencia")

hist_Habitat <- ggplot(df_reducido, aes(x = Tamano_Habitat)) +
  geom_histogram(binwidth = 1, fill = "lightblue", col = "black") +
  labs(x = "Tamaño Habitat", y = "Frecuencia")

# Mostrar los gráficos juntos
grid.arrange(hist_edad, hist_sexo, hist_Comunidad, hist_Habitat,
             nrow = 2, ncol = 2)

names(df_reducido)
# Provincia
hist_provincia <- ggplot(df_reducido, aes(x = Provincia)) +
  geom_histogram(binwidth = 5, fill = "lightblue", col = "black") +
  labs(x = "Provincia", y = "Frecuencia")
# Situacion_economia_hogar
hist_sit_ec_hogar <- ggplot(df_reducido, aes(x = Situacion_economia_hogar)) +
  geom_histogram(binwidth = 1, fill = "lightblue", col = "black") +
  labs(x = "Situacion de la economia del hogar", y = "Frecuencia")
# Valoracion_sit_ec_hogar
hist_valoracion_sit_ec_hogar <- ggplot(df_reducido, aes(x = Valoracion_sit_ec_hogar)) +
  geom_histogram(binwidth = 1, fill = "lightblue", col = "black") +
  labs(x = "Valoracion de la situacion economica del hogar", y = "Frecuencia")
# Personas_paro_entorno
hist_paro_entorno <- ggplot(df_reducido, aes(x = Personas_paro_entorno)) +
  geom_histogram(binwidth = 3, fill = "lightblue", col = "black") +
  labs(x = "Personas en paro de su entorno", y = "Frecuencia")

# Mostrar los gráficos juntos
grid.arrange(hist_provincia, hist_sit_ec_hogar, hist_valoracion_sit_ec_hogar, hist_paro_entorno,
             nrow = 2, ncol = 2)

names(df_reducido)
# Evolucion paro
hist_evolucion_paro <- ggplot(df_reducido, aes(x = Evolucion_paro_entorno_6m)) +
  geom_histogram(binwidth = 1, fill = "lightblue", col = "black") +
  labs(x = "Evolución del paro en su entorno en 6 meses", y = "Frecuencia")
# Valoracion_encontrar_empleo
hist_encontrar_empleo <- ggplot(df_reducido, aes(x = Valoracion_encontrar_empleo)) +
  geom_histogram(binwidth = 1, fill = "lightblue", col = "black") +
  labs(x = "Valoración de encontrar empleo", y = "Frecuencia")
# Valoracion_sit_ec_espana
hist_valoracion_sit_ec_espana <- ggplot(df_reducido, aes(x = Valoracion_sit_ec_espana)) +
  geom_histogram(binwidth = 1, fill = "lightblue", col = "black") +
  labs(x = "Valoracion de la situacion economica de España", y = "Frecuencia")
# Valoracion_mejorar_empleo_esp_6m
hist_mejorar_empleo <- ggplot(df_reducido, aes(x = Valoracion_mejorar_empleo_esp_6m)) +
  geom_histogram(binwidth = 1, fill = "lightblue", col = "black") +
  labs(x = "Valoracion de la mejora de encontrar empleo 6m", y = "Frecuencia")

# Mostrar los gráficos juntos
grid.arrange(hist_evolucion_paro, hist_encontrar_empleo, hist_valoracion_sit_ec_espana, hist_mejorar_empleo,
             nrow = 2, ncol = 2)

names(df_reducido)
# Evolucion ahorro
hist_evolucion_ahorro <- ggplot(df_reducido, aes(x = Evolucion_ahorro_personal)) +
  geom_histogram(binwidth = 1, fill = "lightblue", col = "black") +
  labs(x = "Evolución de la capacidad de ahorro personal", y = "Frecuencia")
# Valoracion_prospectiva_hogar
hist_valoracion_prospectiva_hogar <- ggplot(df_reducido, aes(x = Valoracion_prospectiva_hogar)) +
  geom_histogram(binwidth = 1, fill = "lightblue", col = "black") +
  labs(x = "Valoración prospectiva de la situación del hogar", y = "Frecuencia")
# Valoracion_prospectiva_espana
hist_valoracion_prospectiva_espana <- ggplot(df_reducido, aes(x = Valoracion_prospectiva_espana)) +
  geom_histogram(binwidth = 1, fill = "lightblue", col = "black") +
  labs(x = "Valoración prospectiva de la situación de España", y = "Frecuencia")
# Valoracion_inflacion
hist_inflacion <- ggplot(df_reducido, aes(x = Valoracion_inflacion)) +
  geom_histogram(binwidth = 1, fill = "lightblue", col = "black") +
  labs(x = "Valoracion de la inflación", y = "Frecuencia")

# Mostrar los gráficos juntos
grid.arrange(hist_evolucion_ahorro, hist_valoracion_prospectiva_hogar, hist_valoracion_prospectiva_espana, hist_inflacion,
             nrow = 2, ncol = 2)

names(df_reducido)
table(df_reducido$Intencion_comprar_vivienda)
# Evolucion interes
hist_evolucion_interes <- ggplot(df_reducido, aes(x = Evolucion_tipo_interes)) +
  geom_histogram(binwidth = 1, fill = "lightblue", col = "black") +
  labs(x = "Evolución del tipo de interés en España", y = "Frecuencia")
# Evolucion_precio_vivienda
hist_Evolucion_precio_vivienda <- ggplot(df_reducido, aes(x = Evolucion_precio_vivienda)) +
  geom_histogram(binwidth = 1, fill = "lightblue", col = "black") +
  labs(x = "Evolución del precio de la vivienda", y = "Frecuencia")
# Intencion_comprar_vivienda
hist_Intencion_comprar_vivienda <- ggplot(df_reducido, aes(x = Intencion_comprar_vivienda)) +
  geom_histogram(binwidth = 3, fill = "lightblue", col = "black") +
  labs(x = "Intención de comprar una vivienda en el próximo año", y = "Frecuencia")
# Estado_civil
hist_Estado_civil <- ggplot(df_reducido, aes(x = Estado_civil)) +
  geom_histogram(binwidth = 1, fill = "lightblue", col = "black") +
  labs(x = "Estado Civil del entrevistado", y = "Frecuencia")

# Mostrar los gráficos juntos
grid.arrange(hist_evolucion_interes, hist_Evolucion_precio_vivienda, hist_Intencion_comprar_vivienda, hist_Estado_civil,
             nrow = 2, ncol = 2)

names(df_reducido)
# Evolucion interes
hist_evolucion_interes <- ggplot(df_reducido, aes(x = Evolucion_tipo_interes)) +
  geom_histogram(binwidth = 1, fill = "lightblue", col = "black") +
  labs(x = "Evolución del tipo de interés en España", y = "Frecuencia")
# Evolucion_precio_vivienda
hist_Evolucion_precio_vivienda <- ggplot(df_reducido, aes(x = Evolucion_precio_vivienda)) +
  geom_histogram(binwidth = 1, fill = "lightblue", col = "black") +
  labs(x = "Evolución del precio de la vivienda", y = "Frecuencia")
# Intencion_comprar_vivienda
hist_Intencion_comprar_vivienda <- ggplot(df_reducido, aes(x = Intencion_comprar_vivienda)) +
  geom_histogram(binwidth = 1, fill = "lightblue", col = "black") +
  labs(x = "Intención de comprar una vivienda en el próximo año", y = "Frecuencia")
# Estado_civil
hist_Estado_civil <- ggplot(df_reducido, aes(x = Estado_civil)) +
  geom_histogram(binwidth = 1, fill = "lightblue", col = "black") +
  labs(x = "Estado Civil del entrevistado", y = "Frecuencia")

# Mostrar los gráficos juntos
grid.arrange(hist_evolucion_interes, hist_Evolucion_precio_vivienda, hist_Intencion_comprar_vivienda, hist_Estado_civil,
             nrow = 2, ncol = 2)

# CORRELACION PEARSON -----------------------------------------------------

# MATRIZ CORRELACION DE PEARSON
# Calcular la matriz de correlación de Pearson
cor_reducida <- cor(df_completo, use = "pairwise.complete.obs", method = "pearson")
# Mostrar la matriz de correlación
cor_reducida
# Graficar la matriz de correlación con colores
library(corrplot)
corrplot(cor_reducida, method = "color", type = "upper", tl.col = "black")
# Graficar la matriz de correlación como un mapa de calor
heatmap(cor_reducida, col = colorRampPalette(c("blue", "white", "red"))(100))

#Otras formas de graficar
# Calcular la matriz de correlación de Pearson
correlaciones1 <- cor(df_reducido[, -which(names(df_reducido) == "Confianza_consumidor")], method = "pearson")

# Graficar la matriz de correlación como un mapa de calor con etiquetas
heatmap(correlaciones1, col = colorRampPalette(c("blue", "white", "red"))(100),
        main = "Matriz de correlación", xlab = "Variables", ylab = "Variables",
        cex.main = 1.2, cex.axis = 0.8, cex.lab = 0.8)

# Convertir la matriz de correlación en un data frame
df_correlaciones <- as.data.frame(as.table(correlaciones1))
names(df_correlaciones) <- c("Variable1", "Variable2", "Correlacion")

# Graficar la matriz de correlación como un gráfico de mosaico
ggplot(df_correlaciones, aes(Variable1, Variable2, fill = Correlacion)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", na.value = "gray") +
  labs(title = "Matriz de correlación", x = "Variables", y = "Variables") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Otro codigo 
# No se si sale distinto 
# Calcular la matriz de correlación
matriz_correlacion1 <- cor(df_reducido[, c("Year", "MES", "Confianza_consumidor", "Variacion", 
                                          "Comunidad", "Tamano_Habitat", "Sexo", "Edad", "Provincia", 
                                          "Situacion_economia_hogar", "Valoracion_sit_ec_hogar", "Personas_paro_entorno", 
                                          "Evolucion_paro_entorno_6m", "Valoracion_encontrar_empleo", "Valoracion_sit_ec_espana", 
                                          "Valoracion_mejorar_empleo_esp_6m", "Evolucion_ahorro_personal", "Valoracion_prospectiva_hogar", 
                                          "Valoracion_prospectiva_espana", "Valoracion_inflacion", "Evolucion_tipo_interes", 
                                          "Evolucion_precio_vivienda", "Intencion_comprar_vivienda", "Estado_civil", "Tamano_hogar", 
                                          "Ingresos_hogar", "Situacion_laboral", "Tenencia_vivienda", "Adquisicion_automovil", 
                                          "Adquisicion_mueble", "Adquisicion_electrodomestico_grande", 
                                          "Adquisicion_electrodomestico_pequeño", "Composicion_hogar")])

# Visualizar la matriz de correlación
corrplot::corrplot(matriz_correlacion1, method = "color", type = "lower", tl.col = "black", tl.srt = 45, tl.cex = 0.7)

# Calcular la matriz de correlación
matriz_correlacion3 <- cor(df_reducido[, c("Year", "MES", "Confianza_consumidor", "Variacion", "Comunidad", "Tamano_Habitat", "Sexo", "Edad", "Provincia", "Situacion_economia_hogar", "Valoracion_sit_ec_hogar", "Personas_paro_entorno", "Evolucion_paro_entorno_6m", "Valoracion_encontrar_empleo", "Valoracion_sit_ec_espana", "Valoracion_mejorar_empleo_esp_6m", "Evolucion_ahorro_personal", "Valoracion_prospectiva_hogar", "Valoracion_prospectiva_espana", "Valoracion_inflacion", "Evolucion_tipo_interes", "Evolucion_precio_vivienda", "Intencion_comprar_vivienda", "Estado_civil", "Tamano_hogar", "Ingresos_hogar", "Situacion_laboral", "Tenencia_vivienda", "Adquisicion_automovil", "Adquisicion_mueble", "Adquisicion_electrodomestico_grande", "Adquisicion_electrodomestico_pequeño", "Composicion_hogar")])

# Crear un heatmap de la matriz de correlación
heatmap(matriz_correlacion3, col = colorRampPalette(c("blue", "white", "red"))(100))

# Establecer los colores para el heatmap
colores <- colorRampPalette(c("#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC"))(100)

# Crear un heatmap de la matriz de correlación
corrplot::corrplot(matriz_correlacion3, method = "color", col = colores, type = "lower", tl.col = "black", tl.srt = 45)

# Crear un gráfico de mapa de calor personalizado
ggplot(data = reshape2::melt(matriz_correlacion3),
       aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(x = "Variable 2", y = "Variable 1", fill = "Correlación") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# DIVIDIR EN YEARS --------------------------------------------------------

df_2018 <- subset(df_completo, Year == 2018)
df_2019 <- subset(df_completo, Year == 2019)
df_2020 <- subset(df_completo, Year == 2020)
df_2021 <- subset(df_completo, Year == 2021)
df_2022 <- subset(df_completo, Year == 2022)

# MODELO DE PRUEBA POR AÑOS
# 70% train 30% test
index_prob_2019 <- sample(1:nrow(df_2019), size = round(0.7*nrow(df_2019)), replace = FALSE)
index_prob_2020 <- sample(1:nrow(df_2020), size = round(0.7*nrow(df_2020)), replace = FALSE)
index_prob_2021 <- sample(1:nrow(df_2021), size = round(0.7*nrow(df_2021)), replace = FALSE)
index_prob_2022 <- sample(1:nrow(df_2022), size = round(0.7*nrow(df_2022)), replace = FALSE)
index_prob_2023 <- sample(1:nrow(df_2023), size = round(0.7*nrow(df_2023)), replace = FALSE)

# TRAIN Y TEST POR YEAR
# dividir los datos en conjuntos de entrenamiento y prueba 

# 2019
train_prob_2018 <- df_2018[index_prob_2018, ]
test_prob_2018 <- df_2018[-index_prob_2018, ]

# 2019
train_prob_2019 <- df_2019[index_prob_2019, ]
test_prob_2019 <- df_2019[-index_prob_2019, ]

# 2020
train_prob_2020 <- df_2020[index_prob_2020, ]
test_prob_2020 <- df_2020[-index_prob_2020, ]

# 2021
train_prob_2021 <- df_2021[index_prob_2021, ]
test_prob_2021 <- df_2021[-index_prob_2021, ]

# 2022
train_prob_2022 <- df_2022[index_prob_2022, ]
test_prob_2022 <- df_2022[-index_prob_2022, ]

# VARIABLES SIGNIFICATIVAS ------------------------------------------------

# Para ver cuales son las variables mas importantes en relacion
# a nuestra variable objetivo: Confianza_consumidor

# Calcula la matriz de correlación
cor_matrix <- cor(df_completo)
# Extrae los valores de correlación de la variable objetivo
cor_target <- cor_matrix["Confianza_consumidor",]
# Ordena los valores de correlación de mayor a menor
cor_target_sorted <- sort(cor_target, decreasing = TRUE)
# Visualiza los valores de correlación de cada variable con la variable objetivo
barplot(cor_target_sorted, main = "Correlación de cada variable con Confianza_consumidor")
# Crea la lista de correlación de cada variable con la variable objetivo
cor_list <- as.list(cor_target)
names(cor_list) <- colnames(df_completo)
# Ordena la lista de mayor a menor correlación
cor_list_sorted <- sort(unlist(cor_list), decreasing = TRUE)
# Visualiza las 10 variables más significativas
head(cor_list_sorted, n = 10)


# Nueva significacion -----------------------------------------------------

# Calcula la matriz de correlación
cor_matrix <- cor(df_completo)
# Extrae los valores de correlación de la variable objetivo
cor_target <- cor_matrix["Confianza_consumidor", ]
# Ordena los valores de correlación de mayor a menor
cor_target_sorted <- sort(cor_target, decreasing = TRUE)
# Visualiza los valores de correlación de cada variable con la variable objetivo
barplot(cor_target_sorted, main = "Correlación de cada variable con Confianza_consumidor", las = 3, cex.names = 0.6)
barplot(cor_target_sorted, main = "Correlación de cada variable con Confianza_consumidor",
        las = -1, cex.names = 0.6)
# Crea la lista de correlación de cada variable con la variable objetivo
cor_list <- as.list(cor_target)
names(cor_list) <- colnames(df_completo)
# Ordena la lista de mayor a menor correlación
cor_list_sorted <- sort(unlist(cor_list), decreasing = TRUE)
# Visualiza las 10 variables más significativas
top_10_positive <- head(cor_list_sorted, n = 10)
top_10_negative <- tail(cor_list_sorted, n = 10)
# Obtén las variables con correlación negativa
top_10_negative <- top_10_negative[top_10_negative < 0]
# Combina las variables significativas positivas y negativas
top_10 <- c(top_10_positive, top_10_negative)
# Visualiza las 10 variables más significativas (positivas y negativas)
barplot(top_10, main = "Variables más significativas con correlación positiva y negativa",
        ylim = range(top_10), las = 2, cex.names = 0.7, col = ifelse(top_10 < 0, "red", "blue"))

# Crear el gráfico de barras con nombres rotados
barplot(top_10, main = "Variables más significativas con correlación positiva y negativa",
        ylim = range(top_10), las = 2, cex.names = 0.7, col = ifelse(top_10 < 0, "red", "blue"),
        xpd = TRUE)  # Permite que los nombres de las variables se extiendan fuera del límite del gráfico

# Rotar los nombres de las variables en un ángulo de 45 grados
text(x = seq_along(top_10), y = top_10, labels = names(top_10), cex = 0.7, 
     xpd = TRUE, srt = 45, pos = 1, offset = 0.5)

# Visualiza las 10 variables más significativas (positivas y negativas)
barplot(top_10, main = "Variables más significativas con correlación positiva y negativa",
        ylim = range(top_10), las = 2, col = ifelse(top_10 < 0, "red", "blue"))

# Agrega etiquetas de las variables
text(x = 1:length(top_10), y = top_10, labels = names(top_10), srt = 45, adj = c(1, 1), xpd = TRUE)
# Establece el tamaño del texto del eje x
par(cex.axis = 0.5)

# Guardar los parámetros gráficos actuales
par_defaults <- par()
# Establecer los márgenes a los valores predeterminados
par(mar = c(5, 4, 4, 2) + 0.1)
# Crear un gráfico de barras sin el eje x previo
barplot(top_10, main = "Variables más significativas con correlación positiva y negativa",
        ylim = range(top_10), las = 2, col = ifelse(top_10 < 0, "red", "blue"),
        xaxt = "n")
# Rotar las etiquetas del eje x
axis(1, at = 1:length(top_10), labels = names(top_10), tick = FALSE, las = 1)

# Crear un data frame con los nombres y valores de las variables
plot_data <- data.frame(variable = names(top_10), correlacion = top_10)

# Ordenar el data frame por correlación de manera ascendente
plot_data <- plot_data[order(plot_data$correlacion), ]

# Generar el gráfico de barras
ggplot(plot_data, aes(x = variable, y = correlacion, fill = correlacion > 0)) +
  geom_bar(stat = "identity", color = "black", width = 0.5) +
  scale_fill_manual(values = c("red", "blue")) +
  labs(x = "Variable", y = "Correlación", title = "Variables más significativas") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()


# SIN CONFIANZA_CONSUMIDOR
ggplot(plot_data[plot_data$variable != "Confianza_consumidor", ], aes(x = variable, y = correlacion, fill = correlacion > 0)) +
  geom_bar(stat = "identity", color = "black", width = 0.5) +
  scale_fill_manual(values = c("red", "blue")) +
  labs(x = "Variable", y = "Correlación", title = "Variables más significativas") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()


# SE ME CORTAN LAS VARIABLES
# Calcula la matriz de correlación
cor_matrix <- cor(df_completo)
# Extrae los valores de correlación de la variable objetivo
cor_target <- cor_matrix["Confianza_consumidor", ]
# Ordena los valores de correlación de mayor a menor
cor_target_sorted <- sort(cor_target, decreasing = TRUE)
# Visualiza los valores de correlación de cada variable con la variable objetivo
barplot(cor_target_sorted, main = "Correlación de cada variable con Confianza_consumidor", las = 2, cex.names = 0.7)
# Crea la lista de correlación de cada variable con la variable objetivo
cor_list <- as.list(cor_target)
names(cor_list) <- colnames(df_completo)
# Ordena la lista de mayor a menor correlación
cor_list_sorted <- sort(unlist(cor_list), decreasing = TRUE)
# Visualiza las 10 variables más significativas
top_10_positive <- head(cor_list_sorted, n = 10)
top_10_negative <- tail(cor_list_sorted, n = 10)
# Obtén las variables con correlación negativa
top_10_negative <- top_10_negative[top_10_negative < 0]
# Combina las variables significativas positivas y negativas
top_10 <- c(top_10_positive, top_10_negative)
# Crea un gráfico con nombres de variables truncados
plot_data <- data.frame(variable = names(top_10), correlacion = top_10)
plot_data$variable <- stringr::str_wrap(plot_data$variable, width = 10)

# Visualiza las 10 variables más significativas (positivas y negativas)
ggplot(plot_data, aes(x = variable, y = correlacion, fill = correlacion > 0)) +
  geom_bar(stat = "identity", color = "black", width = 0.5) +
  scale_fill_manual(values = c("red", "blue"), guide = FALSE) +
  labs(x = "Variable", y = "Correlación", title = "Variables más significativas") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

barplot(top_10, main = "Variables más significativas con correlación positiva y negativa",
        ylim = range(top_10), las = 2, cex.names = 0.7, col = ifelse(top_10 < 0, "red", "blue"))


# Con los nuevo cambios
# Calcula la matriz de correlación
cor_matrix <- cor(df_completo)
# Extrae los valores de correlación de la variable objetivo
cor_target <- cor_matrix["Confianza_consumidor", ]
# Ordena los valores de correlación de mayor a menor
cor_target_sorted <- sort(cor_target, decreasing = TRUE)
# Elimina la variable objetivo de la lista de correlaciones
cor_list_sorted <- cor_target_sorted[-1]
# Separa las correlaciones positivas y negativas
positive_cor <- cor_list_sorted[cor_list_sorted > 0]
negative_cor <- cor_list_sorted[cor_list_sorted < 0]

# Crea un gráfico similar al generado con barplot
ggplot() +
  geom_bar(data = data.frame(variable = names(positive_cor), correlacion = positive_cor),
           aes(x = variable, y = correlacion), stat = "identity", fill = "blue", color = "black") +
  geom_bar(data = data.frame(variable = names(negative_cor), correlacion = negative_cor),
           aes(x = variable, y = correlacion), stat = "identity", fill = "red", color = "black") +
  labs(x = "Variable", y = "Correlación", title = "Variables más significativas") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.background = element_rect(fill = "white"))

# CORRELACION DE PEARSON CON TOP10
matriz_cor <- cor(top_10)
print(top_10)

# NUEVA CORRELACION VALIDADA SIN QUE SE CORTEN LAS VARIABLES 
# Crear el gráfico vacío sin ejes ni títulos
plot(0, type = "n", xlim = c(1, length(cor_target_sorted)),
     ylim = range(cor_target_sorted), xlab = "", ylab = "Correlación",
     main = "", axes = FALSE)
# Calcular las coordenadas de las barras
bar_x <- 1:length(cor_target_sorted)
bar_y <- cor_target_sorted
# Dibujar las barras
rect(bar_x - 0.4, 0, bar_x + 0.4, bar_y, col = "lightblue", border = NA)
# Añadir las etiquetas alineadas con las columnas
text(x = bar_x, y = bar_y, labels = names(cor_target_sorted),
     srt = 90, adj = c(0.5, 1), pos = 2, cex = 0.6, xpd = TRUE)
# Ajustar los ejes manualmente
axis(2)


# MULTICOLINEALIDAD -------------------------------------------------------

# Cálculo de la matriz de correlación
correlation_matrix <- cor(df_completo)

# Visualización de la matriz de correlación como un mapa de calor
library(ggplot2)
library(reshape2)

correlation_data <- melt(correlation_matrix)
ggplot(correlation_data, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 10, hjust = 1)) +
  coord_fixed()

# Identificación de multicolinealidad
highly_correlated <- findCorrelation(correlation_matrix, cutoff = 0.8)
highly_correlated_vars <- colnames(df_completo)[highly_correlated]

print("Variables altamente correlacionadas:")
print(highly_correlated_vars)


library(caret)

# Definir umbral de correlación
umbral_correlacion <- 0.6
# Calcular la matriz de correlación
matriz_correlacion <- cor(df_completo)
# Encontrar variables altamente correlacionadas
highly_correlated_vars <- findCorrelation(matriz_correlacion, cutoff = umbral_correlacion)
# Imprimir las variables altamente correlacionadas
print("Variables altamente correlacionadas:")
print(names(df_completo)[highly_correlated_vars])


# MULTICOLINEALIDAD CON OTROS CODIGOS
library(caret)
library(car)
library(dplyr)


# Realizar análisis de componentes principales (PCA)
pca <- function(x) {
  x <- as.matrix(x)
  pca_result <- princomp(x, cor = TRUE)
  pca_summary <- summary(pca_result)
  return(pca_summary)
}

# Calcular el factor de inflación de la varianza (VIF)
vif <- function(x) {
  x <- as.matrix(x)
  vif_values <- stats::vif(x)
  vif_df <- data.frame(Variables = rownames(vif_values), VIF = vif_values)
  return(vif_df)
}

# Seleccionar las variables predictoras para evaluar la multicolinealidad
# predictor_vars <- df_completo %>%
#   select(-Confianza_consumidor)
predictor_vars <- df_completo[, !(names(df_completo) %in% "Confianza_consumidor")]

# Calcular la matriz de correlación entre las variables predictoras
cor_matrix <- cor(predictor_vars)
# Calcular el inverso de la matriz de correlación
inv_cor_matrix <- solve(cor_matrix)
# Calcular el VIF para cada variable
vif_results <- diag(inv_cor_matrix)
print("Factor de inflación de la varianza (VIF):")
print(vif_results)


# PCA ---------------------------------------------------------------------

# Realizar análisis de componentes principales (PCA)
pca_summary <- pca(predictor_vars)
print("Análisis de componentes principales (PCA):")
print(pca_summary)

# Realizar el análisis de componentes principales (PCA)
pca <- prcomp(predictor_vars, scale. = TRUE)

# Obtener la proporción de varianza explicada acumulativa
variance_explained <- cumsum(pca$sdev^2) / sum(pca$sdev^2)

# Determinar el número de componentes principales a utilizar
desired_variance <- 0.95  # Por ejemplo, queremos explicar al menos el 95% de la varianza total
num_components <- sum(variance_explained < desired_variance) + 1

# Reducir la dimensionalidad de los datos utilizando los componentes principales seleccionados
reduced_data <- predict(pca, newdata = predictor_vars)[, 1:num_components]

# CORRELACION -------------------------------------------------------------

# De los datos enteros
correlaciones_dataset <- cor(df_completo)
# PERO CUIDADO QUE ESTE CODIGO SOLO TE PONE LAS POSITIVAS
# QUEREMOS EL VALOR DEL COEFICIENTE 
# Seleccionar las variables con una correlación más fuerte con la variable objetivo
variables_significativas_dataset <- names(sort(correlaciones_dataset["Confianza_consumidor",], decreasing = TRUE))[2:11]
print(variables_significativas_dataset)
# [1] "Variacion"                            "Evolucion_paro_entorno_6m"           
# [3] "Adquisicion_electrodomestico_grande"  "Adquisicion_mueble"                  
# [5] "Adquisicion_electrodomestico_pequeño" "Situacion_laboral"                   
# [7] "Intencion_comprar_vivienda"           "Personas_paro_entorno"               
# [9] "Provincia"                            "Sexo" 
# Seria con este codigo 
# De los datos enteros
correlaciones_dataset <- cor(df_completo)
# Seleccionar las variables con una correlación más fuerte (positiva o negativa) con la variable objetivo
cor_target <- correlaciones_dataset["Confianza_consumidor", ]
variables_significativas_dataset <- names(sort(abs(cor_target), decreasing = TRUE))[2:11]
print(variables_significativas_dataset)

# CORRELACION SIN YEAR MES Y VARIACION
cols_significativas <- subset(df_completo, select = -c(Year, MES, Variacion))
correlation_matrix_division <- cor(cols_significativas)
variables_significativas_sin <- names(sort(correlation_matrix_division["Confianza_consumidor",], decreasing = TRUE))[2:11]
print(variables_significativas_sin)
# [1] "Evolucion_paro_entorno_6m"            "Adquisicion_electrodomestico_grande" 
# [3] "Adquisicion_mueble"                   "Adquisicion_electrodomestico_pequeño"
# [5] "Situacion_laboral"                    "Intencion_comprar_vivienda"          
# [7] "Personas_paro_entorno"                "Provincia"                           
# [9] "Sexo"                                 "Adquisicion_automovil" 
# Excluir las columnas "Year", "MES" y "Variacion"
cols_significativas <- subset(df_completo, select = -c(Year, MES, Variacion))
# Calcular la matriz de correlación sin las columnas excluidas
correlation_matrix_division <- cor(cols_significativas)
# Seleccionar las variables con una correlación más fuerte (positiva o negativa) con la variable objetivo
cor_target <- correlation_matrix_division["Confianza_consumidor", ]
variables_significativas_sin <- names(sort(abs(cor_target), decreasing = TRUE))[2:11]
print(variables_significativas_sin)

# CORRELACION POR AÑOS ----------------------------------------------------

# 2018
# De los datos enteros
# Para poder hacerlo, hay que eliminar el año, porque son todas en 2018
df_2018 <- subset(df_2018, select = -c(Year))
correlaciones_2018 <- cor(df_2018)
# Seleccionar las variables con una correlación más fuerte con la variable objetivo
variables_significativas_2018 <- names(sort(correlaciones_2018["Confianza_consumidor",], decreasing = TRUE))[2:11]
print(variables_significativas_2018)
# [1] "Variacion"                            "Adquisicion_electrodomestico_pequeño"
# [3] "Adquisicion_mueble"                   "Adquisicion_electrodomestico_grande" 
# [5] "Adquisicion_automovil"                "Personas_paro_entorno"               
# [7] "Intencion_comprar_vivienda"           "Evolucion_tipo_interes"              
# [9] "Situacion_laboral"                    "Evolucion_paro_entorno_6m"                              "Sexo" 
# Para absoluto
correlaciones_2018 <- cor(df_2018)
# Seleccionar las variables con una correlación más fuerte (positiva o negativa) con la variable objetivo
cor_target <- correlaciones_2018["Confianza_consumidor", ]
variables_significativas_2018 <- names(sort(abs(cor_target), decreasing = TRUE))[2:11]
print(variables_significativas_2018)

# CORRELACION SIN YEAR MES Y VARIACION
cols_significativas_2018 <- subset(df_2018, select = -c(MES, Variacion))
correlation_matrix_division_2018 <- cor(cols_significativas_2018)
variables_significativas_sin_2018 <- names(sort(correlation_matrix_division_2018["Confianza_consumidor",], decreasing = TRUE))[2:11]
print(variables_significativas_sin_2018)
# [1] "Adquisicion_electrodomestico_pequeño" "Adquisicion_mueble"                  
# [3] "Adquisicion_electrodomestico_grande"  "Adquisicion_automovil"               
# [5] "Personas_paro_entorno"                "Intencion_comprar_vivienda"          
# [7] "Evolucion_tipo_interes"               "Situacion_laboral"                   
# [9] "Evolucion_paro_entorno_6m"            "Sexo"                     
# ABSOLUTO
# Excluir las columnas "Year", "MES" y "Variacion"
cols_significativas <- subset(df_2018, select = -c(MES, Variacion))
# Calcular la matriz de correlación sin las columnas excluidas
correlation_matrix_division <- cor(cols_significativas)
# Seleccionar las variables con una correlación más fuerte (positiva o negativa) con la variable objetivo
cor_target <- correlation_matrix_division["Confianza_consumidor", ]
variables_significativas_sin_2018 <- names(sort(abs(cor_target), decreasing = TRUE))[2:11]
print(variables_significativas_sin_2018)


# 2019
df_2019 <- subset(df_2019, select = -c(Year))
correlaciones_2019 <- cor(df_2019)
# Seleccionar las variables con una correlación más fuerte con la variable objetivo
variables_significativas_2019 <- names(sort(correlaciones_2019["Confianza_consumidor",], decreasing = TRUE))[2:11]
print(variables_significativas_2019)
# [1] "Variacion"                            "Tamano_hogar"                        
# [3] "Adquisicion_mueble"                   "Adquisicion_electrodomestico_pequeño"
# [5] "Evolucion_paro_entorno_6m"            "Adquisicion_automovil"               
# [7] "Adquisicion_electrodomestico_grande"  "Ideologia"                           
# [9] "Edad"                                 "Provincia" 
# ABOSLUTO
# Para absoluto
correlaciones_2019 <- cor(df_2019)
# Seleccionar las variables con una correlación más fuerte (positiva o negativa) con la variable objetivo
cor_target <- correlaciones_2019["Confianza_consumidor", ]
variables_significativas_2019 <- names(sort(abs(cor_target), decreasing = TRUE))[2:11]
print(variables_significativas_2019)

# CORRELACION SIN YEAR MES Y VARIACION
cols_significativas_2019 <- subset(df_2019, select = -c(MES, Variacion))
correlation_matrix_division_2019 <- cor(cols_significativas_2019)
variables_significativas_sin_2019 <- names(sort(correlation_matrix_division_2019["Confianza_consumidor",], decreasing = TRUE))[2:11]
print(variables_significativas_sin_2019)
# [1] "Tamano_hogar"                         "Adquisicion_mueble"                  
# [3] "Adquisicion_electrodomestico_pequeño" "Evolucion_paro_entorno_6m"           
# [5] "Adquisicion_automovil"                "Adquisicion_electrodomestico_grande" 
# [7] "Ideologia"                            "Edad"                                
# [9] "Provincia"                            "Ingresos_hogar" 
# ABSOLUTO
# Excluir las columnas "Year", "MES" y "Variacion"
cols_significativas_2019 <- subset(df_2019, select = -c(MES, Variacion))
# Calcular la matriz de correlación sin las columnas excluidas
correlation_matrix_division <- cor(cols_significativas_2019)
# Seleccionar las variables con una correlación más fuerte (positiva o negativa) con la variable objetivo
cor_target <- correlation_matrix_division["Confianza_consumidor", ]
variables_significativas_sin_2019 <- names(sort(abs(cor_target), decreasing = TRUE))[2:11]
print(variables_significativas_sin_2019)

# 2020
# De los datos enteros
# Para poder hacerlo, hay que eliminar el año, porque son todas en 2020
df_2020 <- subset(df_2020, select = -c(Year))
correlaciones_2020 <- cor(df_2020)

# Seleccionar las variables con una correlación más fuerte con la variable objetivo
variables_significativas_2020 <- names(sort(correlaciones_2020["Confianza_consumidor",], decreasing = TRUE))[2:11]
print(variables_significativas_2020)
# [1] "Variacion"                 "Evolucion_paro_entorno_6m" "Estado_civil"             
# [4] "Tenencia_vivienda"         "Ideologia"                 "Composicion_hogar"        
# [7] "Situacion_laboral"         "Provincia"                 "Adquisicion_mueble"       
# [10] "Comunidad" 
# ABSOLUTO
# Para absoluto
correlaciones_2020 <- cor(df_2020)
# Seleccionar las variables con una correlación más fuerte (positiva o negativa) con la variable objetivo
cor_target <- correlaciones_2020["Confianza_consumidor", ]
variables_significativas_2020 <- names(sort(abs(cor_target), decreasing = TRUE))[2:11]
print(variables_significativas_2020)

# CORRELACION SIN YEAR MES Y VARIACION
cols_significativas_2020 <- subset(df_2020, select = -c(MES, Variacion))
correlation_matrix_division_2020 <- cor(cols_significativas_2020)
variables_significativas_sin_2020 <- names(sort(correlation_matrix_division_2020["Confianza_consumidor",], decreasing = TRUE))[2:11]
print(variables_significativas_sin_2020)
# [1] "Evolucion_paro_entorno_6m" "Estado_civil"              "Tenencia_vivienda"        
# [4] "Ideologia"                 "Composicion_hogar"         "Situacion_laboral"        
# [7] "Provincia"                 "Adquisicion_mueble"        "Comunidad"                
# [10] "Situacion_economia_hogar" 
# ABSOLUTO
# Excluir las columnas "Year", "MES" y "Variacion"
cols_significativas_2020 <- subset(df_2020, select = -c(MES, Variacion))
# Calcular la matriz de correlación sin las columnas excluidas
correlation_matrix_division <- cor(cols_significativas_2020)
# Seleccionar las variables con una correlación más fuerte (positiva o negativa) con la variable objetivo
cor_target <- correlation_matrix_division["Confianza_consumidor", ]
variables_significativas_sin_2020 <- names(sort(abs(cor_target), decreasing = TRUE))[2:11]
print(variables_significativas_sin_2020)

# 2021
# De los datos enteros
# Para poder hacerlo, hay que eliminar el año, porque son todas en 2021
df_2021 <- subset(df_2021, select = -c(Year))
correlaciones_2021 <- cor(df_2021)
# Seleccionar las variables con una correlación más fuerte con la variable objetivo
variables_significativas_2021 <- names(sort(correlaciones_2021["Confianza_consumidor",], decreasing = TRUE))[2:11]
print(variables_significativas_2021)
# [1] "MES"                                  "Evolucion_paro_entorno_6m"           
# [3] "Variacion"                            "Adquisicion_electrodomestico_grande" 
# [5] "Adquisicion_electrodomestico_pequeño" "Ingresos_hogar"                      
# [7] "Tenencia_vivienda"                    "Provincia"                           
# [9] "Edad"                                 "Tamano_Habitat" 
# ABSOLUTO
# Para absoluto
correlaciones_2021 <- cor(df_2021)
# Seleccionar las variables con una correlación más fuerte (positiva o negativa) con la variable objetivo
cor_target <- correlaciones_2021["Confianza_consumidor", ]
variables_significativas_2021 <- names(sort(abs(cor_target), decreasing = TRUE))[2:11]
print(variables_significativas_2021)

# CORRELACION SIN YEAR MES Y VARIACION
cols_significativas_2021 <- subset(df_2021, select = -c(MES, Variacion))
correlation_matrix_division_2021 <- cor(cols_significativas_2021)
variables_significativas_sin_2021 <- names(sort(correlation_matrix_division_2021["Confianza_consumidor",], decreasing = TRUE))[2:11]
print(variables_significativas_sin_2021)
# [1] "Evolucion_paro_entorno_6m"            "Adquisicion_electrodomestico_grande" 
# [3] "Adquisicion_electrodomestico_pequeño" "Ingresos_hogar"                      
# [5] "Tenencia_vivienda"                    "Provincia"                           
# [7] "Edad"                                 "Tamano_Habitat"                      
# [9] "Estado_civil"                         "Ideologia"    
# ABSOLUTO
# Excluir las columnas "Year", "MES" y "Variacion"
cols_significativas_2021 <- subset(df_2021, select = -c(MES, Variacion))
# Calcular la matriz de correlación sin las columnas excluidas
correlation_matrix_division <- cor(cols_significativas_2021)
# Seleccionar las variables con una correlación más fuerte (positiva o negativa) con la variable objetivo
cor_target <- correlation_matrix_division["Confianza_consumidor", ]
variables_significativas_sin_2021 <- names(sort(abs(cor_target), decreasing = TRUE))[2:11]
print(variables_significativas_sin_2021)


# 2022
# De los datos enteros
# Para poder hacerlo, hay que eliminar el año, porque son todas en 2022
df_2022 <- subset(df_2022, select = -c(Year))
correlaciones_2022 <- cor(df_2022)

# Seleccionar las variables con una correlación más fuerte con la variable objetivo
variables_significativas_2022 <- names(sort(correlaciones_2022["Confianza_consumidor",], decreasing = TRUE))[2:11]
print(variables_significativas_2022)
# [1] "Variacion"                           "Evolucion_tipo_interes"             
# [3] "Situacion_economia_hogar"            "Evolucion_paro_entorno_6m"          
# [5] "Adquisicion_electrodomestico_grande" "Adquisicion_mueble"                 
# [7] "Situacion_laboral"                   "Tenencia_vivienda"                  
# [9] "Composicion_hogar"                   "Tamano_Habitat"  
# aBSOLUTO
# Para absoluto
correlaciones_2022 <- cor(df_2022)
# Seleccionar las variables con una correlación más fuerte (positiva o negativa) con la variable objetivo
cor_target <- correlaciones_2022["Confianza_consumidor", ]
variables_significativas_2022 <- names(sort(abs(cor_target), decreasing = TRUE))[2:11]
print(variables_significativas_2022)

# CORRELACION SIN YEAR MES Y VARIACION
cols_significativas_2022 <- subset(df_2022, select = -c(MES, Variacion))
correlation_matrix_division_2022 <- cor(cols_significativas_2022)
variables_significativas_sin_2022 <- names(sort(correlation_matrix_division_2022["Confianza_consumidor",], decreasing = TRUE))[2:11]
# print(variables_significativas_sin_2022)
# [1] "Evolucion_tipo_interes"              "Situacion_economia_hogar"           
# [3] "Evolucion_paro_entorno_6m"           "Adquisicion_electrodomestico_grande"
# [5] "Adquisicion_mueble"                  "Situacion_laboral"                  
# [7] "Tenencia_vivienda"                   "Composicion_hogar"                  
# [9] "Tamano_Habitat"                      "Tamano_hogar"
# ABSOLUTO
# Excluir las columnas "Year", "MES" y "Variacion"
cols_significativas_2022 <- subset(df_2022, select = -c(MES, Variacion))
# Calcular la matriz de correlación sin las columnas excluidas
correlation_matrix_division <- cor(cols_significativas_2022)
# Seleccionar las variables con una correlación más fuerte (positiva o negativa) con la variable objetivo
cor_target <- correlation_matrix_division["Confianza_consumidor", ]
variables_significativas_sin_2022 <- names(sort(abs(cor_target), decreasing = TRUE))[2:11]
print(variables_significativas_sin_2022)


# TABLA CORRELACIONES -----------------------------------------------------
# Combinar las tablas en un único dataframe
tabla_combinada <- rbind(variables_significativas_dataset, variables_significativas_sin,
                         variables_significativas_2018, variables_significativas_sin_2018,
                         variables_significativas_2019, variables_significativas_sin_2019,
                         variables_significativas_2020, variables_significativas_sin_2020,
                         variables_significativas_2021, variables_significativas_sin_2021,
                         variables_significativas_2022, variables_significativas_sin_2022)

# Visualizar la tabla combinada
print(tabla_combinada)

# TRANSPONER LA TABLA PARA QUE SEA EL FORMATO
tabla_combinada_transpuesta <- t(tabla_combinada)
# Imprimir la tabla transpuesta con el formato deseado
print(tabla_combinada_transpuesta, quote = FALSE)
view(tabla_combinada_transpuesta)

# Combinar las tablas en un único dataframe
tabla_combinada2 <- rbind(variables_significativas_dataset, variables_significativas_sin,
                         variables_significativas_2018, variables_significativas_2019,
                         variables_significativas_2020, 
                         variables_significativas_2021, 
                         variables_significativas_2022)

# Visualizar la tabla combinada
print(tabla_combinada2)

# TRANSPONER LA TABLA PARA QUE SEA EL FORMATO
tabla_combinada_transpuesta2 <- t(tabla_combinada2)
view(tabla_combinada_transpuesta2)

# Combinar las tablas en un único dataframe
tabla_combinada1 <- rbind(variables_significativas_dataset, variables_significativas_sin,
                         variables_significativas_2018, variables_significativas_sin_2018)
# TRANSPONER LA TABLA PARA QUE SEA EL FORMATO
tabla_combinada_transpuesta1 <- t(tabla_combinada1)
# Imprimir la tabla transpuesta con el formato deseado
view(tabla_combinada_transpuesta1)

# Combinar las tablas en un único dataframe
tabla_combinada2 <- rbind(variables_significativas_2019, variables_significativas_sin_2019,
                          variables_significativas_2020, variables_significativas_sin_2020)
# TRANSPONER LA TABLA PARA QUE SEA EL FORMATO
tabla_combinada_transpuesta2 <- t(tabla_combinada2)
# Imprimir la tabla transpuesta con el formato deseado
view(tabla_combinada_transpuesta2)

# Combinar las tablas en un único dataframe
tabla_combinada3 <- rbind(variables_significativas_2021, variables_significativas_sin_2021,
                          variables_significativas_2022, variables_significativas_sin_2022)
# TRANSPONER LA TABLA PARA QUE SEA EL FORMATO
tabla_combinada_transpuesta3 <- t(tabla_combinada3)
# Imprimir la tabla transpuesta con el formato deseado
view(tabla_combinada_transpuesta3)

# ELIMINAR REDUCIR --------------------------------------------------------

# Reducir de forma aleatoria, para que se quede mas o menos 
# con 500 observaciones de cada año 
# grafico de observaciones por mes y año 
ggplot(df_completo, aes(x = Year, fill = factor(MES))) +
  geom_bar(position = "dodge")

table(df_completo$Year, df_completo$MES)

# REDUCCION

library(dplyr)

# Agrupar por año y mes y aplicar una función para tomar 500 observaciones aleatorias, pero solo si hay más de 500 observaciones en el grupo
df_reducido <- df_completo %>%
  group_by(Year, MES) %>%
  filter(n() >= 500) %>%
  sample_n(size = 500, replace = FALSE) %>%
  ungroup()

ggplot(df_reducido, aes(x = Year, fill = factor(MES))) +
  geom_bar(position = "dodge")

table(df_reducido$Year, df_reducido$MES)


# Seleccion basada en modelos  --------------------------------------------

# Ajustar modelo de regresión lineal
lm_model <- lm(Confianza_consumidor ~ ., data = df_completo)
# Seleccionar características con coeficientes significativos
significant_features <- names(lm_model$coefficients)[which(lm_model$coefficients != 0 & !is.na(lm_model$coefficients))]

# Ajustar modelo de árboles de decisión
tree_model <- rpart(Confianza_consumidor ~ ., data = df_completo)
# Seleccionar características más importantes
important_features <- names(sort(tree_model$variable.importance, decreasing = TRUE))[1:10]
# Ajustar modelo de árboles de decisión SIN
tree_model2 <- rpart(Confianza_consumidor ~ .-Year -Variacion -MES, data = train)
# Seleccionar características más importantes SIN 
important_features2 <- names(sort(tree_model2$variable.importance, decreasing = TRUE))[1:10]


# DENSIDAD ----------------------------------------------------------------

# Gráfico de densidad para "Evolucion_tipo_interes"
ggplot(df_completo, aes(x = Evolucion_tipo_interes)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(x = "Evolucion_tipo_interes", y = "Densidad") +
  ggtitle("Gráfico de densidad - Evolucion_tipo_interes")

# Gráfico de densidad para "Situacion_economia_hogar"
ggplot(df_completo, aes(x = Situacion_economia_hogar)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(x = "Situacion_economia_hogar", y = "Densidad") +
  ggtitle("Gráfico de densidad - Situacion_economia_hogar")

# Gráfico de densidad para "Evolucion_paro_entorno_6m"
ggplot(df_completo, aes(x = Evolucion_paro_entorno_6m)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(x = "Evolucion_paro_entorno_6m", y = "Densidad") +
  ggtitle("Gráfico de densidad - Evolucion_paro_entorno_6m")

# Gráfico de densidad para "Adquisicion_electrodomestico_grande"
ggplot(df_completo, aes(x = Adquisicion_electrodomestico_grande)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(x = "Adquisicion_electrodomestico_grande", y = "Densidad") +
  ggtitle("Gráfico de densidad - Adquisicion_electrodomestico_grande")


# Gráfico de densidad para "Confianza_consumidor" VARIABLE OBJETIVO
ggplot(df_completo, aes(x = Confianza_consumidor)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(x = "Confianza del consumidor", y = "Densidad") +
  ggtitle("Gráfico de densidad - Variable objetivo")


# DESVIACION TIPICA O RANGO -----------------------------------------------

names(df_completo)
# Calcular desviación típica y rango para las variables
desviacion <- apply(df_completo[, c("Edad", "Provincia", "Comunidad", "Confianza_consumidor", "MES", "Variacion")], 2, sd)
rango <- apply(df_completo[, c("Edad", "Provincia", "Comunidad", "Confianza_consumidor", "MES", "Variacion")], 2, function(x) max(x) - min(x))

# Crear dataframe para gráfico de desviación típica o rango
estadisticas <- data.frame(Variables = c("Edad", "Provincia", "Comunidad", "Confianza_consumidor", "MES", "Variacion"),
                           Desviacion = desviacion,
                           Rango = rango)

# Gráfico de desviación típica
ggplot(estadisticas, aes(x = Variables, y = Desviacion)) +
  geom_bar(stat = "identity", fill = "lightblue", width = 0.5) +
  labs(x = "Variables", y = "Desviación Típica") +
  ggtitle("Gráfico de Desviación Típica")

# Gráfico de rango
ggplot(estadisticas, aes(x = Variables, y = Rango)) +
  geom_bar(stat = "identity", fill = "lightblue", width = 0.5) +
  labs(x = "Variables", y = "Rango") +
  ggtitle("Gráfico de Rango")

names(df_completo)
# Parte 2 de los gráficos
desviacion2 <- apply(df_completo[, c("Year", "Tamano_Habitat", "Situacion_economia_hogar", "Valoracion_sit_ec_hogar", "Personas_paro_entorno", "Evolucion_paro_entorno_6m")], 2, sd)
rango2 <- apply(df_completo[, c("Year", "Tamano_Habitat", "Situacion_economia_hogar", "Valoracion_sit_ec_hogar", "Personas_paro_entorno", "Evolucion_paro_entorno_6m")], 2, function(x) max(x) - min(x))
# Crear dataframe para gráfico de desviación típica o rango
estadisticas <- data.frame(Variables = c("Year", "Tamano_Habitat", "Situacion_economia_hogar", "Valoracion_sit_ec_hogar", "Personas_paro_entorno", "Evolucion_paro_entorno_6m"),
                           Desviacion = desviacion2,
                           Rango = rango2)

# Gráfico de desviación típica
ggplot(estadisticas, aes(x = Variables, y = Desviacion)) +
  geom_bar(stat = "identity", fill = "lightblue", width = 0.5) +
  labs(x = "Variables", y = "Desviación Típica") +
  ggtitle("Gráfico de Desviación Típica")

# Gráfico de rango
ggplot(estadisticas, aes(x = Variables, y = Rango)) +
  geom_bar(stat = "identity", fill = "lightblue", width = 0.5) +
  labs(x = "Variables", y = "Rango") +
  ggtitle("Gráfico de Rango")


names(df_completo)
# Parte 3 de los gráficos
desviacion3 <- apply(df_completo[, c("Valoracion_encontrar_empleo", "Valoracion_sit_ec_espana", "Valoracion_mejorar_empleo_esp_6m", "Evolucion_ahorro_personal", "Valoracion_prospectiva_hogar", "Valoracion_prospectiva_espana")], 2, sd)
rango3 <- apply(df_completo[, c("Valoracion_encontrar_empleo", "Valoracion_sit_ec_espana", "Valoracion_mejorar_empleo_esp_6m", "Evolucion_ahorro_personal", "Valoracion_prospectiva_hogar", "Valoracion_prospectiva_espana")], 2, function(x) max(x) - min(x))
# Crear dataframe para gráfico de desviación típica o rango
estadisticas <- data.frame(Variables = c("Valoracion_encontrar_empleo", "Valoracion_sit_ec_espana", "Valoracion_mejorar_empleo_esp_6m", "Evolucion_ahorro_personal", "Valoracion_prospectiva_hogar", "Valoracion_prospectiva_espana"),
                           Desviacion = desviacion3,
                           Rango = rango3)

# Gráfico de desviación típica
ggplot(estadisticas, aes(x = Variables, y = Desviacion)) +
  geom_bar(stat = "identity", fill = "lightblue", width = 0.5) +
  labs(x = "Variables", y = "Desviación Típica") +
  ggtitle("Gráfico de Desviación Típica")

# Gráfico de rango
ggplot(estadisticas, aes(x = Variables, y = Rango)) +
  geom_bar(stat = "identity", fill = "blue", width = 0.5) +
  labs(x = "Variables", y = "Rango") +
  ggtitle("Gráfico de Rango")

names(df_completo)
# Parte 4 de los gráficos
desviacion4 <- apply(df_completo[, c("Valoracion_inflacion", "Evolucion_tipo_interes", "Evolucion_precio_vivienda", "Intencion_comprar_vivienda", "Estado_civil", "Tamano_hogar")], 2, sd)
rango4 <- apply(df_completo[, c("Valoracion_inflacion", "Evolucion_tipo_interes", "Evolucion_precio_vivienda", "Intencion_comprar_vivienda", "Estado_civil", "Tamano_hogar")], 2, function(x) max(x) - min(x))
# Crear dataframe para gráfico de desviación típica o rango
estadisticas <- data.frame(Variables = c("Valoracion_inflacion", "Evolucion_tipo_interes", "Evolucion_precio_vivienda", "Intencion_comprar_vivienda", "Estado_civil", "Tamano_hogar"),
                           Desviacion = desviacion4,
                           Rango = rango4)

# Gráfico de desviación típica
ggplot(estadisticas, aes(x = Variables, y = Desviacion)) +
  geom_bar(stat = "identity", fill = "lightblue", width = 0.5) +
  labs(x = "Variables", y = "Desviación Típica") +
  ggtitle("Gráfico de Desviación Típica")

# Gráfico de rango
ggplot(estadisticas, aes(x = Variables, y = Rango)) +
  geom_bar(stat = "identity", fill = "lightblue", width = 0.5) +
  labs(x = "Variables", y = "Rango") +
  ggtitle("Gráfico de Rango")


names(df_completo)
# Parte 5 de los gráficos
desviacion5 <- apply(df_completo[, c("Ingresos_hogar", "Situacion_laboral", "Tenencia_vivienda", "Adquisicion_automovil", "Adquisicion_mueble", "Adquisicion_electrodomestico_grande", "Adquisicion_electrodomestico_pequeño", "Composicion_hogar")], 2, sd)
rango5 <- apply(df_completo[, c("Ingresos_hogar", "Situacion_laboral", "Tenencia_vivienda", "Adquisicion_automovil", "Adquisicion_mueble", "Adquisicion_electrodomestico_grande", "Adquisicion_electrodomestico_pequeño", "Composicion_hogar")], 2, function(x) max(x) - min(x))
# Crear dataframe para gráfico de desviación típica o rango
estadisticas <- data.frame(Variables = c("Ingresos_hogar", "Situacion_laboral", "Tenencia_vivienda", "Adquisicion_automovil", "Adquisicion_mueble", "Adquisicion_electrodomestico_grande", "Adquisicion_electrodomestico_pequeño", "Composicion_hogar"),
                           Desviacion = desviacion5,
                           Rango = rango5)

# Gráfico de desviación típica
ggplot(estadisticas, aes(x = Variables, y = Desviacion)) +
  geom_bar(stat = "identity", fill = "lightblue", width = 0.5) +
  labs(x = "Variables", y = "Desviación Típica") +
  ggtitle("Gráfico de Desviación Típica")

# Gráfico de rango
ggplot(estadisticas, aes(x = Variables, y = Rango)) +
  geom_bar(stat = "identity", fill = "lightblue", width = 0.5) +
  labs(x = "Variables", y = "Rango") +
  ggtitle("Gráfico de Rango")

# GRAFICO DE TIEMPO -------------------------------------------------------

df_completoA <- df_completo
library(zoo)

# Convertir variables MES y Year a formato de fecha
df_completoA$fecha <- as.yearmon(paste(df_completoA$Year, df_completoA$MES, sep = "-"))

# Calcular promedio mensual de las variables
df_resumen <- aggregate(. ~ fecha, data = df_completoA, FUN = mean)

# Gráfico de tiempo para "Evolucion_tipo_interes"
ggplot(df_resumen, aes(x = fecha, y = Evolucion_tipo_interes)) +
  geom_line() +
  labs(x = "Fecha", y = "Evolucion_tipo_interes") +
  ggtitle("Gráfico de tiempo - Evolucion_tipo_interes")

# Gráfico de tiempo para "Valoracion_prospectiva_espana"
ggplot(df_resumen, aes(x = fecha, y = Valoracion_prospectiva_espana)) +
  geom_line() +
  labs(x = "Fecha", y = "Valoracion_prospectiva_espana") +
  ggtitle("Gráfico de tiempo - Valoracion_prospectiva_espana")

# Gráfico de tiempo para "Sexo"
ggplot(df_resumen, aes(x = fecha, y = Sexo)) +
  geom_line() +
  labs(x = "Fecha", y = "Sexo") +
  ggtitle("Gráfico de tiempo - Sexo")

  # Gráfico de frecuencia por categoría para "Valoracion_encontrar_empleo"
ggplot(df_completoA, aes(x = fecha, fill = Valoracion_encontrar_empleo)) +
  geom_bar(fill = "lightblue") +
  labs(x = "Fecha", y = "Frecuencia") +
  ggtitle("Gráfico de frecuencia por categoría - Valoracion_encontrar_empleo")

names(df_completoA)
# Gráfico de frecuencia por categoría para "Evolucion_precio_vivienda" y "Valoracion_mejorar_empleo_6m"
ggplot(df_completoA, aes(x = fecha, fill = Evolucion_precio_vivienda)) +
  geom_bar(position = "stack", fill = "lightblue") +
  labs(x = "Fecha", y = "Frecuencia") +
  ggtitle("Gráfico de frecuencia por categoría - Evolucion_precio_vivienda") +
  scale_fill_manual(values = c("Decrease" = "red", "Stable" = "green", "Increase" = "blue")) +
  facet_wrap(~ Valoracion_encontrar_empleo)

# VARIACION 
# Crear gráfico de línea para la variación de la confianza del consumidor a lo largo de los años
ggplot(df_completoA, aes(x = year(fecha), y = Confianza_consumidor)) +
  geom_line() +
  labs(x = "Año", y = "Confianza_consumidor") +
  ggtitle("Variación de la Confianza del Consumidor a lo largo de los años")

# INES --------------------------------------------------------------------
library(zoo)

# Representamos la serie con ‘timeseries’
dt <- seq(from=as.Date("2018-01-01"), by='month', length.out=60) # Son 5 años en total
s <- zoo(x=df_completo$Confianza_consumidor, order.by=dt)
autoplot(s) + ggtitle('Confianza del consumidor entre 2018 y 2022') + xlab("Meses") + ylab("ICC")


# Crear secuencia de fechas mensuales
dt <- seq(as.Date("2018-01-01"), by = "month", length.out = 60)

# Crear objeto zoo con la variable de confianza del consumidor
s <- zoo(x = df_reducido$Confianza_consumidor, order.by = dt)

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
s <- zoo(x = datos_gasolina$A95, order.by = dt)

# Graficar la serie temporal
autoplot.zoo(s) +
  ggtitle("Confianza del Consumidor entre 2007 y 2011") +
  xlab("Meses") +
  ylab("Confianza del Consumidor")

rm(dt, s, orden_meses, x, precios_combustible, estadisticas_anuales_A95, estadisticas_anuales_Diesel,
   estadisticas_anuales_GLP, datos_combinados)

# DF_REDUCIDO -------------------------------------------------------------

# Vamos a intentar reducirLAS OBSERVACIONES DE LOS MESES
# grafico de observaciones por mes y año 
ggplot(df_completo, aes(x = Year, fill = factor(MES))) +
  geom_bar(position = "dodge")
table(df_completo$Year, df_completo$MES)

set.seed(123)  # Establece una semilla para reproducibilidad

df_reducido <- data.frame()  # Crea un dataframe vacío para almacenar los datos reducidos

# Define el número de observaciones objetivo para cada combinación de año y mes
num_observaciones <- c(400, 450, 500, 500, 550)

# Itera sobre todos los años y meses
for (ano in unique(df_completo$Year)) {
  for (mes in unique(df_completo$MES)) {
    # Filtra las observaciones para el año y mes actual
    df_filtrado <- subset(df_completo, Year == ano & MES == mes)
    
    # Verifica si hay más observaciones que el objetivo
    if (nrow(df_filtrado) > num_observaciones[ano - 2018 + 1]) {
      # Obtiene una muestra aleatoria del tamaño objetivo
      df_muestra <- df_filtrado[sample(nrow(df_filtrado), num_observaciones[ano - 2018 + 1]), ]
      
      # Agrega la muestra al dataframe reducido
      df_reducido <- rbind(df_reducido, df_muestra)
    } else {
      # Si hay menos observaciones que el objetivo, agrega todas las observaciones disponibles
      df_reducido <- rbind(df_reducido, df_filtrado)
    }
  }
}

rm(ano, mes, num_observaciones, df_muestra, df_filtrado)
table(df_reducido$Year, df_reducido$MES)
# SE HA QUEDADO ESTO de forma que 2018: 400 observaciones, 2019 450, 
# 2020 500, 2021 500y 2022 550  
ggplot(df_reducido, aes(x = Year, fill = factor(MES))) +
  geom_bar(position = "dodge")

library(inspectdf)
x <- inspect_types(df_reducido)
show_plot(x)

names(df_reducido)




# PDP ---------------------------------------------------------------------

library(pdp)

# Definir el dataframe y la variable objetivo
df_completoA <- data.frame(Confianza_consumidor, Valoracion_sit_ec_espana, Evolucion_precio_vivienda, Valoracion_encontrar_empleo)

# Calcular los gráficos de PDP para las variables seleccionadas
pdp_1 <- partial(df_completoA, pred.var = "Valoracion_sit_ec_espana", plot = TRUE)
pdp_2 <- partial(df_completoA, pred.var = "Evolucion_precio_vivienda", plot = TRUE)
pdp_3 <- partial(df_completoA, pred.var = "Valoracion_encontrar_empleo", plot = TRUE)


# Carga de paquetes necesarios
library(pdp)
library(ggplot2)

# Función para guardar gráficos
guardar_grafico <- function(plt, nombre_archivo) {
  ggsave(nombre_archivo, plot = plt, device = "jpeg", dpi = 300, width = 8, height = 6)
  print(paste("El gráfico ha sido guardado en:", nombre_archivo))
}

# Función para graficar el Partial Dependence Plot
plot_pdp <- function(model, data, feature_name, title_name) {
  pdp_dist <- pdp::partial(model, pred.var = feature_name, pred.data = data)
  plt <- ggplot(pdp_dist, aes_string(x = feature_name, y = "yhat")) +
    geom_line() +
    labs(x = title_name) +
    theme_minimal()
  
  print(plt)
  guardar_grafico(plt, paste0(title_name, ".jpg"))
}

# Variables seleccionadas
variables_seleccionadas <- c("Valoracion_sit_ec_espana", "Evolucion_precio_vivienda", "Valoracion_encontrar_empleo")

# Partial Dependence Plot
for (variable in variables_seleccionadas) {
  plot_pdp(arbol_final, df_completo, variable, variable)
}
