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

# Cargar base de datos ----------------------------------------------------

# EN FORMATO SPSS O SAV
library(haven)
basedatos <- as.data.frame(read_sav("Enaho01-2017-100.sav"))
basedatos <- read_sav("FID_3263.sav")

# Pero nos vamos a quedar en formato csv
# porque son todos int, en sav no eran tipo int
basedatos <- read.csv("FID_3264.csv")
nun <- read.csv("df_completo.csv")

# LIMPIEZA ----------------------------------------------------------------

# Ver base de datos
view(basedatos)
# Primeras observaciones
head(basedatos)
# Ultimas observaciones
tail(basedatos)
# Ver la dimension de la base de datos
dim(basedatos)

# Para explorar las variables
glimpse(basedatos)

# ELIMINAR NAS ------------------------------------------------------------
# Ver si hay variables con valores en blanco
# YA NOS CREAMOS LA BASE DE DATOS LIMPIA
# Eliminar filas con valores faltantes
datos_limpios <- basedatos[complete.cases(basedatos), ]
is.na(datos_limpios)
# Eliminar columnas con valores faltantes
datos_limpios <- basedatos[, colSums(is.na(basedatos)) == 0]

filter(basedatos, B.7>=8)

datos_limpios[datos_limpios$B7 >= 8, ]
adv <- basedatos[-which(basedatos$B.7 %in% boxplot.stats(basedatos$B.7)$out),]
adv <- basedatos[-which(basedatos$A.1 %in% boxplot.stats(basedatos$A.1)$out),]

#datos_limpios <- datos_limpios %>% 
  #select(-X)
# DATOS INDICE ------------------------------------------------------------

indice <- read.csv("indice_confianza2.csv", sep = ";")

# añadimos columna que sea la variacion con respecto al año anterior
# Verificar el tipo de datos de la columna Confianza_consumidor
class(indice$Confianza_consumidor)
# tipo character
# hay que hacer que el separador no sea , sino .

# Reemplazar las comas por puntos en la columna de datos de confianza del consumidor
indice$Confianza_consumidor <- as.numeric(gsub(",", ".", indice$Confianza_consumidor))
# Verificar que la variable "Confianza_consumidor" ahora es numérica
str(indice)

# Calcular la variación del índice
variacion <- c(NA, diff(indice$Confianza_consumidor) / indice$Confianza_consumidor[-length(indice$Confianza_consumidor)])

# Crear una nueva columna con la variación
indice$variacion1 <- round(variacion * 100, 2)
# Eliminar la antigua columna de Variacion y cambiar el nombre para que sea la actual 

indice <- indice %>% 
  select(-Variacion)
names (indice)[4] = "Variacion"


# El primer valor del indice da NA porque no estaba la observacion de 2017
# sustituir el NA por el valor que es la variacion
indice$Variacion <- ifelse(is.na(indice$Variacion), -0.15, indice$Variacion)

# VARIABLES B.1.1 ----------------------------------------------------------
library(dplyr)

# VARIABLE B.1.2.1: 
# Adquisición para el hogar de algún bien: automóvil/moto
# Valores de si. no. no sabe, estan en columnas con variables diferentes
# queremos juntasr todas las columnas en la misma y que salga el numero
# correspondiente a cada una de ellas

names(datos_limpios)
# Crea un data frame
df <- data.frame(col1 = datos_limpios[15], col2 =datos_limpios[16], col3 = datos_limpios[17],
                 col4 = datos_limpios[18], col5 = datos_limpios[19])

# Cambia los valores de las columnas
df <- df %>% 
  mutate(B.1.2.1_2 = ifelse(B.1.2.1_2 == 1, 2, B.1.2.1_2),
         B.1.2.1_3 = ifelse(B.1.2.1_3 == 1, 3, B.1.2.1_3),
         B.1.2.1_4 = ifelse(B.1.2.1_4 == 1, 4, B.1.2.1_4),
         B.1.2.1_5 = ifelse(B.1.2.1_5 == 1, 5, B.1.2.1_5))

df %>%
  mutate(col_final = ifelse(B.1.2.1_1 != 0,B.1.2.1_1, 
                            ifelse(B.1.2.1_2 != 0, B.1.2.1_2, 
                                   ifelse(B.1.2.1_3 != 0, B.1.2.1_3,
                                          ifelse(B.1.2.1_4 != 0, B.1.2.1_4,
                                                 ifelse(B.1.2.1_5 != 0, B.1.2.1_5,0))))))

df$col_final <- ifelse(rowSums(df != 0) > 0, 1, 0)
df$col_sum <- rowSums(df[,c("B.1.2.1_1", "B.1.2.1_2", "B.1.2.1_3", "B.1.2.1_4", "B.1.2.1_5")], na.rm = TRUE)

df_final <- cbind(datos_limpios, df$col_sum)
names(df_final)[63] = "Adquisicion_automovil"

# VARIABLE B.1.2.2
names(df_final)
# Crea un data frame
df2 <- data.frame(col1 = datos_limpios[20], col2 =datos_limpios[21], col3 = datos_limpios[22],
                  col4 = datos_limpios[23], col5 = datos_limpios[24])

# Cambia los valores de las columnas
df2 <- df2 %>% 
  mutate(B.1.2.2_2 = ifelse(B.1.2.2_2 == 1, 2, B.1.2.2_2),
         B.1.2.2_3 = ifelse(B.1.2.2_3 == 1, 3, B.1.2.2_3),
         B.1.2.2_4 = ifelse(B.1.2.2_4 == 1, 4, B.1.2.2_4),
         B.1.2.2_5 = ifelse(B.1.2.2_5 == 1, 5, B.1.2.2_5))
# Junta las columnas en una nueva columna
# Que solo tenga las observaciones distintas de 0

df2 %>%
  mutate(col_final = ifelse(B.1.2.2_1 != 0,B.1.2.2_1, 
                            ifelse(B.1.2.2_2 != 0, B.1.2.2_2, 
                                   ifelse(B.1.2.2_3 != 0, B.1.2.2_3,
                                          ifelse(B.1.2.2_4 != 0, B.1.2.2_4,
                                                 ifelse(B.1.2.2_5 != 0, B.1.2.2_5,0))))))

df2$col_final <- ifelse(rowSums(df2 != 0) > 0, 1, 0)
df2$col_sum <- rowSums(df2[,c("B.1.2.2_1", "B.1.2.2_2", "B.1.2.2_3", "B.1.2.2_4", "B.1.2.2_5")], na.rm = TRUE)

df_final <- cbind(df_final, df2$col_sum)

names(df_final)[64] = "Adquisicion_mueble"

# VARIABLE B.1.2.3
names(datos_limpios)
names(df_final)
# Crea un data frame
df3 <- data.frame(col1 = datos_limpios[25], col2 =datos_limpios[26], col3 = datos_limpios[27],
                  col4 = datos_limpios[28], col5 = datos_limpios[29])

# Cambia los valores de las columnas
df3 <- df3 %>% 
  mutate(B.1.2.3_2 = ifelse(B.1.2.3_2 == 1, 2, B.1.2.3_2),
         B.1.2.3_3 = ifelse(B.1.2.3_3 == 1, 3, B.1.2.3_3),
         B.1.2.3_4 = ifelse(B.1.2.3_4 == 1, 4, B.1.2.3_4),
         B.1.2.3_5 = ifelse(B.1.2.3_5 == 1, 5, B.1.2.3_5))

df3 %>%
  mutate(col_final = ifelse(B.1.2.3_1 != 0,B.1.2.3_1, 
                            ifelse(B.1.2.3_2 != 0, B.1.2.3_2, 
                                   ifelse(B.1.2.3_3 != 0, B.1.2.3_3,
                                          ifelse(B.1.2.3_4 != 0, B.1.2.3_4,
                                                 ifelse(B.1.2.3_5 != 0, B.1.2.3_5,0))))))

df3$col_final <- ifelse(rowSums(df3 != 0) > 0, 1, 0)
df3$col_sum <- rowSums(df3[,c("B.1.2.3_1", "B.1.2.3_2", "B.1.2.3_3", "B.1.2.3_4", "B.1.2.3_5")], na.rm = TRUE)

df_final <- cbind(df_final, df3$col_sum)

names(df_final)[65] = "Adquisicion_electrodomestico_grande"
names(df_final)

# VARIABLE B.1.2.4
# Crea un data frame
df4 <- data.frame(col1 = datos_limpios[30], col2 =datos_limpios[31], col3 = datos_limpios[32],
                  col4 = datos_limpios[33], col5 = datos_limpios[34])

# Cambia los valores de las columnas
df4 <- df4 %>% 
  mutate(B.1.2.4_2 = ifelse(B.1.2.4_2 == 1, 2, B.1.2.4_2),
         B.1.2.4_3 = ifelse(B.1.2.4_3 == 1, 3, B.1.2.4_3),
         B.1.2.4_4 = ifelse(B.1.2.4_4 == 1, 4, B.1.2.4_4),
         B.1.2.4_5 = ifelse(B.1.2.4_5 == 1, 5, B.1.2.4_5))

df4 %>%
  mutate(col_final = ifelse(B.1.2.4_1 != 0,B.1.2.4_1, 
                            ifelse(B.1.2.4_2 != 0, B.1.2.4_2, 
                                   ifelse(B.1.2.4_3 != 0, B.1.2.4_3,
                                          ifelse(B.1.2.4_4 != 0, B.1.2.4_4,
                                                 ifelse(B.1.2.4_5 != 0, B.1.2.4_5,0))))))

df4$col_final <- ifelse(rowSums(df4 != 0) > 0, 1, 0)
df4$col_sum <- rowSums(df4[,c("B.1.2.4_1", "B.1.2.4_2", "B.1.2.4_3", "B.1.2.4_4", "B.1.2.4_5")], na.rm = TRUE)

df_final <- cbind(df_final, df4$col_sum)

names(df_final)[6] = "Adquisicion_electrodomestico_pequeño"
names(df_final)

# Despues de esto, nos quedamos con las que hemos metido al final
# entonces eliminamos las de b.1.1.2_1 
# Eliminar las variables de B.1.2.1
df_final <- df_final %>% 
  select(-B.1.2.1_1, -B.1.2.1_2, -B.1.2.1_3, -B.1.2.1_4, -B.1.2.1_5)
# Eliminar las variables de B.1.2.2
df_final <- df_final %>% 
  select(-B.1.2.2_1, -B.1.2.2_2, -B.1.2.2_3, -B.1.2.2_4, -B.1.2.2_5)
# Eliminar las variables de B.1.2.3
df_final <- df_final %>% 
  select(-B.1.2.3_1, -B.1.2.3_2, -B.1.2.3_3, -B.1.2.3_4, -B.1.2.3_5)
# Eliminar las variables de B.1.2.4
df_final <- df_final %>% 
  select(-B.1.2.4_1, -B.1.2.4_2, -B.1.2.4_3, -B.1.2.4_4, -B.1.2.4_5)
names(df_final)

# VARIABLE G.3 ------------------------------------------------------------

#Para el tratamiento de la variable G.3 que es la composicion del hogar con relacion
# de parentesco hay varias variables que no son excluyentes
# La base de datos ha decidido computar cada respuesta como una variable independiente
# G.3_1: Con su conyuge o pareja
# G.3_2: Con sus hijos
# G.3_3: Con su padre y/o madre
# G.3_4: Con hermanos/as
# G.3_5: Con amigos o compañeros trabajo
# G.3_6: Otra situacion como cuidadores o servicio domestico
# G.3_7: No contesta

# Como son binarias quiero ver en cuantas observaciones de esas 
# la suma de las observaciones es 0 
# Crear un vector con los nombres de las variables binarias
variables <- c("G.3_1", "G.3_2", "G.3_3", "G.3_4", "G.3_5", "G.3_6", "G.3_7")

# Contar el número de observaciones en las que todas las variables son 0
num_observaciones <- sum(apply(df_final[, variables], 1, function(x) all(x == 0)))
# 18949
# Son muchas observaciones 

# VECTOR PREVIO QUE LO HACIA EN FORMATO CHARACTER 
# Creamos un vector vacío donde guardaremos los resultados
# respuestas <- vector("character", length = nrow(df_final))
# 
# # Bucle para recorrer cada variable
# for (i in 1:nrow(df_final)) {
#   if (df_final$G.3_1[i] == 1) {
#     if (df_final$G.3_2[i] == 0) {
#       respuestas[i] <- "conyuge"
#     } else {
#       if (df_final$G.3_3[i] == 0) {
#         respuestas[i] <- "conyuge e hijos"
#       } else {
#         respuestas[i] <- "hijos"
#       }
#     }
#   } else {
#     if (df_final$G.3_2[i] == 1) {
#       if (df_final$G.3_3[i] == 0) {
#         respuestas[i] <- "hijos"
#       }
#     } else {
#       if (df_final$G.3_3[i] == 1) {
#         respuestas[i] <- "padres"
#       } else {
#         if (df_final$G.3_4[i] == 1) {
#           if (df_final$G.3_3[i] == 1) {
#             respuestas[i] <- "hermanos y padres"
#           } else {
#             respuestas[i] <- "hermanos"
#           }
#         } else {
#           if (df_final$G.3_5[i] == 1) {
#             respuestas[i] <- "amigos"
#           } else {
#             if (df_final$G.3_6[i] == 1) {
#               respuestas[i] <- "otro"
#             } else {
#               if (df_final$G.3_7[i] == 1) {
#                 respuestas[i] <- "NC"
#               }
#             }
#           }
#         }
#       }
#     }
#   }
# }

# Crear vector para almacenar valores de Composicion_hogar
Composicion_hogar <- rep(NA, nrow(df_final))

# Recorrer cada fila del dataframe
for (i in 1:nrow(df_final)) {
  
  # Condicionales para asignar valor a Composicion_hogar
  if (df_final[i, "G.3_1"] == 1) {
    Composicion_hogar[i] <- 1
    if (df_final[i, "G.3_2"] == 1) {
      Composicion_hogar[i] <- 2
      if (df_final[i, "G.3_3"] == 1) {
        Composicion_hogar[i] <- 3
        if (df_final[i, "G.3_4"] == 1) {
          Composicion_hogar[i] <- 4
        } else {
          Composicion_hogar[i] <- 5
        }
      }
    }
  } else if (df_final[i, "G.3_2"] == 1) {
    Composicion_hogar[i] <- 3
    if (df_final[i, "G.3_3"] == 1) {
      Composicion_hogar[i] <- 4
    } else if (df_final[i, "G.3_4"] == 1) {
      Composicion_hogar[i] <- 5
    } else {
      Composicion_hogar[i] <- 6
      if (df_final[i, "G.3_5"] == 1) {
        Composicion_hogar[i] <- 7
        if (df_final[i, "G.3_6"] == 1) {
          Composicion_hogar[i] <- 8
          if (df_final[i, "G.3_7"] == 1) {
            Composicion_hogar[i] <- 9
          }
        }
      }
    }
  } else if (df_final[i, "G.3_3"] == 1) {
    Composicion_hogar[i] <- 4
  } else if (df_final[i, "G.3_4"] == 1) {
    Composicion_hogar[i] <- 5
  } else if (df_final[i, "G.3_5"] == 1) {
    Composicion_hogar[i] <- 6
    if (df_final[i, "G.3_6"] == 1) {
      Composicion_hogar[i] <- 7
      if (df_final[i, "G.3_7"] == 1) {
        Composicion_hogar[i] <- 9
      }
    }
  } else if (df_final[i, "G.3_6"] == 1) {
    Composicion_hogar[i] <- 8
  } else if (df_final[i, "G.3_7"] == 1) {
    Composicion_hogar[i] <- 9
  } else {
    Composicion_hogar[i] <- 0
  }
}

# Añadir al dataframe
df_final$Composicion_hogar <- Composicion_hogar

# # Creamos la nueva columna con los resultados
# df_final$nueva_columna <- respuestas
# names(df_final)
# # cambiar el nombre a la nueva columna
# names (df_final)[46] = "Composicion_hogar"
# # Quitar valores vacios
# prob3 <- subset(prob3, Composicion_hogar != "")

# Ver los valores de esta respuesta
unique(df_final$Composicion_hogar)
table(df_final$Composicion_hogar)
class(df_final$Composicion_hogar)

# Tiene valores NC, los vamos a eliminar
# df_final <- df_final[!grepl("NC", df_final$Composicion_hogar), ]

# Ahora vamos a eliminar del dataframe las variables separadas de G.3_1...
df_final <- df_final %>% 
  select(-G.3_1, -G.3_2, -G.3_3, -G.3_4, -G.3_5, -G.3_6, -G.3_7)

# Cambiar nombre variables ------------------------------------------------

df_final <- df_final %>% 
  select(-X)
names(df_final)

names (df_final)[6] = "Comunidad"
names (df_final)[7] = "Tamano_Habitat"
names (df_final)[8] = "Sexo"
names (df_final)[9] = "Edad"
names (df_final)[10] = "Provincia"
names (df_final)[11] = "Municipio"
names (df_final)[12] = "Momento_nacionalidad"
names (df_final)[13] = "Conocimiento_lengua"

names (df_final)[14] = "Situacion_economia_hogar"
names (df_final)[15] = "Valoracion_sit_ec_hogar"
names (df_final)[16] = "Personas_paro_entorno"
names (df_final)[17] = "Evolucion_paro_entorno_6m"
names (df_final)[18] = "Valoracion_encontrar_empleo"
names (df_final)[19] = "Valoracion_sit_ec_espana"

# definitivos <- read_excel("Datos_definitivos.xlsx")
# names(definitivos)
names(df_final)
names (df_final)[20] = "Valoracion_mejorar_empleo_esp_6m"
names (df_final)[21] = "Evolucion_ahorro_personal"
names (df_final)[22] = "Valoracion_prospectiva_hogar"
names (df_final)[23] = "Valoracion_prospectiva_espana"

names (df_final)[24] = "Valoracion_inflacion"
names (df_final)[25] = "Evolucion_tipo_interes"
names (df_final)[26] = "Evolucion_precio_vivienda"
names (df_final)[27] = "Intencion_comprar_vivienda"

names (df_final)[28] = "Escolarizacion"
names (df_final)[29] = "Ideologia"
names (df_final)[30] = "Estado_civil"
names (df_final)[31] = "Tamano_hogar"

names (df_final)[32] = "Ingresos_hogar"
names (df_final)[33] = "Situacion_laboral"
names (df_final)[34] = "Tenencia_vivienda"

names(df_final)
# Limpiar un poco el environment
rm(df, df2, df3, df4, Composicion_hogar, variables, variacion)
rm(basedatos, datos_limpios)
rm(i, num_observaciones)

# ELIMINAR VARIABLES  -----------------------------------------------------
# Variables que no son utiles
# Municipio, ESTUDIO, AÑOMES, NUMENTR
df_final <- df_final %>% 
  select(-Municipio, -ESTUDIO, -NUMENTR, -AÑOMES)

# VER VALORES DE VARIABLES ------------------------------------------------

# Funcion unique para ver los valores unicos que toma una variable

# Variable Tamaño_habitat
unique(df_final$Tamano_Habitat)
table(df_final$Tamano_Habitat)
summary(df_final$Tamano_Habitat)

ggplot(df_final, aes(x = Tamano_Habitat)) +
  geom_boxplot()

ggplot(df_final, aes(x = Tamano_Habitat)) +
  geom_histogram()

# Variable Comunidad
unique(df_final$Comunidad)
table(df_final$Comunidad)
summary(df_final$Comunidad)

ggplot(df_final, aes(x = Comunidad)) +
  geom_boxplot()

ggplot(df_final, aes(x = Comunidad)) +
  geom_histogram()

# Variable Sexo
unique(df_final$Sexo)
table(df_final$Sexo)

ggplot(df_final, aes(x = Sexo)) +
  geom_boxplot()

ggplot(df_final, aes(x = Sexo)) +
  geom_histogram()

# Variable Edad
unique(df_final$Edad)
table(df_final$Edad)

ggplot(df_final, aes(x = Edad)) +
  geom_boxplot()

ggplot(df_final, aes(x = Edad)) +
  geom_histogram()

# Variable Provincia
unique(df_final$Provincia)
table(df_final$Provincia)

ggplot(df_final, aes(x = Provincia)) +
  geom_boxplot()

ggplot(df_final, aes(x = Provincia)) +
  geom_histogram()

# Variable Situacion economica del hogar
unique(df_final$Situacion_economia_hogar)
table(df_final$Situacion_economia_hogar)

ggplot(df_final, aes(x = Situacion_economia_hogar)) +
  geom_boxplot()

ggplot(df_final, aes(x = Situacion_economia_hogar)) +
  geom_histogram()

# Variable Valoracion retrospectiva de la situacion economica del hogar
unique(df_final$Valoracion_sit_ec_hogar)
table(df_final$Valoracion_sit_ec_hogar)

ggplot(df_final, aes(x = Valoracion_sit_ec_hogar)) +
  geom_boxplot()

ggplot(df_final, aes(x = Valoracion_sit_ec_hogar)) +
  geom_histogram()

# Variable Numero de personas en su entorno que se hallan en paro
# y buscando trabajo en la actualidad
unique(df_final$Personas_paro_entorno)
table(df_final$Personas_paro_entorno)

ggplot(df_final, aes(x = Personas_paro_entorno)) +
  geom_boxplot()

ggplot(df_final, aes(x = Personas_paro_entorno)) +
  geom_histogram()

# Variable Evolucion del numero de personas del entorno en paro
# y bascuando trabajo en los ultimos 6 meses
unique(df_final$Evolucion_paro_entorno_6m)
table(df_final$Evolucion_paro_entorno_6m)

ggplot(df_final, aes(x = Evolucion_paro_entorno)) +
  geom_boxplot()

ggplot(df_final, aes(x = Evolucion_paro_entorno)) +
  geom_histogram()

# Variable Valoracion retrospectiva de la posibilidad de 
# mejorar o encontrar empleo en España
unique(df_final$Valoracion_encontrar_empleo)
table(df_final$Valoracion_encontrar_empleo)

ggplot(df_final, aes(x = Valoracion_encontrar_empleo)) +
  geom_boxplot()

ggplot(df_final, aes(x = Valoracion_encontrar_empleo)) +
  geom_histogram()

# Variable Variacion retrospectiva de la posibilidad
# de la situacion economica de españa
unique(df_final$Valoracion_sit_ec_españa)
table(df_final$Valoracion_sit_ec_españa)

ggplot(df_final, aes(x = Valoracion_sit_ec_españa)) +
  geom_boxplot()

ggplot(df_final, aes(x = Valoracion_sit_ec_españa)) +
  geom_histogram()

# Variable Valoracion retrospectiva de la posibilidad de 
# de mejorar o encontrar empleo en españa en 6 meses
unique(df_final$Valoracion_mejorar_empleo_esp_6m)
table(df_final$Valoracion_mejorar_empleo_esp_6m)

ggplot(df_final, aes(x = Valoracion_mejorar_empleo_esp_6m)) +
  geom_boxplot()

ggplot(df_final, aes(x = Valoracion_mejorar_empleo_esp_6m)) +
  geom_histogram()

# Variable de Evolucion de la capacidad de ahorro personal 1 año
unique(df_final$Evolucion_ahorro_personal)
table(df_final$Evolucion_ahorro_personal)

ggplot(df_final, aes(x = Evolucion_ahorro_personal)) +
  geom_boxplot()

ggplot(df_final, aes(x = Evolucion_ahorro_personal)) +
  geom_histogram()

# Variable Valoración prospectiva de la situación económica del hogar (6 meses)
unique(df_final$Valoracion_prospectiva_hogar)
table(df_final$Valoracion_prospectiva_hogar)

ggplot(df_final, aes(x = Valoracion_prospectiva_hogar)) +
  geom_boxplot()

ggplot(df_final, aes(x = Valoracion_prospectiva_hogar)) +
  geom_histogram()

# Variable Valoración prospectiva de la situación económica de espana (6 meses)
unique(df_final$Valoracion_prospectiva_espana)
table(df_final$Valoracion_prospectiva_espana)

ggplot(df_final, aes(x = Valoracion_prospectiva_espana)) +
  geom_boxplot()

ggplot(df_final, aes(x = Valoracion_prospectiva_espana)) +
  geom_histogram()

# Variable Valoración prospectiva de la inflacion en espana 1 año
unique(df_final$Valoracion_inflacion)
table(df_final$Valoracion_inflacion)

ggplot(df_final, aes(x = Valoracion_inflacion)) +
  geom_boxplot()

ggplot(df_final, aes(x = Valoracion_inflacion)) +
  geom_histogram()

# Variable Evolucion de los tipos de interes en espana 1 año
unique(df_final$Evolucion_tipo_interes)
table(df_final$Evolucion_tipo_interes)

ggplot(df_final, aes(x = Evolucion_tipo_interes)) +
  geom_boxplot()

ggplot(df_final, aes(x = Evolucion_tipo_interes)) +
  geom_histogram()

# Variable Evolucion del precio de la vivienda en espana
unique(df_final$Evolucion_precio_vivienda)
table(df_final$Evolucion_precio_vivienda)

ggplot(df_final, aes(x = Evolucion_precio_vivienda)) +
  geom_boxplot()

ggplot(df_final, aes(x = Evolucion_precio_vivienda)) +
  geom_histogram()

# Variable Intencion de comprar una vivienda en el proximo año
unique(df_final$Intencion_comprar_vivienda)
table(df_final$Intencion_comprar_vivienda)

ggplot(df_final, aes(x = Intencion_comprar_vivienda)) +
  geom_boxplot()

ggplot(df_final, aes(x = Intencion_comprar_vivienda)) +
  geom_histogram()

# Variable Ideologia
# Escala de autoubicacion ideologica siendo 1 izquierda y 10 derecha
unique(df_final$Ideologia)
table(df_final$Ideologia)

ggplot(df_final, aes(x = Ideologia)) +
  geom_boxplot()

ggplot(df_final, aes(x = Ideologia)) +
  geom_histogram()

# Variable Valoración prospectiva de la inflacion en espana 1 año
unique(df_final$Estado_civil)
table(df_final$Estado_civil)

ggplot(df_final, aes(x = Estado_civil)) +
  geom_boxplot()

ggplot(df_final, aes(x = Estado_civil)) +
  geom_histogram()

# Variable tamano del hogar (cuantas personas viven en su hogar)
unique(df_final$Tamano_hogar)
table(df_final$Tamano_hogar)

ggplot(df_final, aes(x = Tamano_hogar)) +
  geom_boxplot()

ggplot(df_final, aes(x = Tamano_hogar)) +
  geom_histogram()

# Variable Nivel de ingresos del hogar
unique(df_final$Ingresos_hogar)
table(df_final$Ingresos_hogar)

ggplot(df_final, aes(x = Ingresos_hogar)) +
  geom_boxplot()

ggplot(df_final, aes(x = Ingresos_hogar)) +
  geom_histogram()

# Variable Situacion laboral
unique(df_final$Situacion_laboral)
table(df_final$Situacion_laboral)

ggplot(df_final, aes(x = Situacion_laboral)) +
  geom_boxplot()

ggplot(df_final, aes(x = Situacion_laboral)) +
  geom_histogram()

# Variable del regimen de tenencia de la persona entrevistada
unique(df_final$Tenencia_vivienda)
table(df_final$Tenencia_vivienda)

ggplot(df_final, aes(x = Tenencia_vivienda)) +
  geom_boxplot()

ggplot(df_final, aes(x = Tenencia_vivienda)) +
  geom_histogram()

# Variable adquisicion para el hogar de un automovil o moto
unique(df_final$Adquisicion_automovil)
table(df_final$Adquisicion_automovil)

ggplot(df_final, aes(x = Adquisicion_automovil)) +
  geom_boxplot()

ggplot(df_final, aes(x = Adquisicion_automovil)) +
  geom_histogram()

# Variable adquisicion para el hogar de un mueble para el hogar
unique(df_final$Adquisicion_mueble)
table(df_final$Adquisicion_mueble)

ggplot(df_final, aes(x = Adquisicion_mueble)) +
  geom_boxplot()

ggplot(df_final, aes(x = Adquisicion_mueble)) +
  geom_histogram()

# Variable adquisicion para el hogar de un electrodomesticos u ordenadores
unique(df_final$Adquisicion_electrodomestico_grande)
table(df_final$Adquisicion_electrodomestico_grande)

ggplot(df_final, aes(x = Adquisicion_electrodomestico_grande)) +
  geom_boxplot()

ggplot(df_final, aes(x =Adquisicion_electrodomestico_grande)) +
  geom_histogram()

# Variable adquisicion para el hogar de un electrodomesticos u ordenadores
unique(df_final$Adquisicion_electrodomestico_pequeño)
table(df_final$Adquisicion_electrodomestico_pequeño)

ggplot(df_final, aes(x = Adquisicion_electrodomestico_pequeño)) +
  geom_boxplot()

ggplot(df_final, aes(x =Adquisicion_electrodomestico_pequeño)) +
  geom_histogram()

# ELIMINAR OUTLIERS -------------------------------------------------------

# eliminar outliers en una columna
# Eliminamos en algunas variables las caracteristicas de no contesta
# eliminar outliers en una columna
# Eliminamos en algunas variables las caracteristicas de no contesta
df_prueba <- df_final
names(df_prueba)
df_prueba <- df_prueba %>% filter(Personas_paro_entorno < 35)
df_prueba <- df_prueba %>% filter(Evolucion_paro_entorno_6m < 8)
df_prueba <- df_prueba %>% filter(Comunidad < 98)
df_prueba <- df_prueba %>% filter(Escolarizacion < 9)
df_prueba <- df_prueba %>% filter(Estado_civil < 9)
df_prueba <- df_prueba %>% filter(Tamano_hogar < 11)
df_prueba <- df_prueba %>% filter(Momento_nacionalidad < 9)
df_prueba <- df_prueba %>% filter(Conocimiento_lengua < 9)
df_prueba <- df_prueba %>% filter(Situacion_laboral < 9)
df_prueba <- df_prueba %>% filter(Tenencia_vivienda < 8)
df_prueba <- df_prueba %>% filter(Evolucion_ahorro_personal < 8)
df_prueba <- df_prueba %>% filter(Evolucion_precio_vivienda < 8)
df_prueba <- df_prueba %>% filter(Adquisicion_automovil < 5)
df_prueba <- df_prueba %>% filter(Adquisicion_mueble < 5)
df_prueba <- df_prueba %>% filter(Adquisicion_electrodomestico_grande < 5)
df_prueba <- df_prueba %>% filter(Situacion_economia_hogar < 8)
df_prueba <- df_prueba %>% filter(Valoracion_sit_ec_hogar < 8)
df_prueba <- df_prueba %>% filter(Valoracion_encontrar_empleo < 8)
df_prueba <- df_prueba %>% filter(Valoracion_sit_ec_espana < 8)
df_prueba <- df_prueba %>% filter(Valoracion_mejorar_empleo_esp_6m < 8)
df_prueba <- df_prueba %>% filter(Valoracion_inflacion < 8)
df_prueba <- df_prueba %>% filter(Valoracion_prospectiva_hogar < 8)
df_prueba <- df_prueba %>% filter(Valoracion_prospectiva_espana < 8)
df_prueba <- df_prueba %>% filter(Evolucion_tipo_interes < 8)
df_prueba <- df_prueba %>% filter(Intencion_comprar_vivienda < 8)
df_prueba <- df_prueba %>% filter(Adquisicion_electrodomestico_pequeño < 5)
df_prueba <- df_prueba %>% filter(Ingresos_hogar < 8)
df_prueba <- df_prueba %>% filter(Tamano_hogar < 7)

# Variable edad 
# Voy a marcar limite en 75
df_prueba <- df_prueba %>% filter(Edad < 76)

# Variable ideologia voy a quitar NS NC 
# y voy a meter la ideologia
prob3 <- df_prueba

unique(prob3$Ideologia)
table(prob3$Ideologia)

prob3 <- prob3 %>% filter(Ideologia < 97)

# Composicion hogar
# Ver de las 18949 observaciones que habia, cuantas hemos eliminado
# que eran 0, y ahora ver cuantas hay con las observaciones que hemos eliminado
table(prob3$Composicion_hogar)
# Ahora son 0 6502, que vamos a eliminar tambien 
prob3 <- subset(prob3, Composicion_hogar != 0)

# Pero ahora vamos a eliminar la ideologia porque es una variable sensible
prob3 <- prob3 %>% 
  select(-Ideologia)

# UNIR INDICE -------------------------------------------------------------

# Como año tiene Ñ, mejor poner Year 
# En el indice tambien
names(prob3)[1] = "Year"
names(indice)[2] = "MES"
names(indice)[1] = "Year"

# Ver cuantas observaciones hay en total
# Por mes y por year
table(prob3$Year, prob3$MES)

# PROBLEMA: MES en prob3 es 1,2,3,4
# Y MES en indice es Enero, Febrero, Marzo
# Por eso no se rellenaban

# Convertir la columna MES en un vector de caracteres
# indice$MES <- as.character(indice$MES)

meses <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
           "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

# Crear un bucle para reemplazar los valores de MES en indice
for(i in 1:length(meses)){
  indice$MES[indice$MES == meses[i]] <- i
}

# UNIR DATAFRAMES
# Unimos los dataframes df_completo e indice4 por Year y MES
df_completo <- merge(prob3, indice, by = c("Year", "MES"), all.x = TRUE, 
                     by.x = c("Year", "MES"), by.y = c("Year", "MES"),
                     suffixes = c("", "_indice"))

# Ordenamos el dataframe por Year y MES
df_completo <- df_completo[order(df_completo$Year, df_completo$MES),]

# Completamos los valores faltantes en Confianza_consumidor y Variacion
df_completo <- fill(df_completo, Confianza_consumidor, Variacion)

# # Unimos los dataframes df_completo e indice4 por Year y MES
# df_completo2 <- merge(prob3, indice, by = c("Year", "MES"), all.x = TRUE)
# # Ordenamos el dataframe por Year y MES
# df_completo2 <- df_completo2[order(df_completo2$Year, df_completo2$MES),]
# # Completamos los valores faltantes en Confianza_consumidor y Variacion
# df_completo2 <- fill(df_completo2, Confianza_consumidor, Variacion)
# # Verificamos que el nuevo dataframe tenga todos los valores
# view(df_completo)

# Quiero reorganizar para que salga primero Year, Mes, Confianza_consumidor, Variacion
# Y luego el resto de las variables

# RECOLOCAR PARA VER PRIMERO EL VALOR DEL INDICE
df_completo <- df_completo %>% 
  relocate(Confianza_consumidor, .after = MES) %>% 
  relocate(Variacion, .after = Confianza_consumidor)

# REDUCCION VARIABLES -----------------------------------------------------
unique(df_completo$Comunidad)
table(df_completo$Composicion_hogar)
df_completo <- df_completo %>% filter(Adquisicion_electrodomestico_grande < 4)
df_completo <- df_completo %>% filter(Adquisicion_electrodomestico_pequeño < 4)
df_completo <- df_completo %>% filter(Adquisicion_automovil < 4)
df_completo <- df_completo %>% filter(Adquisicion_mueble < 4)
df_completo <- subset(df_completo, Adquisicion_electrodomestico_grande != 0)
# Quitar los menores de 21 años del dataframe
df_completo <- df_completo %>% filter(Edad > 20)
# Quitar Conocimiento_lengua
df_completo <- subset(df_completo, select = -Conocimiento_lengua)
#Quitar escolarizacion
df_completo <- subset(df_completo, select = -Escolarizacion)
# Quitar momento_nacionalidad
df_completo <- subset(df_completo, select = -Momento_nacionalidad)
names(df_completo)

# No hay observaciones repetidas
nuevo_df <- distinct(df_completo)
# Limpiar environment
rm(meses, prob3, df_prueba, df_final)
rm(i)

# Intentar reducir mas las observaciones, vamos a ver la edad
unique(df_completo$Personas_paro_entorno)
table(df_completo$Personas_paro_entorno)
df_completo <- df_completo %>% filter(Personas_paro_entorno < 16)

df_reducido <- df_reducido %>% filter(Personas_paro_entorno < 21)

# Pruebas para reducir ----------------------------------------------------

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

# ELIMINATION -------------------------------------------------------------

# Método de backward elimination
library(MASS)
backward_model <- lm(Confianza_consumidor ~ ., data = df_completo)
summary(backward_model)
reduced_model <- stepAIC(backward_model, direction = "backward")
summary(reduced_model)
# Método de backward elimination sin Year, Mes y variacion
df_sin <- df_completo[, !(names(df_completo) %in% c("MES", "Year", "Variacion"))]
backward_model_sin <- lm(Confianza_consumidor ~ ., data = df_sin)
summary(backward_model_sin)
reduced_model_sin <- stepAIC(backward_model_sin, direction = "backward")
summary(reduced_model_sin)

# Reducción de dimensionalidad con PCA
library(dplyr)
names(df_completo)
#df_completo_pca <- select(df_completo, -c(Year, MES, Comunidad, Tamano_Habitat, Sexo))
df_completo_pca <- subset(df_completo, select = -c(Year, MES, Variacion))
library(FactoMineR)
res.pca <- PCA(df_completo_pca, graph = FALSE)
library(factoextra)
fviz_eig(res.pca)

rm(df_completo_clean, df_completo_sample)

plot(df_completo$MES,df_completo$Confianza_consumidor)


# EXCEL CON EL DF FINAL  --------------------------------------------------

# Crear un excel con los datos limpios 
install.packages("openxlsx")
library(openxlsx)

# crea un nuevo archivo de Excel
wb <- createWorkbook()

# agrega una hoja de cálculo al archivo
addWorksheet(wb, "Mi hoja de datos")

# agrega los datos a la hoja de cálculo
writeData(wb, "Mi hoja de datos", prob3)

# aplica formato a las celdas
addStyle(wb, "Mi hoja de datos", 
         style = createStyle(fontColour = "white", fgFill = "#0072C6", halign = "center", textDecoration = "bold"), 
         rows = 1, cols = 1:ncol(df))

addStyle(wb, "Mi hoja de datos", 
         style = createStyle(numFmt = "dd/mm/yyyy", halign = "center"), 
         rows = 2:nrow(df), cols = "Fecha")

# guarda los cambios y cierra el archivo
saveWorkbook(wb, "Mi archivo.xlsx")


# Guardar como CSV --------------------------------------------------------

write.csv(df_completo, "df_completo.csv", row.names = FALSE)
write.csv(df_reducido, "df_reducido.csv", row.names = FALSE)
write.csv(indice, "indicecsv.csv", row.names = FALSE)


# Para pasar a python
class(df_completo$Year)
class(df_completo$MES)
str(df_completo)

train <- df_completo %>%
  filter(Year %in% c(2018,2019, 2020, 2021))

# TEST
test <- df_completo %>%
  filter(Year == 2022)

write.csv(train, "train.csv", row.names = FALSE)
write.csv(test, "test.csv", row.names = FALSE)
