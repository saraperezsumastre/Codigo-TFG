library(xgboost)

# TRAIN Y TEST ------------------------------------------------------------

train_boosting <- df_reducido %>%
  filter(Year %in% c(2018,2019, 2020, 2021))

# TEST
test_boosting <- df_reducido %>%
  filter(Year == 2022)

# separar variable objetivo de las varialbes predictoras
# copia de train
train_x_boost <- train_boosting
# eliminamos variable objetivo de la copia
train_x_boost$Confianza_consumidor <- NULL
# variable objetivo: y
train_y_boost <- train_boosting$Confianza_consumidor

# lo mismo con test
test_x_boost <- test_boosting
test_x_boost$Confianza_consumidor <- NULL
test_y_boost <- test_boosting$Confianza_consumidor

# MODELO 1 BOOSTING -------------------------------------------------------

# convertir dataframe en matriz
train_x_boost <- as.matrix(train_x_boost)
test_x_boost <- as.matrix(test_x_boost)

# matriz especifica de xgboost
# label = es para meter ahi en la matriz el valor de la variable objetivo
dtrain <- xgb.DMatrix(train_x_boost, label = train_y_boost)
dtest <- xgb.DMatrix(test_x_boost, label = test_y_boost)

# entrenamiento de boosting
# minimo tenemos que decir el nº de observaciones o iteraciones 
# en boosting eso es con nrounds
mod_boost <- xgb.train(data = dtrain,
                       nrounds = 5)
# prediccion
pred_boost <- predict(mod_boost, newdata = dtest)

# en cada iteracion enseñanos el error que comete el modelo
# watchlist.. es para ver el rmse en train
# eval es para que en cada iteracion este el  error en test
mod_boost <- xgb.train(data = dtrain,
                       nrounds = 5,
                       watchlist = list(train = dtrain,
                                        eval = dtest)
)
# esto es un hiperparametro y no sabemos cuantas iteraciones necesitamos
# vamos a usar parada temprana
mod_boost <- xgb.train(data = dtrain,
                       nrounds = 200,
                       watchlist = list(train = dtrain,
                                        eval = dtest),
                       early_stopping_rounds = 10
)
# el algoritmo como max va a hacer 200 iteraciones
# si llega el momento que hace 10 sucesiones sucesivas
# y no mejora, para

mod_boost <- xgb.train(data = dtrain,
                       nrounds = 1000,
                       watchlist = list(train = dtrain,
                                        eval = dtest),
                       early_stopping_rounds = 10,
                       params = list(
                         max_depth = 5,
                         colsample_bytree =0.5,
                         subsample = 0.5
                       )
)

# Hacer predicciones en los datos de prueba
pred_test_boost <- predict(mod_boost, dtest)

# Calcular el MSE
mse1 <- mean((pred_test_boost - test_y_boost)^2)
# Imprimir el MSE
cat("El MSE en los datos de prueba es:", mse1)
# ES 138.52
# Calcular el RMSE
rmse1 <- sqrt(mse1)
cat("El RMSE en los datos de prueba es:", rmse1)
# es 16.97


# CROSS ENTROPY LOSS
# Hacer predicciones de probabilidad en los datos de prueba
pred_prob <- predict(mod_boost, dtest, type = "prob")
# Calcular la cross-entropy loss
cross_entropy_loss <- -mean(log(pred_prob) * as.matrix(test_y_boost))
# Imprimir la cross-entropy loss
cat("La cross-entropy loss en los datos de prueba es:", cross_entropy_loss)
# ES -285.2573

# GRAFICOS
# Grafico evolucion del error
# # Graficar evolución del error
# ggplot(history, aes(x = iteration, y = dtrain)) + geom_line(aes(y = dtest, color = "test")) + 
#   geom_line(aes(y = dtrain, color = "train")) + 
#   scale_color_manual(values = c("train" = "blue", "test" = "red")) + 
#   labs(x = "Número de iteración", y = "Error", title = "Evolución del error durante el Boosting")


# GRAFICO PREDICCIONES VS OBSERVACIONES
# Hacer predicciones en los datos de prueba
pred_test_graf <- predict(mod_boost, dtest)

# Crear dataframe con predicciones y observaciones
results <- data.frame(observed = test_y_boost, predicted = pred_test_graf)

# Graficar predicciones vs observaciones
ggplot(results, aes(x = observed, y = predicted)) + geom_point(alpha = 0.5) + geom_abline(slope = 1, intercept = 0) +
  labs(x = "Observado", y = "Predicho", title = "Predicciones vs Observaciones")

summary(mod_boost)
summary(pred_prob)


# VALIDACIÓN CRUZADA BOOSTING ---------------------------------------------
# Realizar validación cruzada
cv_mod_boost <- xgb.cv(data = dtrain,
                       params = list(
                         max_depth = 5,
                         colsample_bytree =0.5,
                         subsample = 0.5
                       ),
                       nrounds = 1000,
                       nfold = 5,
                       early_stopping_rounds = 10,
                       metrics = "rmse",
                       stratified = FALSE
)

# Obtener el número óptimo de iteraciones
best_iteration <- cv_mod_boost$best_iteration

# Imprimir el número óptimo de iteraciones
cat("Número óptimo de iteraciones:", best_iteration, "\n")

# Obtener la métrica RMSE promedio en validación cruzada
rmse_cv <- min(cv_mod_boost$evaluation_log$test_rmse_mean)

# Imprimir el RMSE promedio en validación cruzada
cat("RMSE promedio en validación cruzada:", rmse_cv, "\n")


# ANALISIS DE RESIDUOS MODELO 1 BOOSTING ----------------------------------

# Obtener los residuos del modelo
residuals <- pred_test_boost - test_y_boost

# Gráfico de residuos vs predicciones
ggplot(data.frame(residuals, pred_test_boost), aes(x = pred_test_boost, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Predicciones", y = "Residuos", title = "Análisis de Residuos")

# MODELO 2 BOOSTING -------------------------------------------------------

# Intentar hacer lo mismo pero que no salga con year mes y variacion a ver
# si encontramos otro buen modelo 

# Copia de train
train_x_boost_sin <- train_boosting

# Eliminar variables predictoras
train_x_boost_sin$Year <- NULL
train_x_boost_sin$MES <- NULL
train_x_boost_sin$Variacion <- NULL
train_x_boost_sin$Confianza_consumidor <- NULL

# Variable objetivo: y
train_y_boost_sin <- train_boosting$Confianza_consumidor

# Lo mismo con test
test_x_boost_sin <- test_boosting
test_x_boost_sin$Year <- NULL
test_x_boost_sin$MES <- NULL
test_x_boost_sin$Variacion <- NULL
test_x_boost_sin$Confianza_consumidor <- NULL
test_y_boost_sin <- test_boosting$Confianza_consumidor

# Convertir dataframes en matrices
train_x_boost_sin <- as.matrix(train_x_boost_sin)
test_x_boost_sin <- as.matrix(test_x_boost_sin)

# matriz especifica de xgboost
# label = es para meter ahi en la matriz el valor
# de la variable objetivo
dtrain_sin <- xgb.DMatrix(train_x_boost_sin, label = train_y_boost_sin)
dtest_sin <- xgb.DMatrix(test_x_boost_sin, label = test_y_boost_sin)

mod_boost_sin <- xgb.train(data = dtrain_sin,
                           nrounds = 5,
                           watchlist = list(train = dtrain_sin,
                                            eval = dtest_sin)
)
# esto es un hiperparametro y no sabemos cuantas iteraciones necesitamos
# vamos a usar parada temprana
mod_boost_sin <- xgb.train(data = dtrain_sin,
                           nrounds = 200,
                           watchlist = list(train = dtrain_sin,
                                            eval = dtest_sin),
                           early_stopping_rounds = 10
)
# el algoritmo como max va a hacer 200 iteraciones
# si llega el momento que hace 10 sucesiones sucesivas
# y no mejora, para

mod_boost_sin <- xgb.train(data = dtrain_sin,
                           nrounds = 200,
                           watchlist = list(train = dtrain_sin,
                                            eval = dtest_sin),
                           early_stopping_rounds = 10,
                           params = list(
                             max_depth = 5,
                             colsample_bytree =0.5,
                             subsample = 0.5
                           )
)

# Hacer predicciones en los datos de prueba
pred_test_boost_sin <- predict(mod_boost_sin, dtest_sin)

# Calcular el MSE
mse_sin <- mean((pred_test_boost_sin - test_y_boost_sin)^2)
# Imprimir el MSE
cat("El MSE en los datos de prueba es:", mse_sin)
# ES 175.52
# Calcular el RMSE
rmse_sin <- sqrt(mse_sin)
cat("El MSE en los datos de prueba es:", rmse_sin)
# No esta mal, es 13.20

# CROSS ENTROPY LOSS
# Hacer predicciones de probabilidad en los datos de prueba
pred_prob_sin <- predict(mod_boost_sin, dtest_sin, type = "prob")
# Calcular la cross-entropy loss
cross_entropy_loss_sin <- -mean(log(pred_prob_sin) * as.matrix(test_y_boost_sin))
# Imprimir la cross-entropy loss
cat("La cross-entropy loss en los datos de prueba es:", cross_entropy_loss_sin)
# ES -285.7383

# GRAFICO PREDICCIONES VS OBSERVACIONES
# Hacer predicciones en los datos de prueba
pred_test_graf_sin <- predict(mod_boost_sin, dtest_sin)

# Crear dataframe con predicciones y observaciones
results_sin <- data.frame(observed = test_y_boost_sin, predicted = pred_test_graf_sin)

# Graficar predicciones vs observaciones
ggplot(results_sin, aes(x = observed, y = predicted)) + geom_point(alpha = 0.5) + geom_abline(slope = 1, intercept = 0) + 
  labs(x = "Observado", y = "Predicho", title = "Predicciones vs Observaciones")


# MODELO 3 BOOSTING -------------------------------------------------------

# otro codigo distinto 
library(xgboost)
library(caret)
library(Metrics)
library(ggplot2)

set.seed(123)

# Definir los parámetros del modelo
params <- list(
  objective = "reg:squarederror",
  booster = "gbtree",
  eta = 0.1,
  gamma = 0,
  max_depth = 3,
  min_child_weight = 1,
  subsample = 1,
  colsample_bytree = 1,
  eval_metric = "rmse")

# Entrenar el modelo
mod_boost2 <- xgboost(data = dtrain, label = train_y_boost, params = params, nrounds = 100)

# Realizar las predicciones sobre los conjuntos de entrenamiento y prueba
pred_train <- predict(mod_boost, train_x_boost)
pred_test <- predict(mod_boost, test_x_boost)

# Calcular los errores de entrenamiento y prueba
rmse_train <- RMSE(pred_train, train_y_boost)
rmse_test <- RMSE(pred_test, test_y_boost)

# Calcular el coeficiente de determinación (R2) de entrenamiento y prueba
r2_train <- R2(pred_train, train_y_boost)
r2_test <- R2(pred_test, test_y_boost)

# Calcular los residuos de entrenamiento y prueba
resid_train <- pred_train - train_y_boost
resid_test <- pred_test - test_y_boost

# Graficar los residuos de entrenamiento y prueba
ggplot() +
  geom_point(aes(x = pred_train, y = resid_train), alpha = 0.5, color = "blue") +
  geom_point(aes(x = pred_test, y = resid_test), alpha = 0.5, color = "red") +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  labs(x = "Predicciones", y = "Residuos", title = "Gráfico de Residuos")


# MODELO 3 PRUEBAS  -------------------------------------------------------
# Con el algoritmo de adaboost
# Partición de los datos
set.seed(1)
df <- df_completo
nobs <- nrow(df)
# itrain <- sample(nobs, 0.8 * nobs)
train2 <- subset(df, Year %in% c(2018:2021))
test2 <- subset(df, Year == 2022)
library(ada)

# Empezamos con el modelo
# ada.boost <- ada(Confianza_consumidor ~ ., data = train2, type = "real",
#                  control = rpart.control(maxdepth = 2, cp = 0, minsplit = 10, xval = 0),
#                  iter = 100, nu = 0.05)
# No vale porque no puede manejar variables de respuesta 
# que tengan más de dos clases

# Alternativa
library(gbm)

# Ajustar el modelo de boosting
mod_boost_prueba <- gbm(Confianza_consumidor ~ ., data = train, distribution = "gaussian",
                 n.trees = 100, interaction.depth = 2, shrinkage = 0.05, 
                 n.minobsinnode = 10, cv.folds = 0)

# Calcular los errores de entrenamiento y prueba
pred_train <- predict(mod_boost_prueba, newdata = train)
rmse_train <- sqrt(mean((pred_train - train$Confianza_consumidor)^2))

pred_test <- predict(mod_boost_prueba, newdata = test)
rmse_test <- sqrt(mean((pred_test - test$Confianza_consumidor)^2))
# RMSE 17.95

# Importancia de las variables
importance_matrix <- xgb.importance(feature_names = colnames(train2[, -1]), 
                                    model = mod_boost_prueba)

# Graficar el ajuste del modelo en entrenamiento y prueba
plot(mod_boost_prueba, i.tree = 1)
plot(mod_boost_prueba, i.tree = 100)


# MODELO 4 PRUEBA ---------------------------------------------------------

# Ajustar el modelo de boosting
mod_boost_prueba2 <- gbm(Confianza_consumidor ~ .-Year -MES -Variacion, data = train2, distribution = "gaussian",
                        n.trees = 100, interaction.depth = 2, shrinkage = 0.05, 
                        n.minobsinnode = 10, cv.folds = 0)

# Calcular los errores de entrenamiento y prueba
pred_train2 <- predict(mod_boost_prueba2, newdata = train2)
rmse_train2 <- sqrt(mean((pred_train2 - train2$Confianza_consumidor)^2))

pred_test2 <- predict(mod_boost_prueba2, newdata = test2)
rmse_test2 <- sqrt(mean((pred_test2 - test2$Confianza_consumidor)^2))
# RMSE 20.45

# Graficar la importancia de las variables
importance_matrix <- gbm.perf(mod_boost_prueba2, method = "importance")
plot(importance_matrix, main = "Importancia de Variables")

# Graficar el ajuste del modelo en entrenamiento y prueba
plot(mod_boost_prueba2, i.tree = 1)
plot(mod_boost_prueba2, i.tree = 100)




# MODELO 5 ----------------------------------------------------------------

# Variables informe



# MODELO 6 ----------------------------------------------------------------

# 20 variables de correlacion
