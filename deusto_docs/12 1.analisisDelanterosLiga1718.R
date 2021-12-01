# author: Alex Rayón (alex.rayon.jerez@gmail.com)
# date: Octubre, 2020

# Antes de nada, limpiamos el workspace, por si hubiera algún dataset o información cargada
rm(list = ls())

# Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Cargamos todas las librerías necesarias
# install.packages("ggplot2")
library(ggplot2)
library(clusterSim)
library(caret)
library(dplyr)

# Leemos todos los datos necesarios
delanteros<-read.csv("data/delanteros1718.csv",encoding="UTF-8",header = T)

# Tenemos dos tipos de datos en R:
#   - Numéricos: que son las magnitude escalares. Continuos. Datos tipo numeric.
#   - Categóricos: son magnitudes discretas. Datos de tipo factor.
delanteros$jugador<-as.factor(delanteros$jugador)
delanteros$equipo<-as.factor(delanteros$equipo)

# Vamos a hacer un análisis estadístico básico: observar la distribución de mis valores
summary(delanteros)

#####################################################################
# 1. Métodos de aprendizaje no supervisado: Descriptivos (Pasado)
#     - Análisis clúster (grupos de comportamiento / perfiles)
#     - Reglas de asociación (co-ocurrencias)
#     - Análisis de Componentes Principales (reducir dimensiones, o quitar variables)

# 2. Métodos de aprendizaje supervisado: Predictivos (Futuro)
#     - Clasificación: cuando hago predicciones de variables categóricos o discretas
#     - Regresión: cuando hago predicciones de variables numéricas o continuas

#####################################################################
# Análisis Clúster
#####################################################################
str(delanteros)
delanterosNum<-delanteros[,c(3:12)]
boxplot(delanterosNum, las=2)
datos_norm = data.Normalization(delanterosNum,type="n4",normalization = "column")
boxplot(datos_norm)

# Análisis cluster
#   1. Encontrar el número de perfiles óptimo
#   2. Hacer el análisis clúster (o separación de mis delanteros en ese número de perfiles)
#   3. Interpretar las características de cada uno de esos perfiles

# Vamos a calcular el número óptimo de clústers
#   Método del codo
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
wss <- sapply(1:k.max, 
              function(k){kmeans(datos_norm, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
# ¿Cuándo decae lentamente la curva?

# Podemos en adelante: 
# Agrupar por "datos": pero utilizando los Componentes Principales
kc <- kmeans(na.omit(datos_norm), 6)
kc$centers

# Variamos algunos parámetros para visualizar mejor el gráfico
clusplot(na.omit(delanteros[3:12]), kc$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

# Le asignamos a cada jugador su cluster correspondiente, como una característica más
delanteros$Cluster <- kc$cluster 

# Obtener el significado de los clústers
aggregate(datos_norm, by=list(kc$cluster), FUN=mean)

#####################################################################
# Y ahora vamos a hacer un poco de Machine Learning
#####################################################################
# Vamos a particionar los datos
set.seed(42)
delanterosPred<-delanteros[,c(2:12)] #  Dejo el club, ¿por qué?
index <- createDataPartition(delanterosPred$puntos, p = 0.7, list = FALSE)
train_data <- delanterosPred[index, ]
test_data  <- delanterosPred[-index, ]

model_glm <- caret::train(puntos ~ .,
                          data = train_data,
                          method = "glm",
                          preProcess = c("scale", "center"),
                          trControl = trainControl(method = "repeatedcv", 
                                                   number = 10, 
                                                   repeats = 10, 
                                                   savePredictions = TRUE, 
                                                   verboseIter = FALSE))
model_glm

# Hago predicciones sobre nuevos datos: "ahora voy al examen" (datos test)
predictions <- predict(model_glm, test_data)

data.frame(actual = test_data$puntos,
           predicted = predictions) %>%
  ggplot(aes(x = actual, y = predicted)) +
  geom_jitter() +
  geom_smooth(method = "lm")

# Vamos a probar solo con las variables que realmente guardaban relación
model_glm_2 <- caret::train(puntos ~ penaltiesFallados+asistencias+amarillas+sustituido+titular+goles,
                          data = train_data,
                          method = "glm",
                          preProcess = c("scale", "center"),
                          trControl = trainControl(method = "repeatedcv", 
                                                   number = 10, 
                                                   repeats = 10, 
                                                   savePredictions = TRUE, 
                                                   verboseIter = FALSE))
model_glm_2
predictions_2 <- predict(model_glm_2, test_data)
# ¿Hemos mejorado? Bastante...
data.frame(actual = test_data$puntos,
           predicted = predictions_2) %>%
  ggplot(aes(x = actual, y = predicted)) +
  geom_jitter() +
  geom_smooth(method = "lm")

# ¿De qué nos damos cuenta?
imp <- varImp(model_glm_2, scale = FALSE)
plot(imp)
