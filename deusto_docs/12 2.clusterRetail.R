# author: Alex Rayón & Francisco Perez Carrega
# date: Octubre, 2020

# Antes de nada, limpiamos el workspace y cambiamos el directorio de trabajo
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Vamos a cargar las librerías necesarias
library(readr) # Para la lectura del fichero CSV

options(stringsAsFactors = T)
options(scipen = 999)
# Vamos a leer los datos sobre los que vamos a trabajar
datos<-read_csv("12 2.datosRetail.csv")

# Parece que nos ha importado una columna que nos sobra
datos<-datos[,-1]

# Creamos una matriz con el perfil de cliente
clientes<-datos %>%
    group_by(idTarjeta) %>%
    summarize(numProductos=n(),
            numCestas = n_distinct(idTransaccion),
            numMarcas = n_distinct(marca),
            numCategorias = n_distinct(descCategoria),
            numSegmentos = n_distinct(descSegmento),
            tramoHorario = n_distinct(horaCompra),
            diasCompra = n_distinct(diaSemana),
            gastoTotal = sum(importe),
            gastoDesv = sd(importe),
            ofertas = sum(!is.na(idOferta)),
            sumFrescos = sum(descArea == "01 FRESCOS"),
            sumAlimentacion = sum(descArea == "02 ALIMENTACION"),
            sumPromociones = sum(descArea == "08 PROMOCIONES")) %>%
    arrange(-gastoTotal)

# Comprobamos los tipos que nos han quedado
str(clientes)

install.packages("funModeling")
library(funModeling)

df_status(clientes) # ¿Qué vemos?

# Vamos a quitar algún problema que nos ha generado esa no desviación del importe
clientes<-clientes[!is.na(clientes$gastoDesv),]
# ¿Y en lo siguiente qué vemos?
boxplot(clientes$gastoTotal)
clientes<-clientes[!clientes$gastoTotal>1000,] #  ¿Por qué quitamos éste?

# Dado que tenemos todo magnitudes numéricas de caracterización de clientes
#   usamos las mismas para hacer un kMeans
clientes_num<-clientes[,c(2:14)]
boxplot(clientes_num, las=2)

# ¿Qué vemos en el boxplot?
library(clusterSim)
datos_norm = data.Normalization(as.data.frame(clientes_num),
                                type="n4",
                                normalization = "column")
boxplot(datos_norm)

##########################################################################
##########################################################################
# Análisis Cluster
##########################################################################
##########################################################################
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
# ¿Qué vemos?

# Variamos algunos parámetros para visualizar mejor el gráfico
clusplot(clientes_num, kc$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
# Parece evidente un problema que se nos ha olvidado.... ¿lo quitamos?

# Le asignamos a cada jugador su cluster correspondiente, como una característica más
clientes$cluster <- kc$cluster 

##########################################################################
##########################################################################
# Representación visual del Análisis Cluster
##########################################################################
##########################################################################

# The palette with grey:
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Vamos a pintar el clustering con algunas variables
grafica1<-ggplot(clientes,aes(x = numMarcas, 
                              y = ofertas)) +
  geom_point(aes(color = as.factor(cluster)))+
  scale_color_manual(values=cbp1)+
  geom_smooth()+
  theme_light()
grafica1

# Vamos a pintar el clustering con algunas variables
grafica2<-ggplot(clientes,aes(x = sumFrescos, 
                              y = sumAlimentacion)) +
  geom_point(aes(color = as.factor(cluster)))+
  scale_color_manual(values=cbp1)+
  geom_smooth()+
  theme_light()
grafica2