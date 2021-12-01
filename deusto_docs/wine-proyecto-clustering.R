# author: Alex Rayón & Francisco Perez Carrega
# date: Octubre, 2021

# Antes de nada, limpiamos el workspace, por si hubiera alg?n dataset o informaci?n cargada
rm(list = ls())

# Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Instalamos paquetes y cargamos las librerías que vamos a necesitar
install.packages(c('rattle',"corrplot","factoextra","mclust","FactoMineR"))
library(rattle)
library(corrplot)
library(factoextra)
library(mclust)
library(FactoMineR)

# 1.Lectura de los datos
data(wine, package='rattle')
head(wine)
data<- wine

# 2.El objetivo de Clustering
# Obtener grupos optimo de vinos a partir de las caracteristicas quimicas e identificar las diferencias entre estos grupos
## Variables son las caracteristicas - 13 caracteristicas quimicas
dataset <- data[2:14]

# 3.Estandarizar las caracteristicas
Zdata <- scale(dataset)
pairs(Zdata) # matrixplot
correlations <- cor(dataset) # correlation matrix
corrplot(correlations, method="circle") # corrplot

# 4.Aplicar los tres algoritmos de clustering
# El numero optimo de clusters
cluster.optimo <- fviz_nbclust(Zdata, hcut, method = "gap_stat")
cluster.optimo.gap <- cluster.optimo$data$gap
cluster.optimo.gap == max(cluster.optimo.gap)
k <- 3 # numero de clusters optimo

# Hierarchical
# k-means
# GMM - Modelos gaussianos mixtos

# Hierarchical
res.hc <- eclust(Zdata, "hclust", k = k, graph = FALSE) 
fviz_dend(res.hc, rect = TRUE, show_labels = FALSE)
pairs(Zdata,col=res.hc$cluster,pch = 21)

res.hc.clusters <- res.hc$cluster
res.hc <- eclust(Zdata, "hclust", k = k, graph = TRUE) 

# k-means
km.res <- eclust(Zdata, "kmeans", k = k, nstart = 25, graph = FALSE)
fviz_silhouette(km.res)
pairs(Zdata,col=km.res$cluster,pch = 21)

res.km.clusters <- km.res$cluster

eclust(Zdata, "kmeans", k = k, nstart = 25, graph = TRUE)

# GMM
gmm.res = Mclust(Zdata, G = k)
summary(gmm.res, parameters = TRUE)
plot(gmm.res, what = "classification")

res.gmm.clusters <- gmm.res$classification


# 5.Decidir que metodo de cluster es el mas interesante
pca <- PCA(Zdata,graph = TRUE)
# PCA graph - hierarchical

plot(pca$ind$coord[,1:2], pch=16, col=res.hc.clusters, main="HIERARCHICAL")

plot(pca$ind$coord[,1:2], pch=16, col=res.km.clusters, main="k-MEANS")

plot(pca$ind$coord[,1:2], pch=16, col=res.gmm.clusters, main="GMM")

# PCA graph - k-means
eclust(Zdata, "kmeans", k = k, nstart = 25, graph = TRUE)

# conclusion: seleccionamos el k-means como mejor algoritmo de clustering

# 6.Calcular los descriptivos de la nueva clase
misclusters <- res.km.clusters
# PRIMER CLUSTER - DESCRIPTIVOS
df = dataset[misclusters==1,] # seleccionamos todas las variables cuantitativas
estadisticaDescriptiva_C1 <- t(do.call(data.frame, 
                                    list(mean = apply(df, 2, mean),
                                         Desv.Estandar = apply(df, 2, sd),
                                         Mediana = apply(df, 2, median),
                                         IQR = apply(df,2,IQR),
                                         Min = apply(df, 2, min),
                                         Max = apply(df, 2, max),
                                         Rango = apply(df, 2,max)-apply(df, 2,min),
                                         Cuartil1 = apply(df,2,quantile,probs = c(0.25)),
                                         Cuartil3 = apply(df,2,quantile,probs = c(0.75)),
                                         N = apply(df,2,length),
                                         ErrorEstandar = apply(df,2,sd)/sqrt(apply(df,2,length)),
                                         IC95MediaLower = apply(df,2,mean)-1.96*apply(df,2,sd)/sqrt(apply(df,2,length)),
                                         IC95MediaUpper = apply(df,2,mean)+1.96*apply(df,2,sd)/sqrt(apply(df,2,length)),
                                         Varianza = apply(df, 2, var),
                                         Suma = apply(df,2,sum)
                                    )))
# SEGUNDO CLUSTER - DESCRIPTIVOS
df = dataset[misclusters==2,] # seleccionamos todas las variables cuantitativas
estadisticaDescriptiva_C2 <- t(do.call(data.frame, 
                                       list(mean = apply(df, 2, mean),
                                            Desv.Estandar = apply(df, 2, sd),
                                            Mediana = apply(df, 2, median),
                                            IQR = apply(df,2,IQR),
                                            Min = apply(df, 2, min),
                                            Max = apply(df, 2, max),
                                            Rango = apply(df, 2,max)-apply(df, 2,min),
                                            Cuartil1 = apply(df,2,quantile,probs = c(0.25)),
                                            Cuartil3 = apply(df,2,quantile,probs = c(0.75)),
                                            N = apply(df,2,length),
                                            ErrorEstandar = apply(df,2,sd)/sqrt(apply(df,2,length)),
                                            IC95MediaLower = apply(df,2,mean)-1.96*apply(df,2,sd)/sqrt(apply(df,2,length)),
                                            IC95MediaUpper = apply(df,2,mean)+1.96*apply(df,2,sd)/sqrt(apply(df,2,length)),
                                            Varianza = apply(df, 2, var),
                                            Suma = apply(df,2,sum)
                                       )))
# TERCER CLUSTER - DESCRIPTIVOS
df = dataset[misclusters==3,] # seleccionamos todas las variables cuantitativas
estadisticaDescriptiva_C3 <- t(do.call(data.frame, 
                                       list(mean = apply(df, 2, mean),
                                            Desv.Estandar = apply(df, 2, sd),
                                            Mediana = apply(df, 2, median),
                                            IQR = apply(df,2,IQR),
                                            Min = apply(df, 2, min),
                                            Max = apply(df, 2, max),
                                            Rango = apply(df, 2,max)-apply(df, 2,min),
                                            Cuartil1 = apply(df,2,quantile,probs = c(0.25)),
                                            Cuartil3 = apply(df,2,quantile,probs = c(0.75)),
                                            N = apply(df,2,length),
                                            ErrorEstandar = apply(df,2,sd)/sqrt(apply(df,2,length)),
                                            IC95MediaLower = apply(df,2,mean)-1.96*apply(df,2,sd)/sqrt(apply(df,2,length)),
                                            IC95MediaUpper = apply(df,2,mean)+1.96*apply(df,2,sd)/sqrt(apply(df,2,length)),
                                            Varianza = apply(df, 2, var),
                                            Suma = apply(df,2,sum)
                                       )))
