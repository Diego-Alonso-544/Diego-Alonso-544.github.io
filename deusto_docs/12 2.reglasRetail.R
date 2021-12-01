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

##############################################################################################
##############################################################################################
# Reglas de asociación
##############################################################################################
##############################################################################################
# Vamos primero a repasar los pasos para construir una regla de asociación
#   0. Tipos de datos (tienen que ser "transacciones")
#   1. Generar el itemset
#   2. Generar las reglas de asociación
# Vamos a completar los pasos ahora

install.packages("arules")
library(arules)

#   0. Tipos de datos (tienen que ser "transacciones")
transacciones_productos <- split(datos$descProducto,datos$idTransaccion)

# Vamos a controlar los duplicados
listData <- list()
for (i in 1:length(transacciones_productos)) {
  listData[[i]] <- as.character(transacciones_productos[[i]][!duplicated(transacciones_productos[[i]])])
}

#   1. Generar el itemset
cesta <- as(listData, "transactions")

#   2. Generar las reglas de asociación
reglas <- apriori(cesta, parameter = list(support= 0.002 , confidence= 0.01))

# Resumen de las reglas que hemos creado
summary(reglas)

#inspect top 5 rules by highest lift
inspect(head(sort(reglas, by ="lift"),15))

install.packages("arrulesViz")
library(arulesViz)

#Plotting rules
plot(reglas)
# Interactive plots for rules
sel <- plot(reglas, measure=c("support", "lift"), shading="confidence", interactive=FALSE)

# Ahora vamos a indagar un poco por productos
reglas1 <- subset(reglas, subset = rhs %ain% "PIMIENTO ROJO LAMUYO")
inspect(reglas1)

reglas2 <- subset(reglas, subset = lhs %ain%  "ZANAHORIA BOLSA 1 KG" |lhs %ain%  "PIMIENTO VERDE LAMUYO" )
inspect(reglas2)


# También podemos construir reglas solo para un determinado producto (gestión de stocks, layout, etc.)
reglas3<-apriori(data=cesta, 
                 parameter=list(support= 0.001 , confidence= 0.01), 
                 appearance = list(default="lhs",rhs="PIMIENTO ROJO LAMUYO"),
                 control = list(verbose=T))
reglas3<-sort(reglas3, decreasing=TRUE,by="confidence")
inspect(reglas3)

# Representamos visualmente las reglas
library(igraph)
#plot(reglas3,method="graph",interactive=T,shading=NA)
plot(reglas3, method="graph", shading = NA)

# Vamos ahora a estudiar largas y cortas cestas
reglas4<-apriori(data=cesta, 
                 parameter=list(support= 0.001 , confidence= 0.001),
                 control = list(verbose=T))

largasCestas <- subset(reglas4, subset = size(reglas4) >= 3 )
inspect(largasCestas)
plot(largasCestas, method="paracoord")
