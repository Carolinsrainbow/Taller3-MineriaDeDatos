### Taller 3 

## Ingreso de datos
datos <- read.csv("/Users/user/Desktop/lastfm.csv",sep=";", header=TRUE)

## Incorporación de paquetes
install.packages("arules")
install.packages("arulesViz")

library(arules)
library(arulesViz)
library(tidyverse)

## ANÁLISIS BASE 

##Visualización 6 primeros por defecto
head(datos)

## Resumen estadístico
summary(datos)

## Cantidad de bandas 
length(unique(datos$artist))

##Cantidad de tipos de usuarios
length(unique(datos$user))

##cantidad de sub categorias de artistas
length(unique(datos$size))

##cantidad de sub categorias de artistas
length(unique(datos$artist_id))

## Gráficas en base al tamaño 
mean(datos$size)
range(datos$size)
hist(x=datos$size)

## Creación de la tabla de los 10 superiores 

datos %>%
  count(artist) %>%
  arrange(desc(n)) %>%
  top_n(10)

## 
plot(datos$artist, method = "paracoord")
plot(datos$artist, method = "graph")



# Generamos la lista de transacciones a utilizar
write.table(datos, file = tmp <- file(), row.names = FALSE)
listas <- read.transactions(tmp,
                            format = "single",
                            header = TRUE,
                            cols = c("user", "artist"))
close(tmp)
summary(listas)

# Recomendacion: no utilizar el comando image(listas) pues son demasiadas

resultados2 <- apriori(listas,
                       parameter = list(supp = 0.01,
                                        conf = 0.5,
                                        minlen = 2,
                                        maxlen = 10,
                                        maxtime = 5))


# Analizamos los resultados
summary(resultados2)
inspect(resultados2)

# Algunas representaciones graficas
plot(resultados2, method = "paracoord")
plot(resultados2, method = "graph")


# Analisis ordenado por criterios
reglas_conf <- sort(resultados2,
                    by = "confidence",
                    decreasing = TRUE)
inspect(head(reglas_conf))


reglas_lift <- sort (resultados2,
                     by = "lift",
                     decreasing=TRUE)
inspect(head(reglas_lift))