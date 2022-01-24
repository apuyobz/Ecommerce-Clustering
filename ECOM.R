library(tidyverse)

datos_ecom <- read.csv("../Data/E-commerce Data.csv")
head(datos_ecom)

# 1. Análisis de datos
library(skimr)

# Estructura general
glimpse(datos_ecom)
skim(datos_ecom)

# Existencia de NA´s
any(!complete.cases(datos_ecom))
map_dbl(datos_ecom, .f = function(x){sum(is.na(x))})

# comportamiento de NA´s
library(mice)
library(VIM)

md.pattern(datos_ecom)

aggr(datos_ecom,
     numbers = T,
     sortVars = T)

# Eliminamos la variable con NA´s puesto que son varios y no nos aporta mucho en el estudio
datos_ecom$CustomerID <- NULL

# Se cambia el formato de la variable invoicedate a ddtt
library(lubridate)

datos_ecom$InvoiceDate <- mdy_hm(datos_ecom$InvoiceDate)
glimpse(datos_ecom)

# Por no aportar mucho al análisis se eliminan el stock y en N° de ident
datos_ecom$InvoiceNo <- NULL
datos_ecom$StockCode <- NULL

# EDA:
# Cantidades prom por país
datos_ecom %>%
  group_by(Country) %>%
  summarise(media_unds = mean(Quantity)) %>%
  arrange(desc(media_unds))

cant_fch <- datos_ecom %>%
  group_by(InvoiceDate) %>%
  summarise(media_unds = mean(Quantity))

cant_fch %>%
  ggplot() +
  geom_freqpoly(aes(InvoiceDate),
                binwidth = 24*60*60) +
  labs(title = "Fechas de Compra vs Unds Prom")


# Precio prom por país
datos_ecom %>%
  group_by(Country) %>%
  summarise(media_price = mean(UnitPrice)) %>%
  arrange(desc(media_price))


# 2. Clustering
library(factoextra)
library(cluster)
library(fpc)
library(NbClust)
library(scales)

# Datos a usar
dat_km <- datos_ecom %>%
  select(Country, Quantity, UnitPrice) %>%
  group_by(Country) %>%
  summarise(avg_quan = mean(Quantity),
            avg_pric = mean(UnitPrice))

ecom_scaled <- as.data.frame(scale(dat_km[, 2:3]))
rownames(ecom_scaled) <- dat_km$Country

# Info para la sugerencia de clusters
nb <- NbClust(ecom_scaled, distance = "euclidean",
              min.nc = 2, max.nc = 15, method = "ward.D2",
              index = "all")

# Gráfico especial de factoextra, para clusters nb
fviz_nbclust(nb) + theme_minimal()

# Probando el kmeans con la recomendación y mirando la coherencia
km<- kmeans(ecom_scaled, 3)

# Gráfico del resultado de los clusters
fviz_cluster(km, ecom_scaled)

# Composición de los clusters
sil_km <- silhouette(km$cluster,
                     dist(ecom_scaled))

sil_sum <- summary(sil_km)
sil_sum

# Gráfica de la composición del clustering
fviz_silhouette(sil_km)
