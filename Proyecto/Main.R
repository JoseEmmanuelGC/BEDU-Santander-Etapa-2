# Definir el directorio en el que trabajamos actualmente

# Importar los archivos CSV
customers <- read.csv("olist_customers_dataset.csv")
geolocation <- read.csv("olist_geolocation_dataset.csv")
order.items <- read.csv("olist_order_items_dataset.csv")
order.payments <- read.csv("olist_order_payments_dataset.csv")
order.reviews <- read.csv("olist_order_reviews_dataset.csv")
orders <- read.csv("olist_orders_dataset.csv")
products <- read.csv("olist_products_dataset.csv")
sellers <- read.csv("olist_sellers_dataset.csv")
category_names_translation <- read.csv("product_category_name_translation.csv")

states <- unique(customers["customer_state"])

# Distribución de compradores
# Histograma de compras por estado

library(ggplot2)

cs <- data.frame(table(customers["customer_state"]))
ggplot(cs, aes(x = Var1, y = Freq)) + 
    geom_bar(stat = "identity", position = "stack", fill = "#1B6C8C", color = "black") +
    labs( x = "Estado", y = "No. compras") + 
    theme(
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "gray"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray"),
        plot.background = element_rect(fill = "white")
    ) +
    ggtitle("Compras por estado")


library(dplyr)
customers.unique <- distinct(customers[c("customer_zip_code_prefix", "customer_city", "customer_state")])

geolocation.by.states <- list()
customer.by.states <- list()
data.by.states <- list()

for(state in states[,1]) {
  geolocation.by.states[[state]] <- filter(geolocation, geolocation_state==state)
  customer.by.states[[state]] <- filter(customers.unique, customer_state==state)
  data.by.states[[state]] <- merge(x=geolocation.by.states[[state]], 
                          y=customer.by.states[[state]], 
                          by.x ="geolocation_zip_code_prefix", 
                          by.y = "customer_zip_code_prefix")
}


#Formar un solo data frame
dataset <- do.call(rbind, data.by.states)

#Eliminar columnas repetidas 

dataset$customer_city <- NULL ; dataset$customer_state <- NULL

dataset <- rename(dataset, "zip_code" = "geolocation_zip_code_prefix",
                            "latitud" = "geolocation_lat",
                            "longitud" = "geolocation_lng",
                            "city" = "geolocation_city",
                            "state" = "geolocation_state")

#Numero optimo de clusters
library("ggplot2")
library("factoextra")

data4clusters <- dataset[,c(2,3)]

sd.long <- sd(data4clusters$longitud)
sd.lat <- sd(data4clusters$latitud)
#Hay mayor variaci?n con la latitud

mean.lat <- mean(data4clusters$latitud)

#Muestra para poblaci?n infinita

se <- (abs(mean.lat)*0.01)/1.96
n.inf <- (sd.lat**2)/(se**2)

rows <- nrow(data4clusters)

#Correcci?n por muestra finita

n <- n.inf/(1+(n.inf/rows))


#Proporciones
unique.states <- unique(dataset$state)


conteo <- dataset %>% group_by(state) %>% summarise(n = n())
conteo <- mutate(conteo, p = n/rows)


sample_by_state <- list()

set.seed(100)

#Muestreo estratificado

for(i in seq(1,length(conteo$state))) {
  sample_by_state[[as.character(conteo[i,"state"])]] <- sample_n(data.by.states[[as.character(conteo[i,"state"])]],
                                                 size=ceiling(n*as.numeric(conteo[i,"p"])),
                                                  replace = FALSE)
}

dataset_sample <- do.call(rbind, sample_by_state)

dataset_sample <- dataset_sample[,c(2,3)]
rownames(dataset_sample)=NULL

#Numero de clusters
nb_clusters <- fviz_nbclust(dataset_sample, kmeans, method = "silhouette")

#Conglomerados por el metodo kmedias
clusters <- kmeans(dataset_sample, 3, nstart = 1)

centroides <- clusters$centers

composicion <- as.data.frame(clusters$cluster)

#Preparacion de dataset para graficar coordenadas

dataset_sample <- cbind(dataset_sample, composicion)
dataset_sample <- rename(dataset_sample, cluster = "clusters$cluster")

dataset_sample[,4] <- NA ; dataset_sample[,5] <- NA; dataset_sample[,6] <- NA
dataset_sample <- rename(dataset_sample, clat="V4")
dataset_sample <- rename(dataset_sample, clong="V5")

dataset_sample[2639,1] <- centroides[1,1]; dataset_sample[2639,2] <- centroides[1,2];dataset_sample[2639,3] <- 1
dataset_sample[2640,1] <- centroides[2,1]; dataset_sample[2640,2] <- centroides[2,2];dataset_sample[2640,3] <- 2
dataset_sample[2641,1] <- centroides[3,1]; dataset_sample[2641,2] <- centroides[3,2];dataset_sample[2641,3] <- 3


for (i in seq(1,nrow(dataset_sample))){
  if (dataset_sample[i,3]==1){
    dataset_sample[i,4] = centroides[1,1]
    dataset_sample[i,5] = centroides[1,2]
    dataset_sample[i,6] = "CD SAO PABLO"
  }
  else if (dataset_sample[i,3]==2){
    dataset_sample[i,4] = centroides[2,1]
    dataset_sample[i,5] = centroides[2,2]
    dataset_sample[i,6] = "CD BELO HORIZONTE"
  }
  else if (dataset_sample[i,3]==3){
    dataset_sample[i,4] = centroides[3,1]
    dataset_sample[i,5] = centroides[3,2]
    dataset_sample[i,6] = "CD PENDIENTE"
  }
}

dataset_sample[,7] <- 0.1
dataset_sample[2639,7] <- 5
dataset_sample[2640,7] <- 5
dataset_sample[2641,7] <- 5

rownames(dataset_sample) <- NULL
library(plotly)
Sys.setenv(MAPBOX_TOKEN = 11122223333444)

# map projection
geo <- list(
  projection = list(
    type = 'orthographic',
    rotation = list(lon = -100, lat = -40, roll = 0)
  ),
  showland = TRUE,
  landcolor = toRGB("gray95"),
  countrycolor = toRGB("gray80")
)


#Mapa de centros de distribcion
plot_geo() %>%
  add_markers(
    data = dataset_sample, x = ~geolocation_lng, y = ~geolocation_lat, text = ~V6,
    size = ~V7, alpha = 0.6,
    color = ~cluster
  ) %>%
  add_segments(
    data = dataset_sample,
    x = ~clong, xend = ~geolocation_lng,
    y = ~clat, yend = ~geolocation_lat,
    alpha = 0.1, size = I(0.6), color = I("Red"), hoverinfo = "none"
    #color = ~cluster
  ) %>%
  layout(geo = geo, showlegend = FALSE,
         title = 'Propuesta: Ubicaci�n de Centros de distribuci�n')


predict.kmeans <- function(object, newdata){
  centers <- object$centers
  n_centers <- nrow(centers)
  dist_mat <- as.matrix(dist(rbind(centers, newdata)))
  dist_mat <- dist_mat[-seq(n_centers), seq(n_centers)]
  max.col(-dist_mat)
}

# Crear las series de tiempo con la compra de productos

orders_aux <- orders[, c("order_id", "customer_id")]
order.items_aux <- order.items[, c("order_id",
                                   "shipping_limit_date",
                                   "price",
                                   "freight_value")]
order.items_aux <- mutate(order.items_aux, price=price-freight_value)
order.items_aux <- distinct(order.items_aux[, c("order_id",
                                                "shipping_limit_date",
                                                "price")])
orders_items <- merge(x=orders_aux, y=order.items_aux, by="order_id")
orders_items <- distinct(orders_items[, c("customer_id",
                                          "shipping_limit_date",
                                          "price")])
customers_aux <- distinct(customers[, c("customer_id",
                                        "customer_zip_code_prefix")])
orders_items_customers <- merge(x=orders_items,
                                y=customers_aux,
                                by="customer_id")
geolocation_aux <- distinct(geolocation[, c("geolocation_zip_code_prefix",
                                            "geolocation_lat",
                                            "geolocation_lng")])
geolocation_aux = geolocation_aux[!duplicated(geolocation_aux$geolocation_zip_code_prefix ),]

# Predecir el cluster de cada geolocalizacion
# Se hace asi por problemas de memoria
size <- dim(geolocation_aux)[1]

prediction1 <- predict(clusters, 
                       geolocation_aux[1:10000, c("geolocation_lat",
                                                  "geolocation_lng")])
prediction2 <- predict(clusters, 
                       geolocation_aux[10001:size, c("geolocation_lat",
                                                     "geolocation_lng") ])
prediction <- c(prediction1, prediction2)
geolocation_aux$cluster <- prediction
rm(prediction1)
rm(prediction2)
rm(prediction)
price_geolocation <- merge(x=orders_items_customers,
                           y=geolocation_aux,
                           by.x="customer_zip_code_prefix",
                           by.y="geolocation_zip_code_prefix")
price_geolocation <- mutate(price_geolocation, date=shipping_limit_date)
price_geolocation <- distinct(price_geolocation[, c("date",
                                                    "price",
                                                    "cluster")])
price_geolocation$date <- as.Date(price_geolocation$date,
                                  format = "%Y-%m-%d %H:%M:%S")
rm(orders_aux)
rm(order.items_aux)
rm(orders_items)
rm(customers_aux)
rm(orders_items_customers)
rm(geolocation_aux)

timeserie_cluster1 <- filter(price_geolocation, cluster==1)
timeserie_cluster2 <- filter(price_geolocation, cluster==2)
timeserie_cluster3 <- filter(price_geolocation, cluster==3)

rm(price_geolocation)

timeserie_cluster1 <- timeserie_cluster1[, c("date", "price")]
timeserie_cluster2 <- timeserie_cluster2[, c("date", "price")]
timeserie_cluster3 <- timeserie_cluster3[, c("date", "price")]

timeserie_cluster1 <- aggregate(timeserie_cluster1$price,
                                by=list(date=timeserie_cluster1$date),
                                FUN=sum)

timeserie_cluster2 <- aggregate(timeserie_cluster2$price,
                                by=list(date=timeserie_cluster2$date),
                                FUN=sum)

timeserie_cluster3 <- aggregate(timeserie_cluster3$price,
                                by=list(date=timeserie_cluster3$date),
                                FUN=sum)

timeserie_cluster1 <-timeserie_cluster1[order(timeserie_cluster1$date),]
timeserie_cluster2 <-timeserie_cluster2[order(timeserie_cluster2$date),]
timeserie_cluster3 <-timeserie_cluster3[order(timeserie_cluster3$date),]


# Hay una fecha atipica, pasa del 2018 al 2020
timeserie_cluster1 <- timeserie_cluster1[-nrow(timeserie_cluster1),]

# Gráficas de predicciones

pre1 <- read.csv("timeserie_cluster1.csv")
pre2 <- read.csv("timeserie_cluster2.csv")
pre3 <- read.csv("timeserie_cluster3.csv")

# Primer cluster

pre1 <- rename(pre1, Fecha=date)
pre1 <- mutate(pre1, Fecha = as.Date(Fecha, "%Y-%m-%d"))

# Generación del modelo lineal

m1 <- lm(x~Fecha, data = pre1)
summary(m1)

# Gráfica de la regresión

ggplot(pre1, aes(x = Fecha, y = x)) + 
  geom_point(size = 1, color = "#2AB0BF") +
  geom_smooth(method = lm, se = F, color = "#1F628C", size = 1) +
  labs( x = "Fecha", y = "No. compras") + 
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "gray"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "gray"),
    plot.background = element_rect(fill = "white")
  ) +
  ggtitle("Proyección de compras del siguiente año para el centro de distribución del cluster 1")

# Segundo cluster

pre2 <- rename(pre2, Fecha=date)
pre2 <- mutate(pre2, Fecha = as.Date(Fecha, "%Y-%m-%d"))
pre2 <- pre2[1:537,]

# Generación del modelo lineal

m2 <- lm(x~Fecha, data = pre2)
summary(m2)

# Gráfica de la regresión

ggplot(pre2, aes(x = Fecha, y = x)) + 
  geom_point(size = 1, color = "#2AB0BF") +
  geom_smooth(method = lm, se = F, color = "#1F628C", size = 1) +
  labs( x = "Fecha", y = "No. compras") + 
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "gray"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "gray"),
    plot.background = element_rect(fill = "white")
  ) +
  ggtitle("Proyección de compras del siguiente año para el centro de distribución del cluster 2")

# Tercer cluster

pre3 <- rename(pre3, Fecha=date)
pre3 <- mutate(pre3, Fecha = as.Date(Fecha, "%Y-%m-%d"))

# Generación del modelo lineal

m3 <- lm(x~Fecha, data = pre3)
summary(m3)

# Gráfica de la regresión

ggplot(pre3, aes(x = Fecha, y = x)) + 
  geom_point(size = 1, color = "#2AB0BF") +
  geom_smooth(method = lm, se = F, color = "#1F628C", size = 1) +
  labs( x = "Fecha", y = "No. compras") + 
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "gray"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "gray"),
    plot.background = element_rect(fill = "white")
  ) +
  ggtitle("Proyección de compras del siguiente año para el centro de distribución del cluster 3")
