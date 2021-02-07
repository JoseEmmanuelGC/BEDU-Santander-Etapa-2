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

# Distribucion de compradores
# ** Hacer bonita **
plot(table(customers["customer_state"]))


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
#Hay mayor variación con la latitud

mean.lat <- mean(data4clusters$latitud)

#Muestra para población infinita

se <- (abs(mean.lat)*0.01)/1.96
n.inf <- (sd.lat**2)/(se**2)

rows <- nrow(data4clusters)

#Corrección por muestra finita

n <- n.inf/(1+(n.inf/rows))
n.sample <- 105

#Proporciones
unique.states <- unique(dataset$state)


conteo <- dataset %>% group_by(state) %>% summarise(n = n())
conteo <- mutate(conteo, p = n/rows)


sample_by_state <- list()

set.seed(100)

for(i in seq(1,length(conteo$state))) {
  sample_by_state[[as.character(conteo[i,"state"])]] <- sample_n(data.by.states[[as.character(conteo[i,"state"])]],
                                                 size=ceiling(n*as.numeric(conteo[i,"p"])),
                                                  replace = FALSE)
}

dataset_sample <- do.call(rbind, sample_by_state)

dataset_sample <- dataset_sample[,c(2,3)]
rownames(dataset_sample)=NULL

nb_clusters <- fviz_nbclust(dataset_sample, kmeans, method = "silhouette")

prueba <- data4clusters[1:500,]

rownames(prueba) <- NULL

clusters <- kmeans(dataset_sample, 3, nstart = 1)

mat <- as.matrix(clusters$centers)
mat2 <- as.matrix(dataset_sample[1,])

fviz_cluster(clusters, dataset_sample, show.clust.cent = TRUE, geom = "point",
             stand=FALSE, ellipse = TRUE,
             ellipse.type = "convex",
             ellipse.level = 0.95,
             ellipse.alpha = 0.2,
             ggtheme = theme_classic())

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



