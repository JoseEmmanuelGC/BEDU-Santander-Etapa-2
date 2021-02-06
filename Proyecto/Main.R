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

#Falta loop en esta parte
set.seed(100)

for(i in seq(1,length(conteo$state))) {
  sample_by_state[[as.character(conteo[i,"state"])]] <- sample_n(data.by.states[[as.character(conteo[i,"state"])]],
                                                 size=ceiling(n*as.numeric(conteo[i,"p"])),
                                                  replace = FALSE)
  #customer.by.states[[state]] <- filter(customers.unique, customer_state==state)
  #data.by.states[[state]] <- merge(x=geolocation.by.states[[state]], 
                                   #y=customer.by.states[[state]], 
                                   #by.x ="geolocation_zip_code_prefix", 
                                   #by.y = "customer_zip_code_prefix")
  #print(conteo[i,"state"])
}

dataset_sample <- do.call(rbind, sample_by_state)

dataset_sample <- dataset_sample[,c(2,3)]

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
