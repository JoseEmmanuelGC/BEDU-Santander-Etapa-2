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


clusters <- kmeans(dataset_sample, 3, nstart = 1)

centroides <- clusters$centers

composicion <- as.data.frame(clusters$cluster)

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
         title = 'Propuesta: Ubicación de Centros de distribución')


fviz_cluster(clusters, dataset_sample, show.clust.cent = TRUE, geom = "point",
             stand=FALSE, ellipse = TRUE,
             ellipse.type = "convex",
             ellipse.level = 0.95,
             ellipse.alpha = 0.2,
             ggtheme = theme_classic())
