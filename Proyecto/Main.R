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

customers.unique <- distinct(customers[c("customer_zip_code_prefix", "customer_city", "customer_state")])

geolocation.by.states <- list()
customer.by.states <- list()
data.by.states <- list()

for(state in as.vector(states[,1])) {
  geolocation.by.states[[state]] <- filter(geolocation, geolocation_state==state)
  customer.by.states[[state]] <- filter(customers.unique, customer_state==state)
  data.by.states[[state]] <- merge(x=geolocation.by.states[[state]], 
                          y=customer.by.states[[state]], 
                          by.x ="geolocation_zip_code_prefix", 
                          by.y = "customer_zip_code_prefix")
}





