# Sesión 6: Series de tiempo
# Series de tiempo

# Importa el conjunto de datos match.data.csv a R y realiza lo siguiente:

# 1. Agrega una nueva columna sumagoles que contenga la suma de goles por partido.

# Primero se llama a la librería dplyr, la cual permite tomar a 
# un dataframe y editar sus campos. 

library(dplyr)

# Se procede leyendo los datos del archivo match.data.csv que contiene la
# información de los partidos de la liga española desde el 2010 hasta el 
# 2020.

data <- read.csv("match.data.csv")

# Por medio del comando mutate, de la librería dplyr, se agrega el campo
# sua goles que representa los goles anotados por cada partido.

new_data <- mutate(data, sumagoles = home.score + away.score)

head(new_data)

# 2. Obtén el promedio por mes de la suma de goles.

# De forma similar se cambia el nombre del campo date, que contiene la 
# fecha del partido y se cambia su tipo de dato a POSIXlt para poder
# agrupar los partidos por mes.

fdat <- rename(new_data, Fecha = date)

fdat <- mutate(fdat, Fecha = as.POSIXlt(Fecha, "%Y-%m-%d"))

# Se crea un nuevo dataframe con la fecha del partido y la suma de goles
# anotados.

lg <- select(fdat, Fecha, sumagoles)

# Finalmente, se divide a este dataframe por los meses de cada año y se 
# calcula el promedio de la suma de goles por mes.  

tsg <- rbind(mean(lg[lg$Fecha$year=="110" & lg$Fecha$mon==7,2]),
             mean(lg[lg$Fecha$year=="110" & lg$Fecha$mon==8,2]),
             mean(lg[lg$Fecha$year=="110" & lg$Fecha$mon==9,2]),
             mean(lg[lg$Fecha$year=="110" & lg$Fecha$mon==10,2]),
             mean(lg[lg$Fecha$year=="110" & lg$Fecha$mon==11,2]))

for (i in 1:10) {
    
    for (j in 0:11) {
        
        if(i>=1 & i<=9){
            
            y <- paste("11",as.character(i),sep="")
            tsg <- rbind(tsg, mean(lg[lg$Fecha$year==y & lg$Fecha$mon==j,2]))
            
        }else if(i==10 & j<7){
            
            tsg <- rbind(tsg, mean(lg[lg$Fecha$year=="120" & lg$Fecha$mon==j,2]))
            
        }
        
    }
    
}

# Como hay meses en los que no se jugó ningún partido, el promedio 
# que devuelven es NA, por lo que se cambian estos valores por 0.

pm <- replace(tsg, is.na(tsg), 0)

# 3. Crea la serie de tiempo del promedio por mes de la suma de goles hasta diciembre de 2019.

# Ya con el vector de promedios por mes, se crea a la serie de tiempo por
# medio del comando ts(), especificando su inicio y término.

gm <- ts(pm, start=c(2010,8), end=c(2020, 7), fr=12)

# 4. Grafica la serie de tiempo.

# Como último paso se grafica la serie de tiempo obtenida.

plot(gm, xlab = "Meses", ylab = "Suma de goles", main = "Suma de goles por mes",
     sub = "Serie mensual: agosto de 2010 a julio 2020")
