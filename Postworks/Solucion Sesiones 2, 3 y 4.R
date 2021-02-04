# Postwork Sesiones 2, 3 y 4
# Programaci贸n y manipulaci贸n de datos en R

#---------------------- SESION 2 -------------------------------

library(dplyr)

# 1. Importa los datos de soccer de las temporadas 2017/2018, 2018/2019 y 2019/2020 de la primera divisi贸n de la liga espa帽ola a `R`, los datos los puedes encontrar en el siguiente enlace: https://www.football-data.co.uk/spainm.php
URLs <- c('https://www.football-data.co.uk/mmz4281/1718/D1.csv',
          'https://www.football-data.co.uk/mmz4281/1819/D1.csv',
          'https://www.football-data.co.uk/mmz4281/1920/D1.csv')

names <- c('2017.csv', '2018.csv', '2019.csv')

for(i in 1:3) {
  download.file(url = URLs[i], destfile = names[i], mode = "wb")
}

dir()
# Modificar el rango segun los archivos que esten en el directorio
list <- lapply(dir()[1:3], read.csv)

# 2. Obten una mejor idea de las caracter铆sticas de los data frames al usar las funciones: `str`, `head`, `View` y `summary`
str(list[[1]])
head(list[[1]])
View(list[[1]])
summary(list[[1]])
dim(list[[1]])

# 3. Con la funci贸n `select` del paquete `dplyr` selecciona 煤nicamente las columnas `Date`, `HomeTeam`, `AwayTeam`, `FTHG`, `FTAG` y `FTR`; esto para cada uno de los data frames. (Hint: tambi茅n puedes usar `lapply`).
list <- lapply(list, select, Date, HomeTeam:FTR)

# 4. Aseg煤rate de que los elementos de las columnas correspondientes de los nuevos data frames sean del mismo tipo (Hint 1: usa `as.Date` y `mutate` para arreglar las fechas). Con ayuda de la funci贸n `rbind` forma un 煤nico data frame que contenga las seis columnas mencionadas en el punto 3 (Hint 2: la funci贸n `do.call` podr铆a ser utilizada).
list <- lapply(list, mutate, Date=as.Date(Date, format='%d/%m/%y'))

ligaesp <- do.call(rbind, list)

head(ligaesp)
tail(ligaesp)

#---------------------- SESION 3 -------------------------------

# 1. Con el 煤ltimo data frame obtenido en el postwork de la sesi贸n 2, elabora tablas de frecuencias relativas para estimar las siguientes probabilidades:
#
# - La probabilidad (marginal) de que el equipo que juega en casa anote x goles (x=0,1,2,)
rows <- dim(ligaesp)[1]

FTHG <- ligaesp$FTHG
FTHG_MarginalProbability <- as.data.frame(table(FTHG) / rows)
FTHG_MarginalProbability <- rename(FTHG_MarginalProbability, ProbH=Freq)

# - La probabilidad (marginal) de que el equipo que juega como visitante anote y goles (y=0,1,2,)
FTAG <- ligaesp$FTAG
FTAG_MarginalProbability <- as.data.frame(table(FTAG) / rows)
FTAG_MarginalProbability <- rename(FTAG_MarginalProbability, ProbA=Freq)

# - La probabilidad (conjunta) de que el equipo que juega en casa anote x goles y el equipo que juega como visitante anote y goles (x=0,1,2,, y=0,1,2,)
JointProbability <- as.data.frame(table(FTHG, FTAG) / rows)
JointProbability <-  rename(JointProbability, ProbJoint=Freq)

# 2. Realiza lo siguiente:
#   - Un gr醘ico de barras para las probabilidades marginales estimadas del n鷐ero de goles que anota el equipo de casa
library("ggplot2")

(barras.home <- ggplot(FTHG_MarginalProbability, aes(x=FTHG, y=ProbH)) 
                + geom_bar(stat="identity")
                + theme_classic()
                + xlab("N煤mero de goles")
                + ylab("Probabilidad")
                + labs(title = "Probabilidad de gol como local"))

# - Un gr醘ico de barras para las probabilidades marginales estimadas del n鷐ero de goles que anota el equipo visitante.
(barras.away <- ggplot(FTAG_MarginalProbability, aes(x=FTAG, y=ProbA)) 
                + geom_bar(stat="identity")
                + theme_classic()
                + xlab("N煤mero de goles")
                + ylab("Probabilidad")
                + labs(title = "Probabilidad de gol como visitante"))

#  - Un HeatMap para las probabilidades conjuntas estimadas de los n鷐eros de goles que anotan el equipo de casa y el equipo visitante en un partido.
(prob.goles <- ggplot(JointProbability, aes(x = FTHG, y=FTAG, fill = ProbJoint))
                + geom_tile()
                + xlab("Goles local")
                + ylab("Goles visitante")
                +scale_fill_gradient(name = "Probabilidad",
                      low = "#FFFFFF",
                      high = "#012345"))

#---------------------- SESI脫N 4 -------------------------------

#Ahora investigar醩 la dependencia o independencia del n鷐ero de goles anotados por el equipo de casa y el n鷐ero de goles anotados por el equipo visitante mediante un procedimiento denominado bootstrap, revisa bibliograf韆 en internet para que tengas nociones de este desarrollo.

# 1. Ya hemos estimado las probabilidades conjuntas de que el equipo de casa anote X=x goles (x=0,1,... ,8), y el equipo visitante anote Y=y goles (y=0,1,... ,6), en un partido. Obt閚 una tabla de cocientes al dividir estas probabilidades conjuntas sobre el producto de las probabilidades marginales correspondientes.
library("tidyr")

cross_probabilities <- crossing(FTAG_MarginalProbability, FTHG_MarginalProbability)


cross_probabilities <- mutate(cross_probabilities, Probcross = ProbH*ProbA)

probJoint <- as.data.frame(JointProbability$ProbJoint)

cross_probabilities <- cbind(cross_probabilities, probJoint)

cross_probabilities <- mutate(cross_probabilities, ratio=JointProbability$ProbJoint/Probcross)

#Mediante un procedimiento de boostrap, obt閚 m醩 cocientes similares a los obtenidos en la tabla del punto anterior. Esto para tener una idea de las distribuciones de la cual vienen los cocientes en la tabla anterior. Menciona en cu醠es casos le parece razonable suponer que los cocientes de la tabla en el punto 1, son iguales a 1 (en tal caso tendr韆mos independencia de las variables aleatorias X y Y).

cociente <- function(data, indices){
  d <- data[indices,]
  cocientes <- mean(d$ratio)
  return(cocientes)
}

data <- cross_probabilities

library(boot)
set.seed(100)
results <- boot(data, cociente, R=1000)

plot(results)

#El histograma resultante nos permite ver que los cocientes tienen una distribuci髇 normal

evaluacion_cocientes <- select(cross_probabilities, FTAG, FTHG, ratio)
evaluacion_cocientes <- filter(evaluacion_cocientes, ratio<=1.05, ratio>=0.95)

#Los coeficientes m醩 cercanos a uno (evaluamos error +/- 5%), son partidos que muestran resultados cerrados
#Se podr韆 suponer que se tienen coeficientes alrededor de uno cuando aparecen cuando se enfrentan equipos con nivel similar, en donde no se esperar marcadores con gran diferencia.

