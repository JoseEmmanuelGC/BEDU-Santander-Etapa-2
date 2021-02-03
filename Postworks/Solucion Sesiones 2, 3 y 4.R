# Postwork Sesion 2
# Programaci贸n y manipulaci贸n de datos en R

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

#---------------------- SESI脫N 3 -------------------------------

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

setwd("C:\\Users\\Oscar Salazar\\OneDrive\\Documents\\Data Analyst\\2_R y Py\\Postwork\\postwork2")

SmallData <- lapply(dir(), read.csv)

ligaesp <- lapply(ligaesp, select, Date, HomeTeam:FTR)

ligaesp <- do.call(rbind, ligaesp)

ligaesp <- mutate(ligaesp, Date = as.Date(Date, "%d/%m/%y"))