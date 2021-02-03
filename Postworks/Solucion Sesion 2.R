# Postwork Sesion 2
# Programación y manipulación de datos en R

# 1. Importa los datos de soccer de las temporadas 2017/2018, 2018/2019 y 2019/2020 de la primera división de la liga española a `R`, los datos los puedes encontrar en el siguiente enlace: https://www.football-data.co.uk/spainm.php
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

# 2. Obten una mejor idea de las características de los data frames al usar las funciones: `str`, `head`, `View` y `summary`
str(list[[1]])
head(list[[1]])
View(list[[1]])
summary(list[[1]])
dim(list[[1]])

# 3. Con la función `select` del paquete `dplyr` selecciona únicamente las columnas `Date`, `HomeTeam`, `AwayTeam`, `FTHG`, `FTAG` y `FTR`; esto para cada uno de los data frames. (Hint: también puedes usar `lapply`).
list <- lapply(list, select, Date, HomeTeam:FTR)

# 4. Asegúrate de que los elementos de las columnas correspondientes de los nuevos data frames sean del mismo tipo (Hint 1: usa `as.Date` y `mutate` para arreglar las fechas). Con ayuda de la función `rbind` forma un único data frame que contenga las seis columnas mencionadas en el punto 3 (Hint 2: la función `do.call` podría ser utilizada).
list <- lapply(list, mutate, Date=as.Date(Date, format='%d/%m/%y'))

data <- do.call(rbind, list)

head(data)
tail(data)
