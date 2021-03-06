# Postwork Sesion 5
# Regresi�n lineal y clasificaci�n

# 1. A partir del conjunto de datos de soccer de la liga espa�ola de las temporadas 2017/2018, 2018/2019 y 2019/2020, crea el data frame `SmallData`, que contenga las columnas `date`, `home.team`, `home.score`, `away.team` y `away.score`; esto lo puede hacer con ayuda de la funci�n `select` del paquete `dplyr`. Luego establece un directorio de trabajo y con ayuda de la funci�n `write.csv` guarda el data frame como un archivo csv con nombre *soccer.csv*. Puedes colocar como argumento `row.names = FALSE` en `write.csv`. 

#Se define directorio de trabajo en donde est�n los csv con la informaci�n de las tres temporadas

setwd("C:\\Users\\Oscar Salazar\\OneDrive\\Documents\\Data Analyst\\2_R y Py\\Postwork\\postwork2")

#Se leen todos los documentos dentro del directorio
SmallData <- lapply(dir(), read.csv)

library("dplyr")

#Filtrado de columnas con la funci�n select de dplyr
SmallData <- lapply(SmallData, select, Date, HomeTeam:FTAG)

#Formaci�n de un solo data frame
SmallData <- do.call(rbind, SmallData)

View(SmallData)

str(SmallData)

#Formato de fecha para la columna "date"
SmallData <- mutate(SmallData, Date = as.Date(Date, format="%d/%m/%y"))

#Se renombran las columnas para utilizar el paquete fbranks
SmallData <- rename(SmallData, date=Date, home.team=HomeTeam,
                    away.team=AwayTeam, home.score=FTHG, away.score=FTAG)

#Creaci�n del csv "soccer" para utilizarlo en fbranks
write.csv(SmallData, "soccer.csv", row.names = FALSE)


# 2. Con la funci�n `create.fbRanks.dataframes` del paquete `fbRanks` importe el archivo *soccer.csv* a `R` y al mismo tiempo asignelo a una variable llamada `listasoccer`. Se crear� una lista con los elementos `scores` y `teams` que son data frames listos para la funci�n `rank.teams`. Asigna estos data frames a variables llamadas `anotaciones` y `equipos`.

library("fbRanks")

#Se desarrolla una lista con distintos dataframes con la funci�n create.fb...
listasoccer <- create.fbRanks.dataframes("soccer.csv")

#Asignaci�n de dataframes a variables anotaciones y equipos
anotaciones <- listasoccer$scores
equipos <- listasoccer$teams

# 3. Con ayuda de la funci�n `unique` crea un vector de fechas (`fecha`) que no se repitan y que correspondan a las fechas en las que se jugaron partidos. Crea una variable llamada `n` que contenga el n�mero de fechas diferentes. Posteriormente, con la funci�n `rank.teams` y usando como argumentos los data frames `anotaciones` y `equipos`, crea un ranking de equipos usando unicamente datos desde la fecha inicial y hasta la pen�ltima fecha en la que se jugaron partidos, estas fechas las deber� especificar en `max.date` y `min.date`. Guarda los resultados con el nombre `ranking`.

#Lista de fechas (sin repetidos)
fecha <- unique(SmallData$date)

n <- length(fecha)

#se ordenan las fechas en orden cronol�gico
fecha <- sort(fecha)

#obtenci�n de fecha inicial y pen�ltima fecha
f.inicial <- fecha[1] 
f.penultima <- fecha[length(fecha)-1]

#Utilizaci�n de funci�n "rank.teams" para calificar a cada equipo
#se definieron fecha inicial y final
ranking <- rank.teams(anotaciones, equipos,
                      max.date = f.penultima,
                      min.date = f.inicial)

# 4. Finalmente estima las probabilidades de los eventos, el equipo de casa gana, el equipo visitante gana o el resultado es un empate para los partidos que se jugaron en la �ltima fecha del vector de fechas `fecha`. Esto lo puedes hacer con ayuda de la funci�n `predict` y usando como argumentos `ranking` y `fecha[n]` que deber� especificar en `date`.

#se obtienen los �ndices de las filas que contienen la �ltima fecha
partidos.uf <- which(anotaciones$date == fecha[length(fecha)])

#Extracci�n de los partidos realizados en la �ltima fecha
f.ultima <- anotaciones[partidos.uf[1]:partidos.uf[length(partidos.uf)],]

#Predicci�n de los resultados de los partidos de la �ltima fecha
Predicci�n <- predict.fbRanks(ranking, newdata = f.ultima)

#Los resultados generales(gana, pierde, empata) se acertaron en 66%
#En cuanto a los resultados particulares (goles), se acercan las predicciones a los resultados reales.