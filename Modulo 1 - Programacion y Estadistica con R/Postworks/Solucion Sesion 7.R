# Solucion Sesion 7
# RStudio Cloud - Github, conexiones con BDs y lectura de datos externos

#install.packages("mongolite")

library(mongolite)

db <- mongo(collection= "match", db = "match_games", url = "mongodb+srv://beduUser:12345@cluster0.sg9cb.mongodb.net")

data <- read.csv('Postworks/sesion7_data/data.csv')
df <- data.frame(data)

#Alojar el fichero data.csv en una base de datos llamada match_games, nombrando al collection como match
db$insert(df)

#Una vez hecho esto, realizar un count para conocer el número de registros que se tiene en la base
db$count('{}')

#Realiza una consulta utilizando la sintaxis de Mongodb, en la base de datos para conocer el número de goles que metió el Real Madrid el 20 de diciembre de 2015 y contra qué equipo jugó, ¿perdió ó fue goleada?
result <- db$aggregate(
  '[
    {
      "$match": {
        "$or": [{ "HomeTeam": "Real Madrid"}, {"AwayTeam": "Real Madrid"}],
        "Date": "20/12/2015"
      }
    },
    {
      "$addFields": {
        "Goles del Real Madrid": {
          "$cond": {
            "if": {
              "$eq": [
                "$HomeTeam", "Real Madrid"
              ]
            },
            "then": "$FTHG",
            "else": "$FTAG"
          }
        },
        "Equipo contrario": {
          "$cond": {
            "if": {
              "$eq": [
                "$HomeTeam", "Real Madrid"
              ]
            },
            "then": "$AwayTeam",
            "else": "$HomeTeam"
          }
        },
        "Resultado": {
          "$cond": {
            "if": {
              "$eq": [
                "$HomeTeam", "Real Madrid"
              ]
            },
            "then": {
              "$cond": {
                "if": {
                  "$gt": [
                    "$FTHG", "$FTAG"
                  ]
                },
                "then": "Real Madrid Gana",
                "else": {
                  "$cond": {
                    "if": {
                      "$eq": [
                        "$FTHG", "$FTAG"
                      ]
                    },
                    "then": "Empate",
                    "else": "Real Madrid Pierde"
                  }
                }
              }
            },
            "else": {
              "$cond": {
                "if": {
                  "$gt": [
                    "$FTAG", "$FTHG"
                  ]
                },
                "then": "Real Madrid Gana",
                "else": {
                  "$cond": {
                    "if": {
                      "$eq": [
                        "$FTAG", "$FTHG"
                      ]
                    },
                    "then": "Empate",
                    "else": "Real Madrid Pierde"
                  }
                }
              }
            }
          }
        }
      }
    },
    {
      "$project": {
        "_id": false,
        "Goles del Real Madrid": true,
        "Equipo contrario": true,
        "Resultado": true
      }
    }
  ]'
)

print(result)

db$disconnect()

#Agrega el dataset de mtcars a la misma BDD
db <- mongo(collection= "mtcars", db = "mt_cars", url = "mongodb+srv://beduUser:12345@cluster0.sg9cb.mongodb.net")
df <- data.frame(mtcars)
db$insert(df)
db$count('{}')

#Por último, no olvides cerrar la conexión con la BDD.
db$disconnect()

