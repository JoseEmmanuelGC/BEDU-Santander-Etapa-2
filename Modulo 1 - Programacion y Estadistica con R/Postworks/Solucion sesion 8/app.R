library(shiny)
library(shinydashboard)
library(shinythemes)

mdata <- read.csv("match.data.csv")

#Esta parte es el análogo al ui.R
ui <- 
    
    fluidPage(
        
        dashboardPage( skin = "red",
                       
                       dashboardHeader(title = "Postwork 8"),
                       
                       dashboardSidebar(
                           
                           sidebarMenu(
                               menuItem("Gráficas de barras", tabName = "Dashboard", icon = icon("dashboard")),
                               menuItem("Postwork 3", tabName = "graph", icon = icon("area-chart")),
                               menuItem("Data Table", tabName = "data_table", icon = icon("table")),
                               menuItem("Imágenes factores de ganancia", tabName = "img", icon = icon("file-picture-o"))
                           )
                           
                       ),
                       
                       dashboardBody(
                           
                           tabItems(
                               
                               # Gráfica de barras
                               tabItem(tabName = "Dashboard",
                                       fluidRow(
                                           titlePanel("Goles de local y visitante"), 
                                           selectInput("x", "Seleccione al equipo",
                                                       choices = unique(mdata$home.team)),
                                           
                                           # selectInput("zz", "Selecciona la variable del grid", 
                                           #             
                                           #             choices = c("cyl", "vs", "gear", "carb")),
                                           box(plotOutput("plot1", height = 350)),
                                           
                                           # box(
                                           #     title = "Controls",
                                           #     sliderInput("bins", "Number of observations:", 1, 30, 15)
                                           # )
                                       )
                               ),
                               
                               # Postwork 3
                               tabItem(tabName = "graph", 
                                       fluidRow(
                                           titlePanel(h3("Gráficas postwork No. 3")),
                                           # selectInput("a", "Selecciona la gráfica a visualizar",
                                           #             choices = c("Histograma de goles locales", "Histograma de goles de visitantes","Dispersión de probabilidad de goles")),
                                           img( src = "P3_l.png", 
                                                    height = 440, width = 336),
                                           img( src = "P3_v.png", 
                                                height = 440, width = 336),
                                           img( src = "P3_P.png", 
                                                height = 440, width = 336),
                                       )
                               ),
                               
                               
                               # Data table
                               tabItem(tabName = "data_table",
                                       fluidRow(        
                                           titlePanel(h3("Data Table")),
                                           dataTableOutput ("data_table")
                                       )
                               ), 
                               
                               # Factores de ganancia
                               tabItem(tabName = "img",
                                       fluidRow(
                                           titlePanel(h3("Gráficas de los factores de ganacia mínimo y máximo")),
                                           img( src = "g2.jpeg", 
                                                height = 544, width = 840),
                                           img( src = "g1.jpeg", 
                                                height = 544, width = 840),
                                       )
                               )
                               
                           )
                       )
        )
    )

#De aquí en adelante es la parte que corresponde al server

server <- function(input, output) {
    library(ggplot2)
    library(reshape2)
    
    #Gráfico de Barras
    output$plot1 <- renderPlot({
        
        gl <- mdata[mdata$home.team == input$x, 3]
        gv <- mdata[mdata$away.team == input$x, 5]
        g <- data.frame("Local" = gl, "Visitante" = gv)
        ng <- melt(g)
        
        ggplot(ng, aes( x = variable, y = value)) + 
            geom_bar(stat = "identity", position = "stack") +
            labs( x = "Posición", y = "Goles") + 
            theme_light() 
        
    })
    
    #Data Table
    output$data_table <- renderDataTable( {mdata}, 
                                          options = list(aLengthMenu = c(5,25,50),
                                                         iDisplayLength = 5)
    )
    
}


shinyApp(ui, server)
