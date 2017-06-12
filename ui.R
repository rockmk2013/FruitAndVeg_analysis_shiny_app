library(readr)
library(shiny)
#dataset <-  read.csv("dataset.csv")
dataset <-  unique(read_csv("veg_fruit.csv"))
fluidPage(
  titlePanel("Food visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("Fruit", "Choose a Fruit/Vegetable:",
                  names(table(dataset$crop))
      ),
      radioButtons("plot","Which fruit/vegetable data do you want to know?",
                   c("Regional Distribution"=1,"Water Data"=2,"Metal Data"=3,"Rich Period"=4,"TotalWaterDanger"=5,"TotalMetalDanger"=6,"TotalHeatMap(produce)"=7,"Pollution HeatMap"=8)
      )
    ),  
    mainPanel(tabsetPanel(
      tabPanel("Visualization",
               #Wrapping plots in a fluidRow provides easy control over individual plot attributes
               fluidRow(
                 column(width=12,height=200,plotOutput("plot1")),
                 column(width=12,height=200,plotOutput("plot2")),
                 column(width=12,height=200,plotOutput("plot3"))
               ) 
      )
    ))
  )  
)
