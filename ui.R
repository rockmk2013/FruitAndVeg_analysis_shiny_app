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
                   c("Regional Distribution"=1,"Water Data"=2,"Metal Data"=3,"Rich Period"=4,"TotalHeatMap(produce)"=5,"Pollution HeatMap"=6)
      )
    ),  
    mainPanel(tabsetPanel(
      tabPanel("Visualization",
               #Wrapping plots in a fluidRow provides easy control over individual plot attributes
               fluidRow(
                 column(width=12,height=150,plotOutput("plot1")),
                 column(width=12,height=150,plotOutput("plot2")),
                 column(width=12,height=150,plotOutput("plot3"))
               ) 
      )
    ))
  )  
)
