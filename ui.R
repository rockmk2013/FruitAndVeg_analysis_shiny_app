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
                   c("Regional distribution"=1,"water data"=2,"metal data"=3,"Rich period"=4,"TotalHeatMap"=5)
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
