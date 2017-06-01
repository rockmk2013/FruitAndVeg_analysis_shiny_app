library(shiny)

fluidPage(
  titlePanel("Food visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("Fruit", "Choose a Fruit/Vegetable:",
                  crop_name
      ),
      radioButtons("plot","Which fruit/vegetable data do you want to know?",
                   c("Regional distribution"=1,"water data"=2,"metal data"=3,"Rich period"=4,"TotalHeatMap"=5)
      )
    ),  
    mainPanel(tabsetPanel(
      tabPanel("Visualization",
               #Wrapping plots in a fluidRow provides easy control over individual plot attributes
               fluidRow(
                 column(width=10,height=30,plotOutput("plot1")),
                 column(width=10,height=30,plotOutput("plot2")),
                 column(width=10,height=30,plotOutput("plot3"))
               ) 
      )
    ))
  )  
)
