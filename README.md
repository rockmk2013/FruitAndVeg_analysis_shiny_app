# FruitAndVeg_analysis_shiny_app
This project shows the pollution situation of Fruit and Vegetable in Taiwan, following are the data I used.

 1.Groundwater data (2016)
 
 2.Testing station data
 
 3.government control water data(2016)
 
 4.fruit and vegetable production data(2016)(per month)
# Clean data
I combined the same column appears in these data, I used the district column in table 1 and 2, then combined this data with table 3(used country column). 
Then I combined the table above with table 4, finishing our using data.
# Shiny App 
I used shiny app to show the pollution situation of Fruit and Vegetable in Taiwan, you can run this project by running the code below, thanks for your reading.

library(shiny)

runGitHub("rockmk2013/FruitAndVeg_analysis_shiny_app")
