library(readr)
library(shiny)
library(dplyr) #for data manipulation
library(tidyr) #for reshaping data
library(ggplot2)
library(scales) #for labels as percentage
library(RCurl)#for getting url content
library(jpeg) #for reading JPEG file
library(grid) #for rendering a raster grob


#read the csv files, make sure it is in your working directory
dataset <-  unique(read_csv("veg_fruit.csv"))

#蔬菜
vet = dataset[dataset$type=="蔬菜",]
#水果
fruit = dataset[dataset$type=="水果",]
#受水汙染
water_danger = dataset[dataset$waterDanger == 1,]
#重金屬汙染
metal_danger = dataset[dataset$metalDanger == "鎘超標",]


#create a blank theme for each plot
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=12, face="bold")
  )

function(input, output){
  
  output$plot1 <- renderPlot({
    if(input$plot==1){
      subset = filter(dataset,crop%in%input$Fruit)
      x = barplot(sort(table(subset$county)),horiz = TRUE,las=1,col = "#0080FF",main = "區域分布")
    }
    if(input$plot==2){
      subset = filter(dataset,crop%in%input$Fruit)
      names=c("無汙染","受汙染")
      if(is.na(names(table(subset$waterDanger))[2])){
        x = barplot(sort(table(subset$waterDanger)),las=1,names.arg=c("無汙染"),col = "#0080FF",main = "水汙染情況")
      }else{
        x = barplot(sort(table(subset$waterDanger)),las=1,names.arg=names,col = "#0080FF",main = "水汙染情況")
      }
    }
    if(input$plot==3){
      subset = filter(dataset,crop%in%input$Fruit)
      names=c("無汙染","受汙染")
      if(is.na(names(table(subset$metalDanger))[2])){
        x = barplot(sort(table(subset$metalDanger)),las=1,names.arg=c("無汙染"),col = "#0080FF",main = "重金屬汙染情況")
      }else{
        x = barplot(sort(table(subset$metalDanger)),las=1,names.arg=names,col = "#0080FF",main = "重金屬汙染情況")
      }
      }
    if(input$plot==4){
      subset = filter(dataset,crop%in%input$Fruit)
      barplot(sort(table(subset$month),decreasing = TRUE),horiz=TRUE,las=1,col = "#0080FF",main = "盛產月份")
    }  
    if(input$plot==5){
      attach(water_danger)
      barplot(sort(table(crop)),horiz = TRUE,las=1,col = "#0080FF",main = "綜合－水汙染之蔬果")
    }
    if(input$plot==6){
      attach(metal_danger)
      barplot(sort(table(crop)),horiz = TRUE,las=1,col = "#0080FF",main = "綜合－重金屬汙染之蔬果")
    }
    if(input$plot==7){
      attach(dataset)
      count = data.frame(table(crop,county))
      country = count$county
      crop = count$crop
      Freq = count$Freq
      p <- ggplot(count, aes(country,crop )) + geom_tile(aes(fill =  Freq), colour = "white") + scale_fill_gradient(low = "yellow", high = "red")+labs(title="綜合－蔬果HeatMap")+theme(plot.title = element_text(size=20,face = "bold"))
      print(p)
    }  
    if(input$plot==8){
      attach(water_danger)
      count = data.frame(table(crop,county))
      country = count$county
      crop = count$crop
      Freq = count$Freq
      p <- ggplot(count, aes(country,crop )) + geom_tile(aes(fill =  Freq), colour = "white") + scale_fill_gradient(low = "yellow", high = "red")+labs(title="綜合－水汙染HeatMap")+theme(plot.title = element_text(size=20,face = "bold"))
      print(p)
    }
  })
  output$plot2 <- renderPlot({
    if(input$plot==1){
      
    }
    if(input$plot==2){
      subset = filter(dataset,crop%in%input$Fruit)
      
      if(is.na(names(table(subset$waterDanger))[2])  ){
      }else {
         barplot(sort(table(subset[subset$waterDanger==1,]$county)),las=1,col = "#0080FF",main = "水汙染之地區")
      }
      
    }  
    if(input$plot==3){
      subset = filter(dataset,crop%in%input$Fruit)
      
      if(is.na(names(table(subset$metalDanger))[2])  ){
      }else {
        barplot(sort(table(subset[subset$metalDanger=="鎘超標",]$county)),las=1,col = "#0080FF",main = "重金屬汙染之地區")
      }
      
    }
    if(input$plot==4){
      subset = filter(dataset,crop%in%input$Fruit)
      if(is.na(names(table(subset$waterDanger))[2]) & is.na(names(table(subset$metalDanger))[2])){
        
      }else if(is.na(names(table(subset$metalDanger))[2])){
        subset = filter(water_danger,crop%in%input$Fruit)
        x = barplot(sort(table(subset$month)),las=1,col = "#0080FF",main = "水汙染之月份")
      }else if(is.na(names(table(subset$waterDanger))[2])){
        subset = filter(metal_danger,crop%in%input$Fruit)
        x = barplot(sort(table(subset$month)),las=1,col = "#0080FF",main = "重金屬汙染之月份")
      }else{
        subset = filter(water_danger,crop%in%input$Fruit)
        x = barplot(sort(table(subset$month)),las=1,col = "#0080FF",main = "水汙染之月份")
      }
    }
    
    if(input$plot==5){
      attach(water_danger)
      barplot(sort(table(county)),horiz = TRUE,las=1,col = "#0080FF",main = "綜合－水汙染之地區")
    }
    if(input$plot==6){
      attach(metal_danger)
      barplot(sort(table(county)),horiz = TRUE,las=1,col = "#0080FF",main = "綜合－重金屬汙染之地區")
    }
    if(input$plot==7){
      fruitcount = data.frame(table(fruit$crop,fruit$county))
      country = fruitcount$Var2
      crop = fruitcount$Var1
      Freq = fruitcount$Freq
      x <- ggplot(fruitcount, aes(country,crop )) + geom_tile(aes(fill =  Freq), colour = "white") + scale_fill_gradient(low = "yellow", high = "red")+labs(title="綜合－水果HeatMap")+theme(plot.title = element_text(size=20,face = "bold"))
      print(x)
    }
    if(input$plot==8){
      attach(metal_danger)
      count = data.frame(table(crop,county))
      country = count$county
      crop = count$crop
      Freq = count$Freq
      p <- ggplot(count, aes(country,crop )) + geom_tile(aes(fill =  Freq), colour = "white") + scale_fill_gradient(low = "yellow", high = "red")+labs(title="綜合－重金屬汙染HeatMap")+theme(plot.title = element_text(size=20,face = "bold"))
      print(p)
    }
  })
  output$plot3 <- renderPlot({
    if(input$plot==4){
      subset = filter(dataset,crop%in%input$Fruit)
      if(is.na(names(table(subset$waterDanger))[2]) & is.na(names(table(subset$metalDanger))[2])){

      }else if(is.na(names(table(subset$metalDanger))[2])){

      }else if(is.na(names(table(subset$waterDanger))[2])){

      }else{
        subset = filter(metal_danger,crop%in%input$Fruit)
        x = barplot(sort(table(subset$month)),las=1,col = "#0080FF",main = "重金屬汙染之月份")
      }
    }
    if(input$plot==5){
      attach(water_danger)
      barplot(sort(table(month)),horiz = TRUE,las=1,col = "#0080FF",main = "綜合－水汙染之月份")
    }
    if(input$plot==6){
      attach(metal_danger)
      barplot(sort(table(month)),horiz = TRUE,las=1,col = "#0080FF",main = "綜合－重金屬汙染之月份")
    }
    if(input$plot == 7){
      
      vetcount = data.frame(table(vet$crop,vet$county))
      country = vetcount$Var2
      crop = vetcount$Var1
      Freq = vetcount$Freq
      s <- ggplot(vetcount, aes(country,crop)) + geom_tile(aes(fill =  Freq), colour = "white") + scale_fill_gradient(low = "yellow", high = "red")+labs(title="綜合－蔬菜HeatMap")+theme(plot.title = element_text(size=20,face = "bold"))
      print(s)
    }
  })
}