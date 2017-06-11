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
      x = barplot(sort(table(subset$county)),horiz = TRUE,las=1,col = "#0080FF")
    }
    if(input$plot==2){
      subset = filter(dataset,crop%in%input$Fruit)
      names=c("無汙染","受汙染")
      if(is.na(names(table(subset$waterDanger))[2])){
        x = barplot(sort(table(subset$waterDanger)),las=1,names.arg=c("無汙染"),col = "#0080FF")
      }else{
        x = barplot(sort(table(subset$waterDanger)),las=1,names.arg=names,col = "#0080FF")
      }
    }
    if(input$plot==3){
      subset = filter(dataset,crop%in%input$Fruit)
      names=c("無汙染","受汙染")
      if(is.na(names(table(subset$metalDanger))[2])){
        x = barplot(sort(table(subset$metalDanger)),las=1,names.arg=c("無汙染"),col = "#0080FF")
      }else{
        x = barplot(sort(table(subset$metalDanger)),las=1,names.arg=names,col = "#0080FF")
      }
      }
    if(input$plot==4){
      subset = filter(dataset,crop%in%input$Fruit)
      barplot(sort(table(subset$month),decreasing = TRUE),horiz=TRUE,las=1,col = "#0080FF")
    }  
    if(input$plot==5){
      attach(dataset)
      count = data.frame(table(crop,county))
      country = count$county
      crop = count$crop
      p <- ggplot(count, aes(country,crop )) + geom_tile(aes(fill =  count$Freq), colour = "white") + scale_fill_gradient(low = "yellow", high = "red")
      print(p)
    }
    if(input$plot==6){
      attach(water_danger)
      count = data.frame(table(crop,county))
      country = count$county
      crop = count$crop
      p <- ggplot(count, aes(country,crop )) + geom_tile(aes(fill =  count$Freq), colour = "white") + scale_fill_gradient(low = "yellow", high = "red")
      print(p)
    }
    if(input$plot==7){
      #plot the FG% and number of shot attemps by each shooting region for average of the league
      courtimgurl<-"https://thedatagame.files.wordpress.com/2016/03/nba_court.jpg"
      court <- rasterGrob(readJPEG(getURLContent(courtimgurl)),width=unit(1,"npc"), height=unit(1,"npc"))
      region<-ggplot(averageleague, aes(x=LOCX, y=LOCY)) + 
        annotation_custom(court, -250, 250, -50, 420) +
        geom_point(aes(size=FGA),shape=19,color="grey",alpha=0.8)+
        geom_text(aes(label=FG_PCT),vjust=-0.9,size=3,color="orange")+
        xlim(-250, 250)+
        ylim(-50, 420)+
        ggtitle("Overall league FG% by region(size by shots attempted)")+
        blank_theme+
        theme(axis.text.x = element_blank(), axis.text.y = element_blank(),legend.position="bottom")
      print(region)
    }  
  })
  output$plot2 <- renderPlot({
    if(input$plot==1){
      
    }
    if(input$plot==2){
      subset = filter(dataset,crop%in%input$Fruit)
      
      if(is.na(names(table(subset$waterDanger))[2])  ){
      }else {
         barplot(sort(table(subset[subset$waterDanger==1,]$county)),las=1,col = "#0080FF")
      }
      # #create a line plot for 2 point FG% by shotclock time left
      # subset<-filter(shotdashboard,PLAYER_NAME%in%input$player&CLASS%in%"ShotClockShooting")
      # subset<-subset[1:6,]
      # timeleft<-subset$SHOT_TYPE
      # FG3<-subset$FG3_PCT
      # #reorder the "timeleft" variable
      # timeleft<-factor(timeleft,levels = c("22-24","18-22","15-18","07-15","04-07","00-04"))
      # line<-ggplot(subset,aes(x=timeleft,y=FG3))+
      #   geom_point(color="#619CFF",size=3)+
      #   geom_line(aes(group=1),color="#619CFF",size=1)+
      #   ggtitle("3 points FG% by seconds left on the shotclock")+
      #   scale_y_continuous(labels=percent,limits = c(0,0.75))+
      #   blank_theme
      # print(line)
    }  
    if(input$plot==3){
      subset = filter(dataset,crop%in%input$Fruit)
      
      if(is.na(names(table(subset$metalDanger))[2])  ){
      }else {
        barplot(sort(table(subset[subset$metalDanger=="鎘超標",]$county)),las=1,col = "#0080FF")
      }
      #create a line plot for FG% by the range from the closest defender(shot>10 feet)
      # subset<-filter(shotdashboard,PLAYER_NAME%in%input$player&CLASS%in%"ClosestDefender10ftPlusShooting")
      # distance<-subset$SHOT_TYPE
      # FG<-subset$FG_PCT
      # line<-ggplot(subset,aes(x=distance,y=FG))+
      #   geom_point(color="#00BFC4",size=3)+
      #   geom_line(aes(group=1),color="#00BFC4",size=1)+
      #   ggtitle("FG% by feets away from the closest defender(Shot>10 feet)")+
      #   scale_y_continuous(labels=percent,limits = c(0,0.75))+
      #   blank_theme
      # print(line)
    }
    if(input$plot==4){
      
    }
    if(input$plot==5){
     
      fruitcount = data.frame(table(fruit$crop,fruit$county))
      country = fruitcount$Var2
      crop = fruitcount$Var1
      x <- ggplot(fruitcount, aes(country,crop )) + geom_tile(aes(fill =  fruitcount$Freq), colour = "white") + scale_fill_gradient(low = "yellow", high = "red")
      print(x)
    }
    if(input$plot==6){
      attach(metal_danger)
      count = data.frame(table(crop,county))
      country = count$county
      crop = count$crop
      p <- ggplot(count, aes(country,crop )) + geom_tile(aes(fill =  count$Freq), colour = "white") + scale_fill_gradient(low = "yellow", high = "red")
      print(p)
    }
    if(input$plot==7){
      # courtimgurl<-"https://thedatagame.files.wordpress.com/2016/03/nba_court.jpg"
      # court <- rasterGrob(readJPEG(getURLContent(courtimgurl)),width=unit(1,"npc"), height=unit(1,"npc"))
      # subset<-filter(shotchart,PLAYER_NAME%in%input$player)
      # 
      # #group by shot regions(eliminate back court shots) and calculate the shots attempted and accuracy
      # df<-subset%>%group_by(SHOT_ZONE_BASIC,SHOT_ZONE_AREA,SHOT_ZONE_RANGE)%>%summarise(FGA=sum(SHOT_ATTEMPTED_FLAG),Accuracy=sum(SHOT_MADE_FLAG)/sum(SHOT_ATTEMPTED_FLAG))
      # "%ni%" <- Negate("%in%")   #create a new operator, which is the opposite of %in%
      # df<-filter(df,SHOT_ZONE_AREA%ni%"Back Court(BC)")
      # 
      # #set the coordinate as the average of all shots from particular region
      # df$LOCX<-all$LOCX
      # df$LOCY<-all$LOCY
      # df$Accuracy<-percent(round(df$Accuracy,digits=3)) #turn accuracy into percentage
      # 
      # region<-ggplot(df, aes(x=LOCX, y=LOCY)) + 
      #   annotation_custom(court, -250, 250, -50, 420) +
      #   geom_point(aes(size=FGA,color=ifelse(df$Accuracy>=averageleague$FG_PCT,"A","B")),shape=19,alpha=0.6)+
      #   geom_text(aes(label=Accuracy,color=ifelse(df$Accuracy>=averageleague$FG_PCT,"A","B")),vjust=-0.9,size=3)+
      #   xlim(-250, 250)+
      #   ylim(-50, 420)+
      #   ggtitle("Player's FG% by region(size by shots attempted)")+
      #   scale_color_manual(labels = c("above average", "below average"), values = c("#008000","#FF6347")) +
      #   blank_theme+
      #   theme(axis.text.x = element_blank(), axis.text.y = element_blank(),legend.position="bottom",legend.title = element_blank())
      # print(region)
    }
  })
  output$plot3 <- renderPlot({
    if(input$plot == 5){
      
      vetcount = data.frame(table(vet$crop,vet$county))
      country = vetcount$Var2
      crop = vetcount$Var1
      s <- ggplot(vetcount, aes(country,crop)) + geom_tile(aes(fill =  vetcount$Freq), colour = "white") + scale_fill_gradient(low = "yellow", high = "red")
      print(s)
    }
  })
}