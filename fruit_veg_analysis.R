library(readr)
library(ggplot2)
dataset <- read_csv("C:/Users/user/Desktop/DataScience&MachineLearning/datascience_course/dataframe/veg_frult_all.csv")
#刪除重複資料
dataset = unique.data.frame(dataset)
#蔬菜
vet = dataset[dataset$type=="蔬菜",]
#水果
fruit = dataset[dataset$type=="水果",]
#受水汙染
water_danger = dataset[dataset$waterDanger == 1,]
#重金屬汙染
metal_danger = dataset[dataset$metalDanger == "鎘超標",]

pineapple = dataset[dataset$crop == "鳳梨",]
barplot(table(pineapple$county),horiz = TRUE,las=1)
barplot(gear.table2, beside = TRUE)
#--------heatmap------
library(ggplot2)
count = data.frame(table(crop,county))
p <- ggplot(count, aes(count$county,count$crop )) + geom_tile(aes(fill =  count$Freq), colour = "white") + scale_fill_gradient(low = "yellow", high = "red")
p
vetcount = data.frame(table(vet$crop,vet$county))
fruitcount = data.frame(table(fruit$crop,fruit$county))
p <- ggplot(vetcount, aes(vetcount$Var2,vetcount$Var1 )) + geom_tile(aes(fill =  vetcount$Freq), colour = "white") + scale_fill_gradient(low = "yellow", high = "red")
p <- ggplot(fruitcount, aes(fruitcount$Var2,fruitcount$Var1 )) + geom_tile(aes(fill =  fruitcount$Freq), colour = "white") + scale_fill_gradient(low = "yellow", high = "red")



#----------bubble-----------------
symbols(dataset$table(crop),
        dataset$table(county),
        inches = 0.15,
        bg= "purple"
)
