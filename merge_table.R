library(dplyr)
water_test_data <- read.table("C:/Users/user/Desktop/water_test_data.csv",header = TRUE,sep = ",",encoding = "big5")
water_data <-  read.table("C:/Users/user/Desktop/water_data.csv",header = TRUE,sep = ",",encoding = "big5")

colnames(water_test_data)
colnames(water_test)
merge_table <- water_data %>%
                left_join(water_test_data)


data = fruitAll_UTF_8