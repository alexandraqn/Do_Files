

library(openxlsx)

#creating data frame

data <- read.xlsx("/Users/aleqn/Documents/GitHub/R_Projects/CSV/hlo_database.xlsx", 
                  sheet = "HLO Database" )
head(data)
tail(data)
View(data)
dim(data)
str(data)

summary(data)