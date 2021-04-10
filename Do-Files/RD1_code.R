# A non fuzzy RD example using data from elections in the US
# from a course I took on policy analysis with ITS and RD (Edx)

# 1- let's load the data

data <- read.csv("~/Desktop/Edx_Policy_analysis/Week 5/electoral_rd.csv",header=T)

# for the sake of simplicity I will outline the names of the main vars and their function 
# as per RD designs

# dmargin = forcing variable
# demwin = indicator for threshold
# dwinnext = outcome

# due to the nature of the experiment there will be need to test sqr and cubic specifications: 
# below the transformations that will be used later

# cubic and quadratic terms for forcing variable
data$dmargin2 <- data$dmargin^2
data$dmargin3 <- data$dmargin^3

#interaction between forcing var and threshold
data$dmargin_demwin <- data$dmargin*data$demwin

#it is useful to pre set cubic and quadratic terms for the interaction
data$dmargin_demwin2 <- data$dmargin_demwin^2
data$dmargin_demwin3 <- data$dmargin_demwin^3

#I learned that setting the scientific notation off is useful
options(scipen=5)

#####################################################
# initial plot
####################################################

#set up bins for plotting 
bins <- seq(-49,49,2)

#getting the means within bins 
means <- tapply(data$dwinnext,data$bin,mean)

plot(bins,means,
     pch=19,
     ylab="Prob. winning next election",
     xlab= "vote margin in the last election",
     xlim=c(-50,50),
     col="lightblue")

#the line in the threshold
abline(v=0,lty=2,col="black")

#####################################################
# Modeling
####################################################   

model_1 <- lm(dwinnext ~ dmargin+demwin+dmargin_demwin, data=data)    
summary(model_1)

#adding the square term for dmargin and interaction

model_2 <- lm(dwinnext ~ dmargin+dmargin2+demwin+dmargin_demwin+dmargin_demwin2, data=data)    
summary(model_2)

#comparing both models with anova
anova(model_1, model_2)

#it is worth adding the cubic form

model_3 <- lm(dwinnext ~ dmargin+dmargin2+dmargin3+demwin+dmargin_demwin+dmargin_demwin2+dmargin_demwin3, data=data)    
summary(model_3)

#comparing last two models with anova
anova(model_2, model_3)

#####################################################
# Last plot 
####################################################  

# Setup new data frame for plotting
new <- data.frame(dmargin = seq(-50, 50, 0.5),
                  demwin=c(rep(0,100),rep(1,101)))
new$dmargin2 <- new$dmargin^2
new$dmargin3 <- new$dmargin^3
new$dmargin_demwin <- new$dmargin*new$demwin
new$dmargin_demwin2 <- new$dmargin_demwin^2
new$dmargin_demwin3 <- new$dmargin_demwin^3

# Plot the results
plot(bins,means,
     pch=19,
     ylab="Probability of Winning Next Election",
     xlab="Vote Margin in the Last Election",
     xlim=c(-50,50),
     ylim=c(0,1),
     col="lightblue")

# Add predicated values
pred <- predict(model_3, new)
lines(new$dmargin[1:100],pred[1:100],lwd=2,col="brown") 
lines(new$dmargin[102:200],pred[102:200],lwd=2,col="brown")

# Add line at zero
abline(v=0,lty=2,col="grey")

#end, thanks
