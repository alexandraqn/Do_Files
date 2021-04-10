#interrupted time series
# the following code was inspired by the edx course ITSx. I mostly use stata
# so this first code for ITS is a great accomplished in my learning path.

#1- first read the data, the file shows time series data of the Nile river and the Huron river.
#I understand that the file is open source however compilation and sharing should be accredited 
#to professor Mike Law from the edx ITSx course.

data <- read.csv("~/Desktop/R-Projects/nilehuron.csv",header=T)

###################################
# run libraries
###################################
library(nlme)
library(carData) 
library(car)

#When I initially set up the vars in excel I organised the level, time and trend,
# improperly. To fix this I created the code below to set up the vars as per specifications needed
# for the ITS model.
data$time <- c(1:60,1:60)
data$level <- c(rep(0,27), rep(1,33), rep(0,27), rep(1,33))  
data$trend <- c(rep(0,27), 1:33, rep(0,27), 1:33 )
data$niletime <- data$nile*data$time
data$nilelevel <- data$nile*data$level
data$niletrend <- data$nile*data$trend
###################################
#preliminary plot
###################################

# It is always a good idea to plot the data to see the discontinuity between T and C
# over time. below the code I used.

plot(data$time[1:60],data$flow[1:60],
     ylab="water flow",
     ylim=c(0,4500),
     xlab="year",
     type="l",
     col="red",
     xaxt="n")

#control group- huron river flow. It is important to note that the cut off in this case is time 60
# 61 onwards is a post cut off period. 
points(data$time[61:120],data$flow[61:120],
       type='l',
       col="blue")

#year axis
axis(1, at=1:60, labels=data$year[1:60])

#add points graph
points(data$time[1:60],data$flow[1:60],
       col="red",
       pch=20)

points(data$time[61:120],data$flow[61:120],
       col="blue",
       pch=20)

##### vertical line for change in river flow due to drought
abline(v=27.5,lty=2)

#legend- I added the legend to make it easier to visualize

legend(x=3, y=1000, legend=c("nile","huron"),
       col=c("red","blue"),pch=20)

###################################
# modelling
###################################

model_ols <- lm(flow ~ time+nile+niletime+level+trend+nilelevel+niletrend, data=data)
summary(model_ols)
confint(model_ols) #95% confidence intervals
#preliminary interpretation

#the coefficient of the var nile is the difference between  the rivers nile and huron before the cut off.
#the coefficient of the var niletime is the trend difference between the rivers nile and huron water flow.
#the coefficient of the var level is the level change in the huron river water flow.
#the coefficient of the var lever trend is the trend change in the huron river water flow.

#however to continue the interpretation of the series we need to account for possible autocorrelation.
#perfom durwin watson
dwt(model_ols, max.lag=12, alternative="two.sided")

#values greater than 2 show possible presence of negative AR processes, conversely values
#lower to 2 can show the presence of positive AR. As per the DW output it seems there may be an AR process in 
#period 10.

#graph residuals
plot(data$time[1:60],
     residuals(model_ols)[1:60],
     type='o',
     pch=16,
     xlab='time',
     ylab='OLS resid',
     col="red")
abline(h=0,lty=2)

#autocorrelation plots ACF and PACF
par(mfrow=c(1,2))

acf(residuals(model_ols))
acf(residuals(model_ols),type='partial')

#the residual plot confirms the presence of AR 10. the model should include this in
#the specification.

###################################
# updated model
###################################

#GLS model to include an AR(10) MA(0) process. where p is denoted for AR and q for MA.

model_p10 <- gls(flow ~ time+nile+niletime+level+trend+nilelevel+niletrend, data=data,
                 correlation=corARMA(p=10,form=~time|nile), method="ML")

#the character "form" takes in consideration the relative comparison that I want in my specification

summary(model_p10)
confint(model_p10)

#to test if other AR processes could improve the estimation we conduct a likelihood test

model_p10q1 <- update(model_p10,correlation=corARMA(q=1,p=10,form=~time|nile))
anova(model_p10,model_p10q1)

model_p11 <- update(model_p10,correlation=corARMA(p=11,form=~time|nile))
anova(model_p10,model_p11)

# Put plotting back to one chart
par(mfrow=c(1,1))

# Residual plot
qqPlot(residuals(model_p10))

#the residuals seem to fit best in the AR 10 specification of the GLS

###################################
# final plot
###################################

# First  the Nile
plot(data$time[1:60],data$flow[1:60],
     ylim=c(0,4500),
     ylab="Water Flow",
     xlab="Year",
     pch=20,
     col="lightblue",
     xaxt="n")

# Add x-axis year labels
axis(1, at=1:60, labels=data$year[1:60])
# Label the change
abline(v=27.5,lty=2)

# Add in the points for the control
points(data$time[61:120],data$flow[61:120],
       col="pink",
       pch=20)

# Plot the first line segment for the intervention group
lines(data$time[1:27], fitted(model_p10)[1:27], col="blue",lwd=2)

# Add the second line segment for the intervention group
lines(data$time[28:60], fitted(model_p10)[28:60], col="blue",lwd=2)

# Add the counterfactual for the intervention group
segments(28, model_p10$coef[1] + model_p10$coef[2]*28 + model_p10$coef[3]+model_p10$coef[4]*28 + 
           model_p10$coef[5] + model_p10$coef[6],
         60, model_p10$coef[1] + model_p10$coef[2]*60 + model_p10$coef[3]+model_p10$coef[4]*60 + 
           model_p10$coef[5] + model_p10$coef[6]*33,
         lty=2,col='blue',lwd=2)

# Plot the first line segment for the control group
lines(data$time[61:87], fitted(model_p10)[61:87], col="red",lwd=2)

# Add the second line segment for the control
lines(data$time[88:120], fitted(model_p10)[88:120], col="red",lwd=2)

# Add the counterfactual for the control group
segments(1, model_p10$coef[1]+model_p10$coef[2],
         60,model_p10$coef[1]+model_p10$coef[2]*60,
         lty=2,col='red',lwd=2)

# Add in a legend
legend(x=3, y=1000, legend=c("Nile","Huron"), col=c("blue","red"),pch=20)

#############################################
# Prediction for 30 periods after the cut off
#############################################
pred <- fitted(model_p10)[57]
counterfac <-  model_p10$coef[1]+model_p10$coef[2]*57 +
  model_p10$coef[3]+model_p10$coef[4]*57+
  model_p10$coef[5]+model_p10$coef[6]*30
#absolute change
pred-counterfac

#relative change
(pred-counterfac)/counterfac
#-38% change relative to the counter factual of water flow

#end, thanks