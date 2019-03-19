require(HistData)
require(car)
require(MASS)
require(graphics)

# Plain Galton Plot
data("Galton")
saveGraphics <- par(no.readonly = TRUE) # Save graphics parameters
  plot(Galton)
par(saveGraphics)            # Reinstate default parameters


# Galton Plot with jitter
saveGraphics <- par(par(no.readonly = TRUE)) # Save graphics parameters
plot(jitter(child,5) ~ jitter(parent,5), xlab = "parent", ylab = "child", data = Galton)
par(saveGraphics)                          # Reinstate default parameters


# Galton Plot with jitter and lines
saveGraphics <- par(no.readonly = TRUE) # Save graphics parameters
# plot the scatterplot a little lighter
plot(jitter(child,5) ~ jitter(parent,5), xlab = "parent", ylab = "child", data = Galton, col = "dark gray")
abline(a = 0, b = 1, lwd = 3)
# fit regression line
reg = lm(child ~ parent, Galton)
#plot regression line
abline(reg, lty = 5, lwd = 3)
abline(h = mean(Galton$child), lty = 3, lwd = 2)
par(saveGraphics)                   # Reinstate default parameters


# Import Reaction data set
theURL <- "http://lib.stat.cmu.edu/datasets/Andrews/T30.1"
theNames <- c("Table", "Number", "Row", "Experiment", "Temperature", "Concentration", "Time", "Unchanged", "Converted", "Unwanted")
Reaction <- read.table(theURL, header = F , col.names = theNames)
View(Reaction)

# Remove the first four useless columns
Reaction <- Reaction[-c(1:4)]

# Calculate Means and SDs
TempMean <- mean(Reaction$Temperature)
ConvMean <- mean(Reaction$Converted)
TempSD <- sd(Reaction$Temperature)
ConvSD <- sd(Reaction$Converted)

# Plot with base R
plot(Reaction$Temperature, Reaction$Converted, xlab = "Temperature", ylab = "% Converted", main = "Reaction Results", xlim = c(TempMean - 2.5*TempSD, TempMean + 2.5*TempSD), ylim = c(ConvMean - 2.5*ConvSD, ConvMean + 2.5*ConvSD))


# Scatterplot with car library
library(car)
scatterplot(Reaction$Temperature, Reaction$Converted, smooth = FALSE, regLine = FALSE, xlab = "Temperature", ylab = "% Converted", main = "Reaction Results", xlim = c(TempMean - 2.5*TempSD, TempMean + 2.5*TempSD), ylim = c(ConvMean - 2.5*ConvSD, ConvMean + 2.5*ConvSD))


# Scatterplot with Ellipse
scatterplot(Reaction$Temperature, Reaction$Converted, smooth = FALSE, regLine = FALSE, grid = FALSE, xlim = c(TempMean - 2.5*TempSD, TempMean + 2.5*TempSD), ylim = c(ConvMean - 2.5*ConvSD, ConvMean + 2.5*ConvSD), xlab = "Temperature", ylab = "% Converted", main = "Reaction Results", ellipse = list(levels = c(0.90), fill.alpha = 0.1, robust = FALSE, fill = TRUE))


# Mean and sd Lines
scatterplot(Reaction$Temperature, Reaction$Converted, smooth = FALSE, regLine = FALSE, grid = FALSE, reset.par = FALSE, xlab = "Temperature", ylab = "% Converted", main = "Reaction Results", xlim = c(TempMean - 2.5*TempSD, TempMean + 2.5*TempSD), ylim = c(ConvMean - 2.5*ConvSD, ConvMean + 2.5*ConvSD))
abline(col = c("red", "green"), v = TempMean, h = ConvMean)
abline(col = c("green"), lty = "dotted", v = (c(TempMean - 2*TempSD, TempMean + 2*TempSD)))
abline(col = c("red"),lty = 3, h = (c(ConvMean - 2*ConvSD, ConvMean + 2*ConvSD)))


# Plot the four analysis types
saveGraphics <- par(par(no.readonly = TRUE)) # Save graphics parameters
par(mfrow = c(2,2))                          # 2 by 2 Grid
# Continuous X, Continuous Y
plot(Maine$Mercury, Maine$Elevation, xlab = "Mercury", ylab = "Elevation", main = "Scatter Plot")
# Categorical X, Continuous Y
plot(Maine$Lake_Type, Maine$Elevation, xlab = "Lake Type", ylab = "Elevation", main = "Strip Chart")
# Continuous X, Categorical X
plot(Maine$Mercury, Maine$Lake_Type, xlab = "Mercury", ylab = "Lake_Type", main = "Spine Plot")
# Categorical X, Categorical Y
plot(Maine$Stratification, Maine$Lake_Type, xlab = "Stratification", ylab = "Lake Type", main = "Mosaic Plot")
par(saveGraphics)                            # Reinstate default parameters

# Back to Scatterplot with mean, sd limits, and now line
scatterplot(Reaction$Temperature, Reaction$Converted, smooth = FALSE, regLine = FALSE, grid = FALSE, xlim = c(TempMean - 2.5*TempSD, TempMean + 2.5*TempSD), ylim = c(ConvMean - 2.5*ConvSD, ConvMean + 2.5*ConvSD), xlab = "Temperature", ylab = "% Converted", main = "Reaction Results")
abline(col = c("red", "green"), v = TempMean, h = ConvMean)
abline(col = "green", lty = "dotted", v = (c(TempMean - 2*TempSD, TempMean + 2*TempSD)))
abline(col = "red", lty = 3, h = (c(ConvMean - 2*ConvSD, ConvMean + 2*ConvSD)))
#abline(col = "blue", lty = 2, 70.56891, ConvSD/TempSD)
segments(TempMean - 3*TempSD, ConvMean - 3*ConvSD, TempMean + 3*TempSD, ConvMean + 3*ConvSD)

# Scale Reaction Data
zReaction <- as.data.frame(scale(Reaction))
zTempMean <- mean(zReaction$Temperature)
zConvMean <- mean(zReaction$Converted)
zTempSD <- sd(zReaction$Temperature)
zConvSD <- sd(zReaction$Converted)
# Mean and sd Lines
scatterplot(zReaction$Temperature, zReaction$Converted, smooth = FALSE, regLine = FALSE, grid = FALSE, reset.par = FALSE, xlab = "Temperature", ylab = "% Converted", main = "Reaction Results", xlim = c(zTempMean - 2.5*zTempSD, zTempMean + 2.5*zTempSD), ylim = c(zConvMean - 2.5*zConvSD, zConvMean + 2.5*zConvSD))
abline(col = c("red", "green"), v = zTempMean, h = zConvMean)
abline(col = "green", lty = "dotted", v = (c(zTempMean - 2*zTempSD, zTempMean + 2*zTempSD)))
abline(col = "red", lty = 3, h = (c(zConvMean - 2*zConvSD, zConvMean + 2*zConvSD)))
segments(zTempMean - 3*zTempSD, zConvMean - 3*zConvSD, zTempMean + 3*zTempSD, zConvMean + 3*zConvSD)

#Correlation Examples
library("Hmisc")
#Import Correlation Examples Data
corrmatrix = rcorr(as.matrix(Correlation.Examples[2:51,]))

#Correlation using base R
cor(Reaction$Converted, Reaction$Temperature)

# GimmeCor() function
require(MASS)
GimmeCorr <- function(r, n = 100){
  as.data.frame(mvrnorm(n, mu = c(0,0), Sigma = matrix(c(1,r,r,1), ncol = 2), empirical = TRUE))
}
# Display Sample Positive Correlations
saveGraphics <- par(no.readonly = TRUE) # Save graphics parameters
par(mfrow <- c(2,2))
plot(GimmeCorr(-0.95), xlab = expression(italic(x)), ylab = expression(italic(y)), main = expression(italic(r)~" = 0.95"))
plot(GimmeCorr(-0.90), xlab = expression(italic(x)), ylab = "y", main = expression(italic(r)~" = 0.90"))
plot(GimmeCorr(-0.50), xlab = expression(italic(x)), ylab = "y", main = expression(italic(r)~" = 0.50"))
plot(GimmeCorr(0),  xlab = expression(italic(x)), ylab = "y", main = expression(italic(r)~" = 0"))
par(saveGraphics)                       # Reinstate default parameters

# Display Sample Negative Correlations
saveGraphics <- par(no.readonly = TRUE) # Save graphics parameters
par(mfrow <- c(2,2))
plot(GimmeCorr(-0.95), xlab = expression(italic(x)), ylab = expression(italic(y)), main = expression(italic(r)~" = –0.95"))
plot(GimmeCorr(-0.90), xlab = expression(italic(x)), ylab = "y", main = expression(italic(r)~" = –0.90"))
plot(GimmeCorr(-0.50), xlab = expression(italic(x)), ylab = "y", main = expression(italic(r)~" = –0.50"))
plot(GimmeCorr(0),  xlab = expression(italic(x)), ylab = "y", main = expression(italic(r)~" = 0"))
par(saveGraphics)                       # Reinstate default parameters


# Compute Correlations
cor(Temperature, Converted)
cor(Converted, Temperature)


# Test Correlations
cor.test(Reaction$Temperature, Reaction$Converted)


############ FOLLOWING TRANSFORMATIONS DO NOT AFFFECT r ###################

# Swap X and Y
cor(Reaction$Temperature, Reaction$Converted)
cor(Reaction$Temperature, Reaction$Converted)

# Add a number to each variable
Reaction$Temperature
Reaction$Temperature + 5
cor(Reaction$Temperature + 5, Reaction$Converted)
cor(Reaction$Temperature + 5, Reaction$Converted + 7)

#Multiply a positive number on each side
3*Reaction$Temperature
2*Reaction$Converted
cor(3*Reaction$Temperature, 2*Reaction$Converted)

###########################################################################

# Graph of Means
require(Hmisc)
plot.new()
plot.window(range(Galton$parent), range(Galton$child))
axis(1, at = seq(60.5, 74.5, 1))
axis(2, at = seq(60.5, 74.5, 1), las = 2)
box()
title("Galton's Data")
mtext("parent", side = 1, line = 2.5)
mtext("child", side = 2, line = 3)
points(jitter(child, 5) ~ jitter(parent, 5), data = Galton, col = "gray")
points(aggregate(child ~ parent, data = Galton, mean), pch = 15)
lines(aggregate(child ~ parent, data = Galton, mean))
abline(v = c(65, 66), lty = "dashed")


# Galton regression
GaltFit <- lm(child ~ parent, data = Galton)

# Add predicted Values
#abline(predict(GaltFit)) NOT SURE WHY THIS IS HERE

# Add regression line to plot
abline(GaltFit, lwd = 3, col = "red")

#Predict three new values using our fit
predict(GaltFit, data.frame(parent = c(60,65,70)))

#Show all 928 predicted values
predict(GaltFit)

#Show all 928 fitted values -- Same thing as predict()
fitted(GaltFit)


# Graph of Actual, Predicted. and Error terms 
require(Hmisc)
require(grDevices)
require(graphics)
data("Galton")
plot.new()
plot.window(range(Galton$parent), range(Galton$child))
# x-axis
arrows(64.5, 61.5, 64.5, 74, length = 0.1)
# y-axis
arrows(64.5, 61.46, 72.5, 61.46, length = 0.1)
# fit regression line
reg = lm(child ~ parent, Galton)
#arrows(64.5, 65.627, 74, 71.77, lwd = 2)
arrows(64.5, 65.627, 73, 71.12, lwd = 2)
# (xb, yb) is the simulated "actual" point
xb = 68.5; yb = 70.5 
# p is the predicted value at 69
p = predict(reg, data.frame(parent = xb))
#simulated "actual" value
points(xb, yb, col = "black", pch = 20, cex = 2) # "actual" value
segments(xb, 0, xb, yb, lty = "dashed")
# line from "actual" to y-axis
segments(xb, yb, 64.5, yb, lty = "dashed")
# bracket from axis to predicted == PREDICTED VALUE
brackets(xb, p, xb, 61.5, col = "blue")
# bracket from predicted to "actual"== ERROR
brackets(xb, yb, xb, p, col = "blue")
# bracket from "actual to axis == ACTUAL VALUE
brackets(64.5, 61.5, 64.5, yb)
# label brackets
text(xb + 0.5, (p + 61.5)/2, "Predicted Value", pos = 4, font = 2, col = "blue")
text(xb + 0.5, (yb + p)/2, "Error", pos = 4, font = 2, col = "blue")
text(63.8, (yb + 61.5)/2, "Actual Value", font = 2, srt = 90 )


#Compute actual - predicted for residual plot
ChildDiff <- Galton$child - fitted(GaltFit)

#How the plots appear
plot(GaltFit)

# Show default plots from a linear fit
savedGraphics <- par(no.readonly = TRUE) # Save graphics parameters
par(mfrow = c(2, 2))
plot(GaltFit)
par(savedGraphics)

# Fit regression model of volume vs girth with trees data set
scatterplot(trees$Girth, trees$Volume, regLine = FALSE, smooth = FALSE, grid = FALSE)
treeFit <- lm(trees$Volume ~ trees$Girth)

# Check the plotss
savedGraphics <- par(no.readonly = TRUE)
par(mfrow = c(2, 2))
plot(treeFit)
par(savedGraphics)

# scatterplot of Galton Data
plot(jitter(child,5) ~ jitter(parent, 5), xlab = "parent", ylab = "child", data = Galton, col = "gray", main = "Galton Data")
# plot mean Y
meanY <- mean(Galton$child) #calculate mean
segments(min(Galton$parent), meanY, max(Galton$parent, meanY), col = "#522D80", lwd = 4) #plot over data range
# Plot fit
abline(GaltFit, col = "#F66733", lwd = 3)
# Same x range but small increments for calculating confidence bands
lowerLimit <- min(Galton$parent) - sd(Galton$parent)
upperLimit <- max(Galton$parent) + sd(Galton$parent)
newParent <- seq(lowerLimit, upperLimit, 0.2)
# Calculate interval for prediction (i.e. on the slope)
prd <- predict(GaltFit, newdata = data.frame(parent = newParent), interval = c("confidence"), level = 0.95, type = "response")
# Plot the prediction limits
lines(newParent, prd[ ,2], col = "#F66733", lty = "dotted", lwd = 2)
lines(newParent, prd[ ,3], col = "#F66733", lty = "dotted", lwd = 2)
# Calculate interval for predicting a single x
prd <- predict(GaltFit,newdata = data.frame(parent = newParent),interval = c("prediction"), level = 0.95, type = "response")
lines(newParent, prd[ ,2], col = "#F66733", lty = "dotted", lwd = 2)
lines(newParent, prd[ ,3], col = "#F66733", lty = "dotted", lwd = 2)
# Get regression tables using summary()
summary(GaltFit)

# Make Pendulum Data Set
length <- c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50)
time <- c(0.16, 0.29, 0.35, 0.50, 0.59, 0.6, 0.70, 0.71, 0.70, 0.74)
pendulum = data.frame(length, time)
scatterplot(pendulum$length, pendulum$time, smooth = FALSE)
linePend = lm(pendulum$time ~ pendulum$length)
residualPlot(linePend, quadratic = FALSE)
pendulum <- cbind(pendulum, "rLength" = sqrt(pendulum$length))
scatterplot(pendulum$rLength, pendulum$time, smooth = FALSE)
newFitPend = lm(pendulum$time ~ pendulum$rLength)
residualPlot(newFitPend, quadratic = FALSE)
scatterplot(pendulum$length, pendulum$time, smooth = FALSE, regLine = FALSE)
lines(pendulum$length, fitted(newFitPend), type = "l", col = "red", lwd = 2)


#Exponential Data
Time = seq(0,360, 20)
Temp = c(138, 126, 115.8, 107.7, 100.3, 94.8, 89.7, 85.8, 82.3, 79.6, 77.1, 75.1, 73.5, 72, 70.8, 69.9, 69.1, 68.4, 67.6)
CoffeeCool = data.frame(Time, Temp)
scatterplot(CoffeeCool$Time, CoffeeCool$Temp, smooth = FALSE, regLine = FALSE)
# Adjust for asymptote
CoffeeCool <- cbind(CoffeeCool, "AdjTemp" = Temp - 65)
#Check that asymptote==0
scatterplot(CoffeeCool$Time, CoffeeCool$AdjTemp, smooth = FALSE, regLine = FALSE)
#Alternative Ways to Check Logs
scatterplot(CoffeeCool$Time, CoffeeCool$AdjTemp, smooth = F, grid = F, regLine = F, log = "y")
scatterplot(CoffeeCool$Time, CoffeeCool$AdjTemp, smooth = F, grid = F, regLine = F, log = "x")
#Take log(Y)
CoffeeCool <- cbind(CoffeeCool, "logAdjTemp" = log(CoffeeCool$AdjTemp))
scatterplot(CoffeeCool$Time, CoffeeCool$logAdjTemp, smooth = FALSE, regLine = FALSE)
# Fit Model
fitCoffee = lm(CoffeeCool$logAdjTemp ~ CoffeeCool$Time)
# Get fit on original scale
fittedTemp = exp(coef(fitCoffee)[1])*exp(coef(fitCoffee)[2]*CoffeeCool$Time) + 65
#Scatterplot of original data
plot(CoffeeCool$Time, CoffeeCool$Temp, col = "blue", main = "Observed and Predicted Values", xlab = "Temp (ºF)", ylab = "Time (min)")
grid(lty = "solid", col = "light gray")
lines(CoffeeCool$Time, fittedTemp, type = "l")

# Planets.csv should be imported
scatterplot(Planets$Mean.distance.from.Sun, Planets$Period.of.revolution, smooth = FALSE, regLine = FALSE, grid = FALSE)
scatterplot(log(Planets$Mean.distance.from.Sun), log(Planets$Period.of.revolution), smooth = FALSE)
fitPlanets = lm(log(Planets$Period.of.revolution) ~ log(Planets$Mean.distance.from.Sun))