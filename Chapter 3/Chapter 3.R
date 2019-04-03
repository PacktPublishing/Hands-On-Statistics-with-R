#Chapter 3 Asking About Groups
require(MASS)
require(car)
require(Hmisc)
require(multicomp)
require(gplots)
require(lattice)

#### Graphics for Maine ####

stripchart(Mercury ~ Stratification, data = Maine, method = "jitter", vertical = TRUE)

boxplot(Mercury ~ Stratification, data = Maine, boxwex = 0.15) 
means <- tapply(Maine$Mercury, Maine$Stratification, mean)
points(means,col = "red",pch = 18)

#bihistogram
myplot <- histbackback(split(Maine$Mercury, Maine$Stratification), brks = seq(0, 2.5, 0.1))
barplot(-myplot$left, col = "green", horiz = TRUE, space = 0, add = TRUE, axes = FALSE)
barplot(myplot$right, col = "green", horiz = TRUE, space = 0, add = TRUE, axes = FALSE)

# require (car); leveneTest(Mercury ~ Stratification, Maine)   

#### Statistics Tables for Maine Lakes ####
with(Maine, {
  # Group Sample Sizes
  table(Stratification)
  # Group Means
  aggregate(Mercury, by = list(Stratification), mean)
  #Group Standard Deviations
  aggregate(Mercury, by = list(Stratification), sd)
})

fitMercuryStrat = t.test(Mercury ~ Stratification, data = Maine)
# Root Mean Square Error
sigma(fitMercuryStrat)
#ANOVA Table
summary(fitMercuryStrat)

# Create the shoes data frame
shoes <- data.frame(shoes)
# Construct Parallel Coordinate plot
parcoord(shoes, var.label = TRUE, col = ifelse(shoes$A == 10.7,"red","light gray"), lwd = 4)
# Conduct the paired test.
t.test(shoes$A, shoes$B, paired = TRUE)

#### Building an ANOVA ####

# Grand Mean, stored in YY
YY <- mean(Maine$Mercury)
# Group means, stored in list LT
YG <- with(Maine,tapply(Mercury, Lake_Type, mean))
# Add columns to Maine data table for reduced model
Maine <- cbind(Maine, "YY" = Maine$Mercury - YY)
# Add column to Maine data table for full model
Maine <- cbind(Maine, "YG" = Maine$Mercury - YG[Maine$Lake_Type])
# Calculate Sums of Squares
sum((Maine$YY)^2)      # SS(Reduced Model) 
sum((Maine$YG)^2)      # SS(Full Model)
# Let R calculate the Anova()
fitMercuryLake <- aov(Mercury ~ Lake_Type, data = Maine)
summary(fitMercuryLake)
# Plot Means with 95% confidence bars
require(gplots)
plotmeans(Maine$Mercury ~ Maine$Lake_Type, xlab = "Stratification", ylab = "Mercury")

# ANOVA using lm()
aov(Mercury ~ Lake_Type, data = Maine)

#### Unmet Assumptions ####

### Unequal Variances
# Generate Clouds Data
Amount = c(1, 4.9, 4.9, 11.5, 17.3, 21.7, 24.4, 26.1, 26.3, 28.6, 29, 36.6, 41.1, 47.3, 68.5, 81.2, 87, 95, 147.8, 163, 244.3, 321.2, 345.5, 372.4, 830.1, 1202.6, 4.1, 17.5, 7.7, 31.4, 32.7, 40.6, 92.4, 115.3, 118.3, 119, 129.6, 198.6, 200.7, 242.5, 255, 274.7, 274.7, 302.8, 334.1, 430, 489.1, 703.4, 978, 1656, 1697.8, 2745.6)
Seed = gl(2, 26, labels = c("Seeded", "Unseeded"))
Clouds = data.frame(Seed, Amount)
# Look at the two levels -- standard deviations look suspicious
stripchart(Amount ~ Seed, data = Clouds, vertical = TRUE, at = c(1.25, 1.75))
# Charts of means and standard deviations
aggregate(Amount ~ Seed, data = Clouds, mean)
aggregate(Amount ~ Seed, data = Clouds, sd)
# The graphics show the problem, this equal-variance test is for doubters
var.test(Amount ~ Seed, data = Clouds)
# Use Wilcoxon or Kruskal-Wallis Nonparametric Tests
wilcox.test(Amount ~ Seed, data = Clouds)
kruskal.test(Amount ~ Seed, data = Clouds)
# Note that the parametric t-test does not reject with certainty
t.test(Amount ~ Seed, data = Clouds)

# Small Sample Sizes
data("PlantGrowth")
# Look at the two levels -- standard deviations look suspicious
stripchart(weight ~ group, data = PlantGrowth, vertical = TRUE, at = c(1.35, 2, 2.65))
# Charts of means and standard deviations
aggregate(weight ~ group, data = PlantGrowth, length)
aggregate(weight ~ group, data = PlantGrowth, mean)
aggregate(weight ~ group, data = PlantGrowth, sd)
# Use Kruskal-Wallis Nonparametric Test
kruskal.test(weight ~ group, data = PlantGrowth)

#### Multiple Comparison Tests ####
data("warpbreaks")
boxplot(breaks ~ tension,data = warpbreaks, boxwex = 0.3)
aggregate(breaks ~ tension, data = warpbreaks, length)
fitTens = aov(breaks ~ tension,data = warpbreaks)
summary(fitTens)
TukeyHSD(fitTens)
plot(TukeyHSD(fitTens))
require(multcomp)
#par(mar = c(5, 4, 6, 2))
THSD <- glht(fitTens, linfct = mcp(tension = "Tukey"))
plot(cld(THSD, level = 0.05), boxwex = 0.3)

# Ten-level ANOVA
# Import Rugby data set
theURL <- "http://www.statsci.org/data/oz/rugby.txt"
theNames <- c("Game", "Time")
Rugby <- read.table(theURL, header = T , col.names = theNames)
Rugby$Game <- as.factor(Rugby$Game)
fitRugby = aov(Time ~ Game, data = Rugby)
RugbyHSD <- glht(fitRugby, linfct = mcp(Game = "Tukey"))
plot(cld(RugbyHSD, level = 0.05))

# Two-Way ANOVA
fitTensAndWool = aov(breaks ~ tension + wool, data = warpbreaks)
