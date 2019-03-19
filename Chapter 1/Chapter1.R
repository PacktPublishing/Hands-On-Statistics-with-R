##################################################
#               CHAPTER ONE                      #
#     Exploring and Analyzing One Variable       #
#                                                #
##################################################

# PART ONE: CATEGORICAL VARIABLES

#Examination of data sets
require(datasets)
View(rock)

#Example of levels() command
View(chickwts)
levels(chickwts$feed)

# Example of names() command
View(mtcars)
names(mtcars)

# Structure of a data set with str() command
str(mtcars)

# Help!
?mtcars

# Simple stats: mean() and sd()
mean(mtcars$mpg)
sd(mtcars$mpg)
mean(mtcars$cyl)

# Examine Candies data set
head(Candies)
tail(Candies)
levels(Candies$MMs)

# Change to categorical/factor
Candies$MMs <- as.factor(Candies$MMs) 
Candies$BrandX <- as.factor(Candies$BrandX)

# levels() and how they are stored with unclass()
levels(Candies$MMs)
unclass(Candies$MMs)

# tables() command and summary of M&Ms
nMMs <- table(Candies$MMs)
nMMs

# Use counts to make barplot
barplot(nMMs)

# Decorate the bar plot
barplot(nMMs, main = "M&M Distribution", ylab = "Count of M&Ms", col = "green")

# Store colors of M&Ms
cMMs <- levels(Candies$MMs)

# Make a bar plot with candy colors
barplot(nMMs, main = "M&M Distribution", ylab = "Count of M&Ms", col = cMMs)

# Make color bar plot for Brand X
nBrandX <- table(Candies$BrandX)
cBrandX <- levels(Candies$BrandX)
barplot(nBrandX, main = "Brand X Distribution", ylab = "Count of Brand X", col = cBrandX)

# cbind() both plot in anticipation of...
nBoth <- rbind(nMMs, nBrandX)

# ...stacked bar chart
barplot(nBoth, main = "Candy Distribution")

# Add color to the stacked chart
barplot(nBoth, main = "Candy Distribution", ylab = "Candy Count", col = c("blue", "orange"))

# side-by-side bar chart
barplot(nBoth, main = "Candy Distribution", ylab = "Candy Count",  col = c("blue”, “orange"), beside = TRUE)

# Add a legend, although in a bad spot
barplot(nBoth, main = "Candy Distribution", ylab = "Candy Count", col = c("blue", "orange") )
legend("topright", c("M&Ms", "BrandX"), fill = c("blue", "orange"))

# Correct legend() call
barplot(nBoth, main = "Candy Distribution", ylab = "Candy Count", col = c("blue", "orange") )
legend("top", c("M&Ms", "BrandX"), fill = c("blue", "orange"))

# Exploring Tables
table(Candies$MMs)
table(Candies$BrandX)
table(Candies)

# Assure factors are in mtcars, preparing for table()
remove(mtcars) # in case there were already changes made
mtcars$vs <- factor(mtcars$vs, levels = c(0, 1), labels = c("V-Shaped", "Straight"))
mtcars$am <- factor(mtcars$am, levels = c(0, 1), labels = c("Automatic", "Manual"))
mtcars$gear <- factor(mtcars$gear, ordered = TRUE)
mtcars$carb <- factor(mtcars$carb, ordered = TRUE)
mtcars$cyl <- factor(mtcars$cyl, ordered = TRUE)

# table of vsand am
table(mtcars$vs, mtcars$am)

# Store this table in tttable (==transportation type table)
tttable <- table(mtcars$cyl, mtcars$am)
tttable

# addmargins() command
addmargins(tttable)

#3-D tabel with marginal totals in one call
addmargins(table(mtcars$am, mtcars$gear, mtcars$vs))

# Get length of Candies table
length(Candies$MMs)

# Compute proportions by hand
pMMs <- nMMs/250
pMMs
pBrandX <- nBrandX/250
pBrandX

#Compute proportions using prop.table()
pMMs <- prop.table(nMMs)
pBrandX <- prop.table(nBrandX)
pMMs
pBrandX

# Perform chi-square test on Candies
resMMs <- chisq.test(x = nMMs, p = c(0.1, 0.3, 0.1, 0.1, 0.2, 0.2))
resBrandX <- chisq.test(x = nBrandX, p = c(0.1, 0.3, 0.1, 0.1, 0.2, 0.2))

# Pull Values from fits
resMMs$p.value
resBrandX$method


################################## 
# PART TWO: CONTINUOUS VARIABLES #
##################################

# Assign Maine factor variabes
Maine$Lake_Type <- factor(Maine$Lake_Type, levels = c(1, 2, 3), labels = c("Mesotropic", "Eurotropic", "Oligotropic"))

# Histogram of Elevation
hist(Maine$Elevation)

# Two plots using with() command
with( Maine, {
  hist(Mercury)
  barplot(table(N_of_Fish), main = "Number of Fish")
})

# 2×2 plots, plain
savedGraphicsParams <- par(no.readonly = TRUE) # Save these for later
par(mfrow = c(2,2)) 
with(Maine,{
  barplot(table(N_of_Fish))
  hist(Elevation)
  hist(Surface_Area)
  hist(Maximum_Depth)
}) 
par(savedGraphicsParams)                      # Reset the graphics back

#Illustration of with() command
with( Maine, {
  hist(Elevation)
  barplot(table(N_of_Fish), main = "Number of Fish")
}) 


# 2×2 plots with details
savedGraphicsParams <- par(no.readonly = TRUE) #Save these for later
par(mfrow = c(2,2))
with(Maine, {
  barplot(table(N_of_Fish), main = "Fish", xlab = "No. of Fish")
  hist(Elevation, main = "Elevation", xlab = "ft")
  hist(Surface_Area, main = "Surface Area", xlab = "acres")
  hist(Maximum_Depth, main = "Max Depth", xlab = "ft")
}) 
par(savedGraphicsParams)                       # Reset the graphics back

# Summary using indices rather than column names
summary(Maine[3:6])

#Large Annotated Box Plots, which have labels added later
with(mpg, {
  boxplot(hwy ~ class, horizontal = TRUE, frame = FALSE, xlab = "mpg", las = 1)
})


#Three stacked bar charts
savedGraphicsParams <- par(no.readonly = TRUE)
layout(matrix(c(1, 2, 3), nrow = 3, byrow = TRUE))
with(Maine, {
  ElBox <- boxplot(Elevation, horizontal = TRUE, main = "Elevation (ft)", width = 1, frame = FALSE)
  SABox <- boxplot(Surface_Area, horizontal = TRUE, main = "Surface Area (acres)", width = 1, frame = FALSE)
  MDBox <- boxplot(Maximum_Depth, horizontal = TRUE, main = "Maximum Depth (ft)", width = 1, frame = FALSE)
}) 
par(savedGraphicsParams)


# Three stacked bar charts with histograms
savedGraphicsParams  <-  par(no.readonly = TRUE)
layout(matrix(c(1, 2), nrow = 1, byrow = TRUE)) # NOTE: layout() rather than par()
ElBox <- boxplot(Maine$Elevation, horizontal = TRUE, main = "Elevation (ft)", width = 1, frame = FALSE)
hist(Maine$Elevation, main = "Elevation", xlab = "ft")
SABox <- boxplot(Maine$Surface_Area, horizontal = TRUE, main = "Surface Area (acres)", width = 1, frame = FALSE)
hist(Maine$Surface_Area, main = "Surface Area", xlab = "acres")
MDBox <- boxplot(Maine$Maximum_Depth, horizontal = TRUE, main = "Maximum Depth (ft)", width = 1, frame = FALSE)
hist(Maine$Maximum_Depth, main = "Maximum Depth", xlab = "ft")
par(savedGraphicsParams)


# Stem-snd-Leaf plots
savedGraphicsParams <- par(no.readonly = TRUE)
layout(matrix(c(1, 2, 3), nrow = 3, byrow = TRUE))
with(Maine,{
  stem(Elevation)
  stem(Maximum_Depth)
})     
par(savedGraphicsParams)

# Using sum() to deal with missing data
sum(is.na(Maine$Elevation))
sum(is.na(Maine$Impoundment_Class))

# Calculate and store density plots
t1 <- Elevation[Impoundment_Class == "Some Man-Made Flowage"] # Note: ==, not =
t2 <- Elevation[Impoundment_Class == "All Natural Flowage"] # Note: ==, not =
d.t1 <- density(t1, na.rm = TRUE)
d.t2 <- density(t2, na.rm = TRUE)

# Get x and y ranges
d.t1.range <- range(d.t1$x, d.t2$x)
d.t2.range <- range(d.t1$y, d.t2$y)

# Draw density plots with labels
plot(d.t1, xlim = d.t1.range, ylim = d.t2.range, xlab = "Density", main = "Density Plot of Elevation by Impoundment Class")
lines(d.t2, lty = 2)
legend("topright", legend = c("None", "Some"), inset = 0.05, lty = c(1,2), title = "Impoundment")

# Summarize Elevation values
summary(Maine$Elevation)

# Calculate and show π, sprt(2), e
irrationals <- as.data.frame(cbind(1:10, pi*1:10, sqrt(2)*1:10, exp(1)*1:10))
irrationals

# Find mean of columns using apply()
apply(irrationals, 2, mean) #2 for columns

# find mean of rows using apply()
apply(irrationals, 1, mean) # 1 for rows

# Find standard deviation of rows
apply(irrationals, 1, sd)

# Use floor() function with apply()
apply(irrationals, 2, floor)

# Make a function() and use it with apply()
apply(irrationals, 2, function(x){ log(pi)/log(x) } )

# What happens with apply(Maine)?
apply(Maine, 2, mean)

# Examine Maximum Depth variablee
Maine$Maximum_Depth

# apply() and ignore na values using na.rm = TRUEE
apply(Maine[4:6], 2, mean, na.rm = TRUE)

# Correct version of apply()
apply(Maine[4:6], 2, mean)

# Compare Tukey's five number summary with summary() command
fivenum(Maine$Elevation)
summary(Maine$Elevation)

# Use describe() from psych package
require(psych)
describe(Maine$Elevation)

# Use describe() from Hmisc package
require(Hmisc)
describe(Maine$Elevation)


#Q-Q Plots
w <- rnorm(1000)
x <- rchisq(1000, 6)
y <- rchisq(1000,1)
z <- 1 - rchisq(1000,1)
savedGraphicsParams <- par(no.readonly = TRUE)

#Set up 2×2 frame and change open circle marker to smaller dot
par(mfrow = c(2,2))

# Plot 4 Distributions
hist(w, main = "Normal Distribution")
hist(x, main = parse(text = "chi^2"))
hist(y, main = "Skewed Left")
hist(z, main = "Skewed Right")

# Draw 4 QQ Plots
par(pch = 20)
qqnorm(w); qqline(w, col = 2)
qqnorm(x); qqline(x, col = 2)
qqnorm(y); qqline(y, col = 2)
qqnorm(z); qqline(z, col = 2)
par(savedGraphicsParams) #Reset the graphics


# Q-Q Plot
savedGraphicsParams <- par(no.readonly = TRUE) #Save these for later
#Set up 3×1 frame
par(mfrow = c(1,3))
with( Maine,{
  qqnorm(Elevation)
  qqline(Elevation, col = "red")
  qqnorm(Surface_Area)
  qqline(Surface_Area, col = "red")
  qqnorm(Maximum_Depth)
  qqline(Maximum_Depth, col = "red")
})
par(savedGraphicsParams) #Reset the graphics

# Normality Tests
shapiro.test(w)
shapiro.test(x)
shapiro.test(y)
shapiro.test(z)

# Draw labeled normal curve for mercury
mm=mean(Maine$Mercury)
msd=sd(Maine$Mercury)
ll=mm-4*msd
ul=mm+4*msd

f <- function(x) dnorm(x, mm, msd)
plot(f, -0.5, 1.5, axes=F, xlab="", ylab="", col="gray")
axis(1)

segments(mm, 0, mm, f(mm), col = "pink")
segments(mm - msd, 0, mm - msd, f(mm - msd), lty = "dotted", col = "red")
segments(mm + msd, 0, mm + msd, f(mm + msd), lty = "dotted", col = "red")
segments(mm - 2*msd, 0, mm - 2*msd, f(mm - 2*msd), lty = "dotted", col = "red")
segments(mm + 2*msd, 0, mm + 2*msd, f(mm + 2*msd), lty = "dotted", col = "red")
segments(mm - 3*msd, 0, mm - 3*msd, f(mm - 3*msd), lty = "dotted", col = "red")
segments(mm + 3*msd, 0, mm + 3*msd, f(mm + 3*msd), lty = "dotted", col = "red")
arrows(0.05, f(mm), mm, f(mm), length = 0.1)
text(-0.25, f(mm), paste("Mean Hg =", mm))
arrows(mm - msd, 0.65, mm + msd, 0.65, length = 0.1, code = 3)
text(mm + msd + 0.28, 0.65, "68% of the data")
arrows(mm - 2*msd, 0.1, mm + 2*msd, 0.1, length = 0.1, code = 3)
text(mm + 2*msd + 0.28, 0.1, "95% of the data")
axis(1, at = seq(mm - 2*msd, mm + 2*msd, msd), pos = 0.3, col.axis = "blue", labels = c("-2s", "-1s", "mean", "+1s", "+2s"), lwd = 0)


# Test mercury level at typical state standard
t.test(Maine$Mercury, mu = 0.5)

#Test Mercury at MAine standard
t.test(Maine$Mercury, mu = 0.43)
