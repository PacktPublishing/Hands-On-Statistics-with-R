library(mosaicData)
library(vcd)
library(datasets)
library(gplots)
library(ca)

#### Look at some data`` ####
levels(Whickham$outcome)
levels(Whickham$smoker)
str(Whickham$outcome)
str(Whickham$smoker)
# Examine Smoker Data
table(Whickham$outcome, Whickham$smoker)
with(Whickham, table(outcome, smoker))
xtabs(~ outcome + smoker, data = Whickham)

#### Graphical Methods ####
WhickTable <- xtabs(~ smoker + outcome, data = Whickham)
mosaicplot(WhickTable)
# Get official Clemson University colors from Google, which sends us to 
# https://www.clemson.edu/brand/guide/color.html
mosaicplot(WhickTable, color = c("#F66733", "#522D80"), main = "Whickham Data", xlab = "Smoker?", ylab = "outcome")

#### Categorical Models ####
# Titanic Example #
csT <- with(CTitanic, table(class, survived)) # csT = class  survival Titanic
addmargins(csT)
csTest <- chisq.test(csT)
summary(csTest)
csTest$expected

# Approximate vs Exact Test #
choose(8, 4)

## Entering a table by hand
# Enter Matrix Values, 2 columns, entered by row
Tea = matrix(c(4, 0, 0, 4), ncol = 2, byrow = TRUE)
# Add Column and row names, which must be in a list
dimnames(Tea) = list(c("Had Tea", "Had Milk"), c("Guessed Tea", "Guessed Milk"))
# Tea.Table <- as.table(Tea.Table) # If you need Tea matrix as list
fisher.test(Tea, alternative = "greater")
chisq.test(Tea)

# Test that crazy smoking/survival result from the start of the chapter
WhickTable <- xtabs( ~ smoker + outcome, data = Whickham)
prop.table(WhickTable, 1)
#      outcome
#smoker     Alive      Dead
#   No  0.6857923 0.3142077
#   Yes 0.7611684 0.2388316
fisher.test(WhickTableOS)

#### Correspondence Analysis ####
HEC <- data.frame(HairEyeColor)
HE <- xtabs(Freq ~ Hair + Eye, data = HEC) # HE = Hair and Eye
csHE <- chisq.test(HE) # csHS = chisq(HE)
csHE$observed - csHE$expected
caHE <- ca(HE)
plot(caHE)

#### Odds, Odds Ratios CMH Tests ####
# Odds are calculated using these two tables
xtabs( ~ smoker + AgeRange, subset = (outcome == "Alive"), data = Whickham)
xtabs( ~ smoker + AgeRange, subset = (outcome == "Dead"), data = Whickham)
WhickhamASO <- xtabs(~ (outcome + smoker + AgeRange), data = Whickham)
# So odds, odds ratios, and CMH test are...
odds(WhickhamASO)
oddsratio(WhickhamASO, log = FALSE)
mantelhaen.test(WhickhamASO)

# Our extended CMH Test uses Maine Data Table,
# N_of_Fish, Stratification, and Lake Type variables 
table(Maine$N_of_Fish, Maine$Stratification)
mantelhaen.test(Maine$N_of_Fish, Maine$Stratification,  Maine$Lake_Type)
