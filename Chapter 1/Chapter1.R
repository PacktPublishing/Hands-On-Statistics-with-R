
Candies <- read_csv("Library/Containers/com.eltima.cloudmounter.mas/Data/.CMVolumes/Dropbox/0Packt Book Resources/Data Files/Final/Candies.csv")
View(Candies)
Candies$MMs <- as.factor(Candies$MMs)
Candies$BrandX <- as.factor(Candies$BrandX)
levels(Candies$MMs)
levels(Candies$BrandX)
