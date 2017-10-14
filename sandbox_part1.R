
if(!require(psych)) {
  install.packages("psych") 
  library(psych)
}

if(!require(ggplot2)) {
  install.packages("ggplot2") 
  library(ggplot2)
}

if(!require(scales)) {
  install.packages("scales") 
  library(scales)
}

# Reading in the data
politics <- read.csv("data/politics.txt", sep = "\t")

# Explore
str(politics)
summary(politics)

# PCA
# Base package: https://www.r-bloggers.com/principal-component-analysis-in-r/
politics_pca <- prcomp(politics[,-1], scale = TRUE)
politics_pca
plot(politics_pca)
summary_pca <- summary(politics_pca)
summary_pca
var_importance <- data.frame(number_of_PC = 1:length(colnames(summary_pca$importance)), 
                         cumulative_variation_explained = summary_pca$importance[3,])
ggplot(var_importance) +
  geom_line(aes(number_of_PC, cumulative_variation_explained)) +
  scale_x_continuous(breaks = 1:11, minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(0,1,.1), limits = c(0,1), labels=percent) +
  labs(y = "Variance explained", x = "Number of principal components")

# Psych package
politics_pca_1 <- principal(politics[, -1], nfactors = 2)
politics_pca_1
politics_pca_2 <- principal(politics[, -1], nfactors = 2)
politics_pca_2
summary(politics_pca_2)
plot(politics_pca_2, labels = colnames(politics[, -1]))
scores2 <- as.data.frame(politics_pca_2$scores)
scores2$Country <- politics$cntry
ggplot(scores2, aes(RC1, RC2, label = Country)) +
  geom_point() + geom_text(hjust = 1.2, vjust = 1)

politics_pca_3 <- principal(politics[, -1], nfactors = 3)
politics_pca_3
summary(politics_pca_3)
plot(politics_pca_3, labels = colnames(politics[, -1]))
scores3 <- as.data.frame(politics_pca_3$scores)
scores3$Country <- politics$cntry
ggplot(scores3, aes(RC1, RC2, label = Country)) +
  geom_point() + geom_text(hjust = 1.2, vjust = 1)


politics_pca_4 <- principal(politics[, -1], nfactors = 4)
politics_pca_4
summary(politics_pca_4)
plot(politics_pca_4, labels = colnames(politics[, -1]))
scores4 <- as.data.frame(politics_pca_4$scores)
scores4$Country <- politics$cntry
ggplot(scores4, aes(RC1, RC2, label = Country)) +
  geom_point() + geom_text(hjust = 1.2, vjust = 1)

# Probeersel voor mooie europese map :) http://egallic.fr/en/european-map-using-r/
source("functions/plotEurope.R")
# De landen zoals gebruik in het package "rworldmap" opgezocht in de volgorde van:
print(as.character(politics$cntry))
countryNames <- c("Belgium", "Bulgaria", "Switzerland", "Cyprus", "Czech Rep.", 
                  "Germany", "Denmark", "Estonia", "Spain", "Finland", "France", 
                  "United Kingdom", "Greece", "Croatia", "Hungary", "Israel", 
                  "Latvia", "Netherlands", "Norway", "Poland", "Portugal", "Romania", 
                  "Russia", "Sweden", "Slovenia","Slovakia", "Turkey", "Ukraine")
scores4$country_name <- countryNames
scores2$country_name <- countryNames
scores3$country_name <- countryNames


# test: which(!scores$country_name %in% worldMap$NAME)
countriesCoords <- getCountriesCoords(countryNames)

plotEurope(countriesCoords, scores2, 1, 2)
plotEurope(countriesCoords, scores2, 2, 2)
plotEurope(countriesCoords, scores3, 1, 3)
plotEurope(countriesCoords, scores3, 2, 3)
plotEurope(countriesCoords, scores3, 3, 3)
plotEurope(countriesCoords, scores4, 1, 4)
plotEurope(countriesCoords, scores4, 2, 4)
plotEurope(countriesCoords, scores4, 3, 4)
plotEurope(countriesCoords, scores4, 4, 4)

