setwd("~/Desktop/854datasets")
university.df <- read.csv("Universities.csv", header = TRUE)
# problem 4.2
# part a removing all categorical variables and missing data
university.df <- university.df[, -1] #remove categorical variable university name
university.df <- university.df[, -1] #remove categorical variable state name
university.df <- university.df[, -1] #remove categorical variable private or public 
university.clean.df <- university.df[complete.cases(university.df), ] #remove records with missing data

# part b conducting a PCA
pcs.cor <- prcomp(university.clean.df, scale. = T)
summary(pcs.cor)
screeplot(pcs.cor, npcs=17, type="lines")
screeplot(pcs.cor, npcs=17, type="barplot")
# the barplot and line plot shows us that the most variation can be explained by the first two principal components, but in order to explain a proper amount of variation (about 88%) we could use the first 8 principle components
# PC 1 explains 30.4% of the variation and PC2 explains 27% of the variation
pcs.cor$rot[,1:8]
# when I ran the PCA I normalized the data because I felt that variables like student to faculty ratio or graduation rate would be dominated by other variables like tuition or room and board
# the pcs.cor$rot using the standardized data appears to be proper and none of the variables appear too large or small, so I believe standardizing was the correct option
# in other words, the weights of the variables are similar for all of the principal components because of the standardization
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
ggbiplot(pcs.cor)
# attempt at grouping in ggbiplot
university.clean.df$RATE <- na.omit(ifelse(university.clean.df$Graduation.rate>mean(university.clean.df$Graduation.rate), "High Grad", "Low Grad"))
ggbiplot(pcs.cor, ellipse=TRUE, labels=rownames(university.clean.df), groups = university.clean.df$RATE)

# I tried looking at the grouping based off of graduation rate. It appears that higher cost, teachers with PHd's, and the number of students can account for the higher graduation rate group. Higher student to facutly ratio, estimated personal, appears to account for the lower graduation rate group. Things like accepted applicants or recorded applications don't appear to account for either grouping



