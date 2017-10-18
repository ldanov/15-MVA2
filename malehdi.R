#Download the csv file
malehdi = read.csv(file ="malehdi.csv" , sep = ";", head = TRUE)

#Substitute Missing Information
malehdi[malehdi == ".."] = NA

#Translate into numeric Data
malehdi[,1] = sapply(malehdi[,1], as.character)
malehdi[,1] = sapply(malehdi[,1], as.numeric)
malehdi[,3] = sapply(malehdi[,3], as.character)
malehdi[,3] = sapply(malehdi[,3], as.numeric)

#Name the columns
malehdi.names = c("HDI.rank","country","value")
colnames(malehdi) = malehdi.names
head(malehdi)

##Eliminate the non analyzed nations and create a row vector
g20 = factor(c("Argentina", "Australia", "Brazil", "Canada", "China", "France", "Germany", "India", "Indonesia", "Italy", "Japan", "Korea (South)", "Mexico", "Russia", "Saudi Arabia", "South Africa", "Turkey", "United Kingdom", "United States of America"))
malehdi.g20 = subset(malehdi, malehdi$country == "Argentina" | malehdi$country == "Australia" | malehdi$country == "Brazil" | malehdi$country == "Canada" | malehdi$country == "China" | malehdi$country == "France" | malehdi$country == "Germany" | malehdi$country == "India" | malehdi$country == "Indonesia" | malehdi$country =="Italy" | malehdi$country == "Japan" | malehdi$country == "Korea (Republic of)" | malehdi$country == "Mexico" | malehdi$country == "Russian Federation" | malehdi$country == "Saudi Arabia" | malehdi$country == "South Africa" | malehdi$country == "Turkey" | malehdi$country == "United Kingdom" | malehdi$country == "United States")

#determine the rows
malehdi.g20 = malehdi.g20[order(malehdi.g20[,2], decreasing=FALSE),]
rownames(malehdi.g20) = g20
malehdi.g20$country=NULL

#Summary statistics
summary(malehdi.g20)
sd(malehdi.g20[,2], na.rm = TRUE)

#Transposed frame
malehdi.g20t=t(malehdi.g20)

#Compare Data
subset(malehdi.g20, malehdi.g20$value < mean(malehdi.g20[,2], na.rm = TRUE))
subset(malehdi.g20, malehdi.g20$value > mean(malehdi.g20[,2], na.rm = TRUE))
subset(malehdi.g20, malehdi.g20$value < median(malehdi.g20[,2], na.rm = TRUE))
subset(malehdi.g20, malehdi.g20$value > median(malehdi.g20[,2], na.rm = TRUE))

# Determination of the values distribution
malehdi.g18 = malehdi.g20[-c(3, 16),]
hist(malehdi.g18$value, freq = FALSE, xlab = 'Male HDI', main = "Distribution of male HDI in G20 countries")
g = seq(min(malehdi.g18$value), max(malehdi.g18$value), length = 100)
dens.nrom = dnorm(g, mean(malehdi.g18$value), sd(malehdi.g18$value))
lines(g, dens.nrom, col = "red3", lwd = 2)

#Which are the countries with the lowest/highest values
rownames(malehdi.g20[which(malehdi.g20[,2] == max(malehdi.g20[,2], na.rm = TRUE)),])
rownames(malehdi.g20[which(malehdi.g20[,2] == min(malehdi.g20[,2], na.rm = TRUE)),])

#KDE using gaussian Kernel
density(malehdi.g20[,2], na.rm = TRUE)
plot(density(gini.g20[,2], na.rm = TRUE))

#Emperical cdf
ecdf(malehdi.g20[,2])
plot(ecdf(malehdi.g20[,2]))

#Mean characteristics and measures of the sample
summary(malehdi.g20[,2]) 
boxplot(malehdi.g20[,2])

#Correlation Matrix (po co to tutaj?) 
cor.malehdi.LE = cor(na.omit(malehdi.g20))
cor.malehdi.LE

#Covariances 
cov(malehdi.g18[,1], malehdi.g18[,2])
plot(malehdi.g20[,1] ~ gini.g20[,2], main="Covariance of gender male HDI in G20 countries and HDI", xlab="gini.g20uality in life expectancy", ylab="HDI")

#Regression
fit = lm(gini.g20[,1] ~ gini.g20[,2])
fit

# Plot values for different countries
plot(malehdi.g20[,2], xaxt = "n", pch = 16, col = ifelse(malehdi.g20$value > mean(malehdi.g20[,2], na.rm = TRUE) , "green", "red"))
abline(mean(malehdi.g20$value, na.rm = TRUE), b = 0, col = "blue")
text(mean(malehdi.g20$value, na.rm = TRUE), "mean", pos = 3)
text(malehdi.g20[,2], rownames(malehdi.g20), cex = 0.7, pos = 1)
title("Countries with malehdi above/below the mean")