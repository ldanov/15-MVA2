#Download the csv file
gender = read.csv(file ="gender.csv" , sep = ";", head = TRUE)
head(gender)

#Substitute Missing Information
gender[gender == ".."] = NA
head(gender)

#Translate into numeric Data
gender[,1] = sapply(gender[,1], as.character)
gender[,1] = sapply(gender[,1], as.numeric)
gender[,3:6] = sapply(gender[,3:6], as.character)
gender[,3:6] = sapply(gender[,3:6], as.numeric)
head(gender)

#Name the columns
gender.names = c("HDI.rank", "country", "2000", "2005", "2010", "2013")
colnames(gender) = gender.names
head(gender)

##Eliminate the non analyzed nations and create a row vector
g20 = factor(c("Argentina", "Australia", "Brazil", "Canada", "China", "France", "Germany", "India", "Indonesia", "Italy", "Japan", "Korea (South)", "Mexico", "Russia", "Saudi Arabia", "South Africa", "Turkey", "United Kingdom", "United States of America"))
gender.g20 = subset(gender, gender$country == "Argentina" | gender$country == "Australia" | gender$country == "Brazil" | gender$country == "Canada" | gender$country == "China" | gender$country == "France" | gender$country == "Germany" | gender$country == "India" | gender$country == "Indonesia" | gender$country =="Italy" | gender$country == "Japan" | gender$country == "Korea (Republic of)" | gender$country == "Mexico" | gender$country == "Russian Federation" | gender$country == "Saudi Arabia" | gender$country == "South Africa" | gender$country == "Turkey" | gender$country == "United Kingdom" | gender$country == "United States")
head(gender.g20)

#Sort alfabetically
gender.g20 = gender.g20[order(gender.g20[,2], decreasing = FALSE),]

#determine the rows
rownames(gender) = gender[,2]
gender[,1] = NULL

#summary statistics
summary(gender.g20)
sd(gender.g20[,2], na.rm=TRUE)
sd(gender.g20[,3],na.rm=TRUE)
sd(gender.g20[,4],na.rm=TRUE)
sd(gender.g20[,5],na.rm=TRUE)
sd(gender.g20[,6],na.rm=TRUE)

#determine the rows
rownames(gender.g20) = g20
gender.g20$country = NULL
gender.g20$HDI.rank = NULL
head(gender.g20)

# transposed frame
gender.g20t = t(gender.g20)

#Determine the mean 
means.gender = c(rowMeans(gender.g20, na.rm=TRUE))
means.gender

install.packages("matrixStats")
library("matrixStats")
median.gender = c(colMedians(gender.g20t, na.rm = TRUE))
mm.gender.df = data.frame(means.gender, median.gender)
mm.gender.df

subset(mm.gender.df, mm.gender.df$means.gender < mean(mm.gender.df[,1]))
subset(mm.gender.df, mm.gender.df$means.gender > mean(mm.gender.df[,1]))
subset(mm.gender.df, mm.gender.df$median.gender < median(mm.gender.df[,2]))
subset(mm.gender.df, mm.gender.df$median.gender > median(mm.gender.df[,2]))

# graphics from ebor
hist(mm.gender.df$means.gender, freq = FALSE, xlab = 'country means', main = NULL)
g = seq(min(mm.gender.df$means.gender), max(mm.gender.df$means.gender), length = 100)
dens.nrom = dnorm(g, mean(mm.gender.df$means.gender), sd(mm.gender.df$means.gender))
lines(g, dens.nrom, col = "red3", lwd=2)

#Summary statistics (raczej tu niepotrzebne, bo to wewn¹trz kraju)
summary(gender.g20)
sd(mm.gender.df[,1], na.rm=TRUE)
sd(mm.gender.df[,2], na.rm=TRUE)

# Which are the countries with the lowest/highest values
# means
rownames(mm.gender.df[which(mm.gender.df[,1] == min(mm.gender.df[,1])),])
rownames(mm.gender.df[which(mm.gender.df[,1] == max(mm.gender.df[,1])),])

# medians
rownames(mm.gender.df[which(mm.gender.df[,2] == min(mm.gender.df[,2])),])
rownames(mm.gender.df[which(mm.gender.df[,2] == max(mm.gender.df[,2])),])

#KDE using gaussian Kernel
density(mm.gender.df[,1])
plot(density(mm.gender.df[,1]))
plot(density(mm.gender.df[,2]))

#Emperical cdf
ecdf(mm.gender.df[,1])
plot(ecdf(mm.gender.df[,1]))
ecdf(mm.gender.df[,2])
plot(ecdf(mm.gender.df[,2]))

#Boxplot comparison
boxplot(mm.gender.df[,1], col="yellow", varwith=TRUE, outline=TRUE)
boxplot(mm.gender.df[,2], col="yellow", varwith=TRUE, outline=TRUE)

# correlation matrix (w tym przypadku raczej bez sensu)
cor.gender.LE = cor(mm.gender.df[,1], mm.gender.df[,2])
cor.gender.LE = cor(na.omit(mm.gender.df))
cor.gender.LE

#Covariances (here makes no sense)
cov(mm.gender.df[,1], mm.gender.df[,2])
plot(mm.gender.df[,1] ~ mm.gender.df[,2])

# Plot values for different countries
plot(means.gender, xaxt = "n", pch = 16, col = ifelse(means.gender < mean(means.gender), "red", "green"), xlab = "Country", ylab = "gender")
abline(mean(means.gender), b = 0, col = "blue")
text(mean(means.gender), "mean", pos = 1, col = "blue")
text(means.gender, rownames(data.frame(means.gender)), cex = 0.7, pos = 1)
title("Countries with gender above/below the mean")