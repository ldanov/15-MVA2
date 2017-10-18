#Download the csv file
income = read.csv(file ="income.csv" , sep = ";", head = TRUE)
head(income)

#Substitute Missing Information
income[income == ".."] = NA
head(income)

#Translate into numeric Data
income[,1] = sapply(income[,1], as.character)
income[,1] = sapply(income[,1], as.numeric)
income[,3:15] = sapply(income[,3:15], as.character)
income[,3:15] = sapply(income[,3:15], as.numeric)
head(income)

#Name the columns
income.names = c("HDI.rank", "country", "1980", "1985", "1990", "2000", "2005", "2006":"2013")
colnames(income) = income.names
head(income)

##Eliminate the non analyzed nations and create a row vector
g20 = factor(c("Argentina", "Australia", "Brazil", "Canada", "China", "France", "Germany", "India", "Indonesia", "Italy", "Japan", "Korea (South)", "Mexico", "Russia", "Saudi Arabia", "South Africa", "Turkey", "United Kingdom", "United States of America"))
income.g20 = subset(income, income$country == "Argentina" | income$country == "Australia" | income$country == "Brazil" | income$country == "Canada" | income$country == "China" | income$country == "France" | income$country == "Germany" | income$country == "India" | income$country == "Indonesia" | income$country =="Italy" | income$country == "Japan" | income$country == "Korea (Republic of)" | income$country == "Mexico" | income$country == "Russian Federation" | income$country == "Saudi Arabia" | income$country == "South Africa" | income$country == "Turkey" | income$country == "United Kingdom" | income$country == "United States")
head(income.g20)

#Sort alfabetically
income.g20 = income.g20[order(income.g20[,2], decreasing = FALSE),]

#determine the rows
rownames(income) = income[,2]
income[,1] = NULL

#summary statistics
summary(income.g20)
sd(income.g20[,2], na.rm=TRUE)
sd(income.g20[,3],na.rm=TRUE)
sd(income.g20[,4],na.rm=TRUE)
sd(income.g20[,5],na.rm=TRUE)
sd(income.g20[,6],na.rm=TRUE)
sd(income.g20[,7],na.rm=TRUE)
sd(income.g20[,8],na.rm=TRUE)
sd(income.g20[,9],na.rm=TRUE)

#determine the rows
rownames(income.g20) = g20
income.g20$country = NULL
income.g20$HDI.rank = NULL
head(income.g20)

# transposed frame
income.g20t = t(income.g20)
income.t = t(income)

#Determine the mean 
income[,1] = NULL
means.income.g20 = c(rowMeans(income.g20, na.rm=TRUE))
means.income.p = c(rowMeans(income, na.rm = TRUE))
means.income

library("matrixStats")
median.income.g20 = c(colMedians(income.g20t, na.rm = TRUE))
median.income.p = c(colMedians(income.t, na.rm = TRUE))
mm.income.df = data.frame(means.income.g20, median.income.g20)
mm.income.dfp = data.frame(means.income.p, median.income.p)

subset(mm.income.df, mm.income.df$means.income.g20 < mean(mm.income.df[,1]))
subset(mm.income.df, mm.income.df$means.income.g20 > mean(mm.income.df[,1]))
subset(mm.income.df, mm.income.df$median.income.g20 < median(mm.income.df[,2]))
subset(mm.income.df, mm.income.df$median.income.g20 > median(mm.income.df[,2]))

# graphics from ebor
hist(mm.income.df$means.income.g20, freq = FALSE, xlab = 'country means', main = NULL)
g = seq(min(mm.income.df$means.income.g20), max(mm.income.df$means.income.g20), length = 100)
dens.nrom = dnorm(g, mean(mm.income.df$means.income.g20), sd(mm.income.df$means.income.g20))
lines(g, dens.nrom, col = "red3", lwd=2)

#Summary statistics (raczej tu niepotrzebne, bo to wewn¹trz kraju)
summary(income.g20)
sd(mm.income.df[,1], na.rm=TRUE)
sd(mm.income.df[,2], na.rm=TRUE)

# Which are the countries with the lowest/highest values
# means
rownames(mm.income.df[which(mm.income.df[,1] == min(mm.income.df[,1])),])
rownames(mm.income.df[which(mm.income.df[,1] == max(mm.income.df[,1])),])

# medians
rownames(mm.income.df[which(mm.income.df[,2] == min(mm.income.df[,2])),])
rownames(mm.income.df[which(mm.income.df[,2] == max(mm.income.df[,2])),])

#KDE using gaussian Kernel
density(mm.income.df[,1])
plot(density(mm.income.df[,1]))
plot(density(mm.income.df[,2]))

#Emperical cdf
ecdf(mm.income.df[,1])
plot(ecdf(mm.income.df[,1]))
ecdf(mm.income.df[,2])
plot(ecdf(mm.income.df[,2]))

#Boxplot comparison
boxplot(mm.income.df[,1], col="yellow", varwith=TRUE, outline=TRUE)
boxplot(mm.income.df[,2], col="yellow", varwith=TRUE, outline=TRUE)

# correlation matrix (w tym przypadku raczej bez sensu)
cor.income.LE = cor(mm.income.df[,1], mm.income.df[,2])
cor.income.LE = cor(na.omit(mm.income.df))
cor.income.LE

#Covariances (here makes no sense)
cov(mm.income.df[,1],mm.income.df[,2])
plot(mm.income.df[,1]~mm.income.df[,2])

# Plot values for different countries
plot(means.income.g20, xaxt = "n", pch = 16, col = ifelse(means.income.g20 < mean(means.income.g20), "red", "green"), xlab = "Country", ylab = "income")
abline(mean(means.income), b = 0, col = "blue")
text(mean(means.income.g20), "mean", pos = 3)
text(means.income.g20, rownames(data.frame(means.income.g20)), cex = 0.7, pos = 1)
title("Countries with income above/below the mean")


#We now transfor the data in order to fitt it with the entrepreneurial data the operation is as follows.
1: #create a vector with the row names of the large matrix
  2: # Add this vector to the data frame
  3: # Create a subset with the countries that belong to the entrepreneurship data from the new vector
  4: # The dimention must be 100 if it is not 100 we check for differences
  5: # Check for diferences between the vector and our entrepreneurial data this differences are mostly because of the name
  6: # look for the countries with different names and change their name as it is in the Entrepreneurship data (This step must be done after step one)
  7: #leave outside Tonga Islands, Puerto Rico and Taiwan
  8: #Check that the new dimention is 100
  
vector.income = rownames(mm.income.dfp)
vector.income[64] = "Trinidad & Tobago"
vector.income[5] = "United States of America"
vector.income[113] = "Bolivia"
vector.income[86] = "Bosnia & Herzegovina"
vector.income[16] = "Hong Kong"
vector.income[75] = "Iran"
vector.income[15] = "Korea (South)"
vector.income[84] = "Macedonia"
vector.income[107] = "Palestine"
vector.income[57] = "Russia"
vector.income[37] = "Slovak Republic"
vector.income[118] = "Syria"
vector.income[121] = "Vietnam"
vector.income[67] = "Venezuela"

analysis.income = data.frame(mm.income.dfp, vector.income)
mm.income.a = subset(analysis.income, vector.income%in%rownames(oppo))
dim(mm.income.a)
head(mm.income.a)

Dif = setdiff(rownames(oppo), mm.income.a$vector.income)
Dif = Dif[order(Dif,decreasing = FALSE)]
Dif

# ALEJANDRO, shouldn't we also rename rownames? Isn't it all about this? I mean:
rownames(mm.income.a) = mm.income.a$"vector.income"
head(mm.income.a)
mm.income.a[,3] = NULL
head(mm.income.a)
