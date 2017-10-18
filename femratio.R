#Download the csv file
femratio = read.csv(file ="femratio.csv" , sep = ";", head = TRUE)
head(femratio)

#Substitute Missing Information
femratio[femratio==".."]=NA
head(femratio)

#Translate into numeric Data
femratio[,1] = sapply(femratio[,1], as.character)
femratio[,1] = sapply(femratio[,1], as.numeric)
femratio[,3:10] = sapply(femratio[,3:10], as.character)
femratio[,3:10] = sapply(femratio[,3:10], as.numeric)
head(femratio)

#Name the columns
femratio.names=c("HDI.rank", "country", "2000", "2005", "2008":"2013")
colnames(femratio)=femratio.names
head(femratio)

##Eliminate the non analyzed nations and create a row vector
g20 = factor(c("Argentina", "Australia", "Brazil", "Canada", "China", "France", "Germany", "India", "Indonesia", "Italy", "Japan", "Korea (South)", "Mexico", "Russia", "Saudi Arabia", "South Africa", "Turkey", "United Kingdom", "United States of America"))
femratio.g20 = subset(femratio, femratio$country == "Argentina" | femratio$country == "Australia" | femratio$country == "Brazil" | femratio$country == "Canada" | femratio$country == "China" | femratio$country == "France" | femratio$country == "Germany" | femratio$country == "India" | femratio$country == "Indonesia" | femratio$country =="Italy" | femratio$country == "Japan" | femratio$country == "Korea (Republic of)" | femratio$country == "Mexico" | femratio$country == "Russian Federation" | femratio$country == "Saudi Arabia" | femratio$country == "South Africa" | femratio$country == "Turkey" | femratio$country == "United Kingdom" | femratio$country == "United States")
head(femratio.g20)

#Sort alfabetically
femratio.g20 = femratio.g20[order(femratio.g20[,2], decreasing=FALSE),]

#determine the rows
rownames(femratio) = femratio[,2]
femratio[,1] = NULL
head(femratio)

#summary statistics
summary(femratio.g20)
sd(femratio.g20[,2],na.rm=TRUE)
sd(femratio.g20[,3],na.rm=TRUE)
sd(femratio.g20[,4],na.rm=TRUE)
sd(femratio.g20[,5],na.rm=TRUE)
sd(femratio.g20[,6],na.rm=TRUE)
sd(femratio.g20[,7],na.rm=TRUE)
sd(femratio.g20[,8],na.rm=TRUE)
sd(femratio.g20[,9],na.rm=TRUE)

#determine the rows
rownames(femratio.g20) = g20
femratio.g20$country = NULL
femratio.g20$HDI.rank = NULL
head(femratio.g20)
head(femratio)
femratio$"country"=NULL

# transposed frame
femratio.g20t = t(femratio.g20)
femratio.t=t(femratio)

#Determine the mean 
means.femratio = c(rowMeans(femratio.g20, na.rm=TRUE))
means.femratio.p=c(rowMeans(femratio, na.rm=TRUE))
means.femratio
means.femratio.p

install.packages("matrixStats")
library("matrixStats")
median.femratio=c(colMedians(femratio.g20t,na.rm=TRUE))
median.femratio.p=c(colMedians(femratio.t,  na.rm=TRUE))
mm.femratio.df = data.frame(means.femratio,median.femratio)
mm.femratio.dfp=data.frame(means.femratio.p,median.femratio.p)
mm.femratio.df
head(mm.femratio.dfp)

subset(mm.femratio.df,mm.femratio.df$means.femratio < mean(mm.femratio.df[,1]))
subset(mm.femratio.df,mm.femratio.df$means.femratio > mean(mm.femratio.df[,1]))
subset(mm.femratio.df,mm.femratio.df$median.femratio < median(mm.femratio.df[,2]))
subset(mm.femratio.df,mm.femratio.df$median.femratio > median(mm.femratio.df[,2]))

# graphics from ebor
hist(mm.femratio.df$means.femratio, freq = FALSE, xlab = 'country means', main = NULL)
g = seq(min(mm.femratio.df$means.femratio), max(mm.femratio.df$means.femratio), length = 100)
dens.nrom = dnorm(g,mean(mm.femratio.df$means.femratio), sd(mm.femratio.df$means.femratio))
lines(g, dens.nrom, col = "red3", lwd=2)

hist(mm.femratio.dfp$means.femratio.p, freq = FALSE, xlab = 'country means', main = NULL)
g = seq(min(mm.femratio.dfp$means.femratio.p, na.rm=TRUE), max(mm.femratio.dfp$means.femratio.p, na.rm=TRUE), length = 100)
dens.nrom = dnorm(g,mean(mm.femratio.dfp$means.femratio.p), sd(mm.femratio.dfp$means.femratio.p))
lines(g, dens.nrom, col = "red3", lwd=2)

#Summary statistics (raczej tu niepotrzebne, bo to wewn?trz kraju)
summary(femratio.g20)
sd(femratio.g20[,2], na.rm=TRUE)
sd(femratio.g20[,3], na.rm=TRUE)
sd(femratio.g20[,4], na.rm=TRUE)
sd(femratio.g20[,5], na.rm=TRUE)

# Which are the countries with the lowest/highest values
# means
rownames(mm.femratio.df[which(mm.femratio.df[,1] == min(mm.femratio.df[,1])),])
rownames(mm.femratio.df[which(mm.femratio.df[,1] == max(mm.femratio.df[,1])),])

# medians
rownames(mm.femratio.df[which(mm.femratio.df[,2] == max(mm.femratio.df[,2])),])
rownames(mm.femratio.df[which(mm.femratio.df[,2] == min(mm.femratio.df[,2])),])

#KDE using gaussian Kernel
density(mm.femratio.dfp[,1], na.rm=TRUE)
plot(density(mm.femratio.dfp[,1], na.rm=TRUE))
plot(density(mm.femratio.dfp[,2],na.rm=TRUE))

#Emperical cdf
ecdf(mm.femratio.dfp[,1])
plot(ecdf(mm.femratio.dfp[,1]))
ecdf(mm.femratio.dfp[,2])
plot(ecdf(mm.femratio.dfp[,2]))

#Boxplot comparison
boxplot(mm.femratio.df[,1], col="yellow", varwith=TRUE, outline=TRUE)
boxplot(mm.femratio.df[,2], col="yellow", varwith=TRUE, outline=TRUE)

# correlation matrix (w tym przypadku raczej bez sensu)
cor.femratio.LE = cor(mm.femratio.df[,1], mm.femratio.df[,2])
cor.femratio.LE = cor(na.omit(mm.femratio.df))
cor.femratio.LE

#Covariances (here makes no sense)
cov(mm.femratio.df[,1],mm.femratio.df[,2])
plot(mm.femratio.df[,1]~mm.femratio.df[,2])

# Plot values for different countries
plot(means.femratio, xaxt = "n", pch = 16, col = ifelse(means.femratio < mean(means.femratio), "red", "green"), xlab = "Country", ylab = "femratio")
abline(mean(means.femratio), b = 0, col = "blue")
text(mean(means.femratio), "mean", pos = 3)
text(means.femratio, rownames(data.frame(means.femratio)), cex = 0.7, pos = 1)
title("Countries with femratio above/below the mean")

#We now transfor the data in order to fitt it with the entrepreneurial data the operation is as follows.
1: #create a vector with the row names of the large matrix
2: # Add this vector to the data frame
3: # Create a subset with the countries that belong to the entrepreneurship data from the new vector
4: # The dimention must be 100 if it is not 100 we check for differences
5: # Check for diferences between the vector and our entrepreneurial data this differences are mostly because of the name
6: # look for the countries with different names and change their name as it is in the Entrepreneurship data (This step must be done after step one)
7: #leave outside Tonga Islands, Puerto Rico and Taiwan
8: #Check that the new dimention is 100
vector=rownames(mm.femratio.dfp)
vector[62]="Malaysia"
vector[64]="Trinidad & Tobago"
vector[5]="United States of America"
vector[113]="Bolivia"
vector[86]="Bosnia & Herzegovina"
vector[16]="Hong Kong"
vector[75]="Iran"
vector[15]= "Korea (South)"
vector[84]= "Macedonia" 
vector[107]="Palestine" 
vector[57]="Russia"
vector[38]="Slovak Republic" 
vector[118]="Syria"
vector[121]="Vietnam"
vector[67]="Venezuela"  
analysis.femratio=data.frame(mm.femratio.dfp, vector)
mm.femratio.a = subset(analysis.femratio, vector%in%rownames(oppo))
dim(mm.femratio.a)
head(mm.femratio.a)

Dif=setdiff(rownames(oppo), mm.femratio.a$vector)
Dif=Dif[order(Dif,decreasing=FALSE)]
Dif

mm.femratio.a[,3]=NULL

# males vs females plot
# create a vector of males ratio
means.maleratio = c(1 - means.femratio)
# create a matrix o male and female ratios
A = matrix(c(means.femratio, means.maleratio), ncol = 2)
# make a plot
barplot(t(A), col = ifelse(A == A[1,], "pink", "lightblue"), names.arg = rownames(data.frame(means.femratio)), las = 3, cex.names = 0.7, main = "male vs. female ratio of parliamentary seats")
abline(0.5, 0, col = "blue")

