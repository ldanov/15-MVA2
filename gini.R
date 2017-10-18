#Download the csv file
gini = read.csv(file ="gini.csv" , sep = ";", head = TRUE)
head(gini)

#Substitute Missing Information
gini[gini==".."]=NA
head(gini)

#Translate into numeric Data
gini[,1] = sapply(gini[,1], as.character)
gini[,1] = sapply(gini[,1], as.numeric)
gini[,3] = sapply(gini[,3], as.character)
gini[,3] = sapply(gini[,3], as.numeric)
head(gini)

#Name the columns
gini.names=c("HDI.rank", "country", "2013")
colnames(gini)=gini.names
head(gini)

##Eliminate the non analyzed nations and create a row vector
g20 = factor(c("Argentina", "Australia", "Brazil", "Canada", "China", "France", "Germany", "India", "Indonesia", "Italy", "Japan", "Korea (South)", "Mexico", "Russia", "Saudi Arabia", "South Africa", "Turkey", "United Kingdom", "United States of America"))
gini.g20 = subset(gini, gini$country == "Argentina" | gini$country == "Australia" | gini$country == "Brazil" | gini$country == "Canada" | gini$country == "China" | gini$country == "France" | gini$country == "Germany" | gini$country == "India" | gini$country == "Indonesia" | gini$country =="Italy" | gini$country == "Japan" | gini$country == "Korea (Republic of)" | gini$country == "Mexico" | gini$country == "Russian Federation" | gini$country == "Saudi Arabia" | gini$country == "South Africa" | gini$country == "Turkey" | gini$country == "United Kingdom" | gini$country == "United States")
head(gini.g20)

#Sort alfabetically
gini.g20 = gini.g20[order(gini.g20[,2], decreasing=FALSE),]

#determine the rows
rownames(gini) = gini[,2]
gini[,2] = NULL
gini[,1] = NULL
head(gini)

#summary statistics
summary(gini.g20)
sd(gini.g20[,1],na.rm=TRUE)
sd(gini.g20[,2],na.rm=TRUE)


#determine the rows
rownames(gini.g20) = g20
gini.g20$country = NULL
gini.g20$HDI.rank=NULL
head(gini.g20)

# transposed frame
gini.g20t = t(gini.g20)
gini.t=t(gini)

#Determine the mean 
means.gini = c(rowMeans(gini.g20, na.rm=TRUE))
means.gini.p=c(rowMeans(gini, na.rm=TRUE))
means.gini
means.gini.p

install.packages("matrixStats")
library("matrixStats")
median.gini=c(colMedians(gini.g20t,na.rm=TRUE))
median.gini.p=c(colMedians(gini.t,  na.rm=TRUE))
mm.gini.df = data.frame(means.gini,median.gini)
mm.gini.dfp=data.frame(means.gini.p,median.gini.p)
mm.gini.df
head(mm.gini.dfp)

subset(mm.gini.df,mm.gini.df$means.gini < mean(mm.gini.df[,1],na.rm=TRUE))
subset(mm.gini.df,mm.gini.df$means.gini > mean(mm.gini.df[,1], na.rm=TRUE))
subset(mm.gini.df,mm.gini.df$median.gini < median(mm.gini.df[,2], na.rm=TRUE))
subset(mm.gini.df,mm.gini.df$median.gini > median(mm.gini.df[,2], na.rm=TRUE))

# graphics from ebor
hist(mm.gini.df$means.gini, freq = FALSE, xlab = 'country means', main = NULL)
g = seq(min(mm.gini.df$means.gini), max(mm.gini.df$means.gini), length = 100)
dens.nrom = dnorm(g,mean(mm.gini.df$means.gini), sd(mm.gini.df$means.gini))
lines(g, dens.nrom, col = "red3", lwd=2)

hist(mm.gini.dfp$means.gini.p, freq = FALSE, xlab = 'country means', main = NULL)
g = seq(min(mm.gini.dfp$means.gini.p, na.rm=TRUE), max(mm.gini.dfp$means.gini.p, na.rm=TRUE), length = 100)
dens.nrom = dnorm(g,mean(mm.gini.dfp$means.gini.p), sd(mm.gini.dfp$means.gini.p))
lines(g, dens.nrom, col = "red3", lwd=2)

#Summary statistics (raczej tu niepotrzebne, bo to wewn?trz kraju)
summary(gini.g20)
sd(gini.g20[,1], na.rm=TRUE)

# Which are the countries with the lowest/highest values
# means
rownames(mm.gini.df[which(mm.gini.df[,1] == min(mm.gini.df[,1], na.rm=TRUE)),])
rownames(mm.gini.df[which(mm.gini.df[,1] == max(mm.gini.df[,1], na.rm=TRUE)),])

# medians
rownames(mm.gini.df[which(mm.gini.df[,2] == max(mm.gini.df[,2], na.rm=TRUE)),])
rownames(mm.gini.df[which(mm.gini.df[,2] == min(mm.gini.df[,2], na.rm=TRUE)),])

#KDE using gaussian Kernel
density(mm.gini.dfp[,1], na.rm=TRUE)
plot(density(mm.gini.dfp[,1], na.rm=TRUE))
plot(density(mm.gini.dfp[,2],na.rm=TRUE))

#Emperical cdf
ecdf(mm.gini.dfp[,1])
plot(ecdf(mm.gini.dfp[,1]))
ecdf(mm.gini.dfp[,2])
plot(ecdf(mm.gini.dfp[,2]))

#Boxplot comparison
boxplot(mm.gini.df[,1], col="yellow", varwith=TRUE, outline=TRUE)
boxplot(mm.gini.df[,2], col="yellow", varwith=TRUE, outline=TRUE)

# Plot values for different countries
plot(means.gini, xaxt = "n", pch = 16, col = ifelse(means.gini < mean(means.gini), "red", "green"), xlab = "Country", ylab = "gini")
abline(mean(means.gini), b = 0, col = "blue") ??
text(mean(means.gini), "mean", pos = 3)
text(means.gini, rownames(data.frame(means.gini)), cex = 0.7, pos = 1)
title("Countries with gini above/below the mean")

#We now transfor the data in order to fitt it with the entrepreneurial data the operation is as follows.
1: #create a vector with the row names of the large matrix
  2: # Add this vector to the data frame
  3: # Create a subset with the countries that belong to the entrepreneurship data from the new vector
  4: # The dimention must be 100 if it is not 100 we check for differences
  5: # Check for diferences between the vector and our entrepreneurial data this differences are mostly because of the name
  6: # look for the countries with different names and change their name as it is in the Entrepreneurship data (This step must be done after step one)
  7: #leave outside Tonga Islands, Puerto Rico and Taiwan
  8: #Check that the new dimention is 100
vector=rownames(mm.gini.dfp)
vector[67]
vector[62]="Malaysia"
vector[64]="Trinidad & Tobago"
vector[5]="United States of America"
vector[113]="Bolivia"
vector[86]="Bosnia & Herzegovina"
vector[15]="Hong Kong"
vector[75]="Iran"
vector[16]= "Korea (South)"
vector[85]= "Macedonia" 
vector[107]="Palestine" 
vector[57]="Russia"
vector[38]="Slovak Republic" 
vector[119]="Syria"
vector[122]="Vietnam"
vector[67]="Venezuela"  
analysis.gini=data.frame(mm.gini.dfp, vector)
mm.gini.a = subset(analysis.gini, vector%in%rownames(oppo))
dim(mm.gini.a)
head(mm.gini.a)
mm.gini.dfp
Dif=setdiff(rownames(oppo), mm.gini.a$vector)
Dif=Dif[order(Dif,decreasing=FALSE)]
Dif
vector

mm.gini.a[,3]=NULL


