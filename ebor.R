#established business ownership rates (import data)
ebor = read.csv(file = "ebor.csv", sep = ";", head = TRUE)
head(ebor)
# let's change the "-" to NAs
ebor[ebor=="-"] = NA

# what is the class of variables? Can we perform any analysis?
is.numeric(ebor[,2]) # not numeric so we have to transform them in order to do anything
ebor[,2:15] = sapply(ebor[,2:15], as.character)
ebor[,2:15] = sapply(ebor[,2:15], as.numeric)
is.numeric(ebor[,2])

# let's name the columns nicely
ebor.names = c("country", "2001":"2014")
colnames(ebor) = ebor.names

# let's try to extract the g20 countries
g20 = factor(c("Argentina", "Australia", "Brazil", "Canada", "China", "France", "Germany", "India", "Indonesia", "Italy", "Japan", "Korea (South)", "Mexico", "Russia", "Saudi Arabia", "South Africa", "Turkey", "United Kingdom", "United States of America"))
ebor.g20 = subset(ebor,  ebor$country == "Argentina" | ebor$country == "Australia" | ebor$country == "Brazil" | ebor$country == "China" | ebor$country == "France" | ebor$country == "India" | ebor$country == "Indonesia" | ebor$country == "Japan" | ebor$country =="Canada" | ebor$country == "Korea (South)" | ebor$country == "Mexico" | ebor$country == "Germany" |ebor$country == "Saudi Arabia" | ebor$country == "South Africa" | ebor$country == "Russia" | ebor$country == "United States of America" | ebor$country == "Turkey" | ebor$country == "United Kingdom" | ebor$country == "Italy")
# it's an awful command but works, must do for now
head(ebor.g20)
# let's name the rows nicely
rownames(ebor.g20) = g20

rownames(ebor)=ebor$"country"
ebor$country=NULL
head(ebor)

#let's transpose
ebor.g20t = t(ebor.g20)

# remove the first column
ebor.g20$country = NULL
ebor.g20

# means per country
ebor.g20t = t(ebor.g20)
ebor.t=t(ebor)
means.ebor = c(rowMeans(ebor.g20, na.rm = TRUE))
means.ebor.p=c(rowMeans(ebor, na.rm=TRUE))
head(means.ebor.p)
# install matrixStats package
# install.packages("matrixStats")
library("matrixStats")

#
median.ebor = c(colMedians(ebor.g20t, na.rm = TRUE))
median.ebor.p=c(colMedians(ebor.t, na.rm=TRUE))
# create an mm dataframe
mm.ebor.df = data.frame(means.ebor, median.ebor)
mm.ebor.dfp= data.frame(means.ebor.p, median.ebor.p)
# subset the countries by the mean
subset(mm.ebor.df, mm.ebor.df$means.ebor < mean(mm.ebor.df[,1]))
subset(mm.ebor.df, mm.ebor.df$means.ebor > mean(mm.ebor.df[,1]))

# subset the countries by the median
subset(mm.ebor.df, mm.ebor.df$median.ebor < median(mm.ebor.df[,2]))
subset(mm.ebor.df, mm.ebor.df$median.ebor > median(mm.ebor.df[,2]))

# which country has the lowest/highest mean
rownames(mm.ebor.df[which(mm.ebor.df[,1] == min(mm.ebor.df[,1])),])
rownames(mm.ebor.df[which(mm.ebor.df[,1] == max(mm.ebor.df[,1])),])

# which country has the lowest/highest median
rownames(mm.ebor.df[which(mm.ebor.df[,2] == min(mm.ebor.df[,2])),])
rownames(mm.ebor.df[which(mm.ebor.df[,2] == max(mm.ebor.df[,2])),])

# plot means
plot(g20, mm.ebor.df[,1])

# is the data normally distributed?
hist(mm.ebor.df$means.ebor, freq = FALSE, xlab = 'country means', main = NULL)
g = seq(na.omit(min(mm.ebor.df$means.ebor), max(mm.ebor.df$means.ebor)), length=100) #Create a grid
dens.norm = dnorm(g, mean(mm.ebor.dfp$means.ebor.p), sd(mm.ebor.dfp$means.ebor.p)) #Compute the normal density on g
lines(g, dens.norm, col="red3", lwd=2)

hist(mm.ebor.dfp$means.ebor.p, freq = FALSE, xlab = 'country means', main = NULL)
g = seq(min(mm.ebor.dfp$means.ebor.p,na.rm = TRUE), max(mm.ebor.dfp$means.ebor.p,na.rm = TRUE), length=100) #Create a grid
dens.norm = dnorm(g, mean(mm.ebor.df$means.ebor), sd(mm.ebor.df$means.ebor)) #Compute the normal density on g
lines(g, dens.norm, col="red3", lwd=2)

#KDE using gaussian Kernel
mm.ebor.df
density(mm.ebor.dfp[,1],na.rm=TRUE)
plot(density(mm.ebor.dfp[,1],na.rm=TRUE))
#Emperical cdf
ecdf(mm.ebor.dfp[,1])
plot(ecdf(mm.ebor.dfp[,1]))
#Mean characteristics and measures of the sample
summary(mm.ebor.dfp[,1]) 
boxplot(mm.ebor.dfp[,1])
mm.ebor.dfp
mm.oppo.dfp

mm.ebor.a=mm.ebor.dfp[-c(89,91,74,101),]

mm.ebor.a
which(rownames(mm.ebor.dfp)=="Taiwan")
which(rownames(mm.ebor.dfp)=="Tonga Islands")
which(rownames(mm.ebor.dfp)=="Puerto Rico")

dim(mm.ebor.a)

