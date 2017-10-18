#established business ownership rates (import data)
nascent = read.csv(file = "nascent.csv", sep = ";", head = TRUE)
head(nascent)
# let's change the "-" to NAs
nascent[nascent=="-"] = NA

# what is the class of variables? Can we perform any analysis?
is.numeric(nascent[,2]) # not numeric so we have to transform them in order to do anything
nascent[,2:15] = sapply(nascent[,2:15], as.character)
nascent[,2:15] = sapply(nascent[,2:15], as.numeric)
is.numeric(nascent[,2])

# let's name the columns nicely
nascent.names = c("country", "2001":"2014")
colnames(nascent) = nascent.names

# let's try to extract the g20 countries
g20 = factor(c("Argentina", "Australia", "Brazil", "Canada", "China", "France", "Germany", "India", "Indonesia", "Italy", "Japan", "Korea (South)", "Mexico", "Russia", "Saudi Arabia", "South Africa", "Turkey", "United Kingdom", "United States of America"))
nascent.g20 = subset(nascent,  nascent$country == "Argentina" | nascent$country == "Australia" | nascent$country == "Brazil" | nascent$country == "China" | nascent$country == "France" | nascent$country == "India" | nascent$country == "Indonesia" | nascent$country == "Japan" | nascent$country =="Canada" | nascent$country == "Korea (South)" | nascent$country == "Mexico" | nascent$country == "Germany" |nascent$country == "Saudi Arabia" | nascent$country == "South Africa" | nascent$country == "Russia" | nascent$country == "United States of America" | nascent$country == "Turkey" | nascent$country == "United Kingdom" | nascent$country == "Italy")
# it's an awful command but works, must do for now
head(nascent.g20)
# let's name the rows nicely
rownames(nascent.g20) = g20

rownames(nascent)=nascent$"country"
nascent$country=NULL
head(nascent)

#let's transpose
nascent.g20t = t(nascent.g20)

# remove the first column
nascent.g20$country = NULL
nascent.g20

# means per country
nascent.g20t = t(nascent.g20)
nascent.t=t(nascent)
means.nascent = c(rowMeans(nascent.g20, na.rm = TRUE))
means.nascent.p=c(rowMeans(nascent, na.rm=TRUE))
head(means.nascent.p)
# install matrixStats package
# install.packages("matrixStats")
library("matrixStats")

#
median.nascent = c(colMedians(nascent.g20t, na.rm = TRUE))
median.nascent.p=c(colMedians(nascent.t, na.rm=TRUE))
# create an mm dataframe
mm.nascent.df = data.frame(means.nascent, median.nascent)
mm.nascent.dfp= data.frame(means.nascent.p, median.nascent.p)
# subset the countries by the mean
subset(mm.nascent.df, mm.nascent.df$means.nascent < mean(mm.nascent.df[,1]))
subset(mm.nascent.df, mm.nascent.df$means.nascent > mean(mm.nascent.df[,1]))

# subset the countries by the median
subset(mm.nascent.df, mm.nascent.df$median.nascent < median(mm.nascent.df[,2]))
subset(mm.nascent.df, mm.nascent.df$median.nascent > median(mm.nascent.df[,2]))

# which country has the lowest/highest mean
rownames(mm.nascent.df[which(mm.nascent.df[,1] == min(mm.nascent.df[,1])),])
rownames(mm.nascent.df[which(mm.nascent.df[,1] == max(mm.nascent.df[,1])),])

# which country has the lowest/highest median
rownames(mm.nascent.df[which(mm.nascent.df[,2] == min(mm.nascent.df[,2])),])
rownames(mm.nascent.df[which(mm.nascent.df[,2] == max(mm.nascent.df[,2])),])

# plot means
plot(g20, mm.nascent.df[,1])

# is the data normally distributed?
hist(mm.nascent.df$means.nascent, freq = FALSE, xlab = 'country means', main = NULL)
g = seq(na.omit(min(mm.nascent.df$means.nascent), max(mm.nascent.df$means.nascent)), length=100) #Create a grid
dens.norm = dnorm(g, mean(mm.nascent.dfp$means.nascent.p), sd(mm.nascent.dfp$means.nascent.p)) #Compute the normal density on g
lines(g, dens.norm, col="red3", lwd=2)

hist(mm.nascent.dfp$means.nascent.p, freq = FALSE, xlab = 'country means', main = NULL)
g = seq(min(mm.nascent.dfp$means.nascent.p,na.rm = TRUE), max(mm.nascent.dfp$means.nascent.p,na.rm = TRUE), length=100) #Create a grid
dens.norm = dnorm(g, mean(mm.nascent.df$means.nascent), sd(mm.nascent.df$means.nascent)) #Compute the normal density on g
lines(g, dens.norm, col="red3", lwd=2)

#KDE using gaussian Kernel
mm.nascent.df
density(mm.nascent.dfp[,1],na.rm=TRUE)
plot(density(mm.nascent.dfp[,1],na.rm=TRUE))
#Emperical cdf
ecdf(mm.nascent.dfp[,1])
plot(ecdf(mm.nascent.dfp[,1]))
#Mean characteristics and measures of the sample
summary(mm.nascent.dfp[,1]) 
boxplot(mm.nascent.dfp[,1])
mm.nascent.dfp
mm.oppo.dfp

mm.nascent.a=mm.nascent.dfp[-c(89,91,74),]

mm.nascent.a
which(rownames(mm.nascent.dfp)=="Taiwan")
which(rownames(mm.nascent.dfp)=="Tonga Islands")
which(rownames(mm.nascent.dfp)=="Puerto Rico")
