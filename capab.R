#established business ownership rates (import data)
capab = read.csv(file = "capab.csv", sep = ";", head = TRUE)
head(capab)
# let's change the "-" to NAs
capab[capab=="-"] = NA

# what is the class of variables? Can we perform any analysis?
is.numeric(capab[,2]) # not numeric so we have to transform them in order to do anything
capab[,2:15] = sapply(capab[,2:15], as.character)
capab[,2:15] = sapply(capab[,2:15], as.numeric)
is.numeric(capab[,2])

# let's name the columns nicely
capab.names = c("country", "2001":"2014")
colnames(capab) = capab.names

# let's try to extract the g20 countries
g20 = factor(c("Argentina", "Australia", "Brazil", "Canada", "China", "France", "Germany", "India", "Indonesia", "Italy", "Japan", "Korea (South)", "Mexico", "Russia", "Saudi Arabia", "South Africa", "Turkey", "United Kingdom", "United States of America"))
capab.g20 = subset(capab,  capab$country == "Argentina" | capab$country == "Australia" | capab$country == "Brazil" | capab$country == "China" | capab$country == "France" | capab$country == "India" | capab$country == "Indonesia" | capab$country == "Japan" | capab$country =="Canada" | capab$country == "Korea (South)" | capab$country == "Mexico" | capab$country == "Germany" |capab$country == "Saudi Arabia" | capab$country == "South Africa" | capab$country == "Russia" | capab$country == "United States of America" | capab$country == "Turkey" | capab$country == "United Kingdom" | capab$country == "Italy")
# it's an awful command but works, must do for now
head(capab.g20)
# let's name the rows nicely
rownames(capab.g20) = g20

rownames(capab)=capab$"country"
capab$country=NULL
head(capab)

#let's transpose
capab.g20t = t(capab.g20)

# remove the first column
capab.g20$country = NULL
capab.g20

# means per country
capab.g20t = t(capab.g20)
capab.t=t(capab)
means.capab = c(rowMeans(capab.g20, na.rm = TRUE))
means.capab.p=c(rowMeans(capab, na.rm=TRUE))
head(means.capab.p)
# install matrixStats package
# install.packages("matrixStats")
library("matrixStats")

#
median.capab = c(colMedians(capab.g20t, na.rm = TRUE))
median.capab.p=c(colMedians(capab.t, na.rm=TRUE))
# create an mm dataframe
mm.capab.df = data.frame(means.capab, median.capab)
mm.capab.dfp= data.frame(means.capab.p, median.capab.p)
# subset the countries by the mean
subset(mm.capab.df, mm.capab.df$means.capab < mean(mm.capab.df[,1]))
subset(mm.capab.df, mm.capab.df$means.capab > mean(mm.capab.df[,1]))

# subset the countries by the median
subset(mm.capab.df, mm.capab.df$median.capab < median(mm.capab.df[,2]))
subset(mm.capab.df, mm.capab.df$median.capab > median(mm.capab.df[,2]))

# which country has the lowest/highest mean
rownames(mm.capab.df[which(mm.capab.df[,1] == min(mm.capab.df[,1])),])
rownames(mm.capab.df[which(mm.capab.df[,1] == max(mm.capab.df[,1])),])

# which country has the lowest/highest median
rownames(mm.capab.df[which(mm.capab.df[,2] == min(mm.capab.df[,2])),])
rownames(mm.capab.df[which(mm.capab.df[,2] == max(mm.capab.df[,2])),])

# plot means
plot(g20, mm.capab.df[,1])

# is the data normally distributed?
hist(mm.capab.df$means.capab, freq = FALSE, xlab = 'country means', main = NULL)
g = seq(min(mm.capab.df$means.capab, na.rm=TRUE), max(mm.capab.df$means.capab, na.rm=TRUE), length=100) #Create a grid
dens.norm = dnorm(g, mean(mm.capab.dfp$means.capab.p), sd(mm.capab.dfp$means.capab.p)) #Compute the normal density on g
lines(g, dens.norm, col="red3", lwd=2)

hist(mm.capab.dfp$means.capab.p, freq = FALSE, xlab = 'country means', main = NULL)
g = seq(min(mm.capab.dfp$means.capab.p,na.rm = TRUE), max(mm.capab.dfp$means.capab.p,na.rm = TRUE), length=100) #Create a grid
dens.norm = dnorm(g, mean(mm.capab.df$means.capab), sd(mm.capab.df$means.capab)) #Compute the normal density on g
lines(g, dens.norm, col="red3", lwd=2)

#KDE using gaussian Kernel
mm.capab.df
density(mm.capab.dfp[,1],na.rm=TRUE)
plot(density(mm.capab.dfp[,1],na.rm=TRUE))
#Emperical cdf
ecdf(mm.capab.dfp[,1])
plot(ecdf(mm.capab.dfp[,1]))
#Mean characteristics and measures of the sample
summary(mm.capab.dfp[,1]) 
boxplot(mm.capab.dfp[,1])
mm.capab.dfp
mm.oppo.dfp

mm.capab.a=mm.capab.dfp[-c(89,91,74),]

mm.capab.a
which(rownames(mm.capab.dfp)=="Barbados")
which(rownames(mm.capab.dfp)=="Tonga Islands")
which(rownames(mm.capab.dfp)=="Puerto Rico")

dim(mm.capab.a)