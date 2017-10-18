#established business ownership rates (import data)
necessity = read.csv(file = "necessity.csv", sep = ";", head = TRUE)
head(necessity)
# let's change the "-" to NAs
necessity[necessity=="-"] = NA

# what is the class of variables? Can we perform any analysis?
is.numeric(necessity[,2]) # not numeric so we have to transform them in order to do anything
necessity[,2:15] = sapply(necessity[,2:15], as.character)
necessity[,2:15] = sapply(necessity[,2:15], as.numeric)
is.numeric(necessity[,2])

# let's name the columns nicely
necessity.names = c("country", "2001":"2014")
colnames(necessity) = necessity.names

# let's try to extract the g20 countries
g20 = factor(c("Argentina", "Australia", "Brazil", "Canada", "China", "France", "Germany", "India", "Indonesia", "Italy", "Japan", "Korea (South)", "Mexico", "Russia", "Saudi Arabia", "South Africa", "Turkey", "United Kingdom", "United States of America"))
necessity.g20 = subset(necessity,  necessity$country == "Argentina" | necessity$country == "Australia" | necessity$country == "Brazil" | necessity$country == "China" | necessity$country == "France" | necessity$country == "India" | necessity$country == "Indonesia" | necessity$country == "Japan" | necessity$country =="Canada" | necessity$country == "Korea (South)" | necessity$country == "Mexico" | necessity$country == "Germany" |necessity$country == "Saudi Arabia" | necessity$country == "South Africa" | necessity$country == "Russia" | necessity$country == "United States of America" | necessity$country == "Turkey" | necessity$country == "United Kingdom" | necessity$country == "Italy")
# it's an awful command but works, must do for now
head(necessity.g20)
# let's name the rows nicely
rownames(necessity.g20) = g20

rownames(necessity)=necessity$"country"
necessity$country=NULL
head(necessity)

#let's transpose
necessity.g20t = t(necessity.g20)

# remove the first column
necessity.g20$country = NULL
necessity.g20

# means per country
necessity.g20t = t(necessity.g20)
necessity.t=t(necessity)
means.necessity = c(rowMeans(necessity.g20, na.rm = TRUE))
means.necessity.p=c(rowMeans(necessity, na.rm=TRUE))
head(means.necessity.p)
# install matrixStats package
# install.packages("matrixStats")
library("matrixStats")

#
median.necessity = c(colMedians(necessity.g20t, na.rm = TRUE))
median.necessity.p=c(colMedians(necessity.t, na.rm=TRUE))
# create an mm dataframe
mm.necessity.df = data.frame(means.necessity, median.necessity)
mm.necessity.dfp= data.frame(means.necessity.p, median.necessity.p)
# subset the countries by the mean
subset(mm.necessity.df, mm.necessity.df$means.necessity < mean(mm.necessity.df[,1]))
subset(mm.necessity.df, mm.necessity.df$means.necessity > mean(mm.necessity.df[,1]))

# subset the countries by the median
subset(mm.necessity.df, mm.necessity.df$median.necessity < median(mm.necessity.df[,2]))
subset(mm.necessity.df, mm.necessity.df$median.necessity > median(mm.necessity.df[,2]))

# which country has the lowest/highest mean
rownames(mm.necessity.df[which(mm.necessity.df[,1] == min(mm.necessity.df[,1])),])
rownames(mm.necessity.df[which(mm.necessity.df[,1] == max(mm.necessity.df[,1])),])

# which country has the lowest/highest median
rownames(mm.necessity.df[which(mm.necessity.df[,2] == min(mm.necessity.df[,2])),])
rownames(mm.necessity.df[which(mm.necessity.df[,2] == max(mm.necessity.df[,2])),])

# plot means
plot(g20, mm.necessity.df[,1])

# is the data normally distributed?
hist(mm.necessity.df$means.necessity, freq = FALSE, xlab = 'country means', main = NULL)
g = seq(min(mm.necessity.df$means.necessity, na.rm=TRUE), max(mm.necessity.df$means.necessity, na.rm=TRUE), length=100) #Create a grid
dens.norm = dnorm(g, mean(mm.necessity.dfp$means.necessity.p), sd(mm.necessity.dfp$means.necessity.p)) #Compute the normal density on g
lines(g, dens.norm, col="red3", lwd=2)

hist(mm.necessity.dfp$means.necessity.p, freq = FALSE, xlab = 'country means', main = NULL)
g = seq(min(mm.necessity.dfp$means.necessity.p,na.rm = TRUE), max(mm.necessity.dfp$means.necessity.p,na.rm = TRUE), length=100) #Create a grid
dens.norm = dnorm(g, mean(mm.necessity.df$means.necessity), sd(mm.necessity.df$means.necessity)) #Compute the normal density on g
lines(g, dens.norm, col="red3", lwd=2)

#KDE using gaussian Kernel
mm.necessity.df
density(mm.necessity.dfp[,1],na.rm=TRUE)
plot(density(mm.necessity.dfp[,1],na.rm=TRUE))
#Emperical cdf
ecdf(mm.necessity.dfp[,1])
plot(ecdf(mm.necessity.dfp[,1]))
#Mean characteristics and measures of the sample
summary(mm.necessity.dfp[,1]) 
boxplot(mm.necessity.dfp[,1])
mm.necessity.dfp
mm.oppo.dfp

mm.necessity.a=mm.necessity.dfp[-c(89,91,74),]

mm.necessity.a
which(rownames(mm.necessity.dfp)=="Taiwan")
which(rownames(mm.necessity.dfp)=="Tonga Islands")
which(rownames(mm.necessity.dfp)=="Puerto Rico")

dim(mm.necessity.a)
