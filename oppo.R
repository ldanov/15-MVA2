#Opportunity Entrepreneurship (import data)
oppo = read.csv(file = "oppo.csv", sep = ";", head = TRUE)
head(oppo)
# let's change the "-" to NAs
oppo[oppo=="-"] = NA

# what is the class of variables? Can we perform any analysis?
is.numeric(oppo[,2]) # not numeric so we have to transform them in order to do anything
oppo[,2:15] = sapply(oppo[,2:15], as.character)
oppo[,2:15] = sapply(oppo[,2:15], as.numeric)
is.numeric(oppo[,2])

# let's name the columns nicely
oppo.names = c("country", "2001":"2014")
colnames(oppo) = oppo.names

# let's try to extract the g20 countries
g20 = factor(c("Argentina", "Australia", "Brazil", "Canada", "China", "France", "Germany", "India", "Indonesia", "Italy", "Japan", "Korea (South)", "Mexico", "Russia", "Saudi Arabia", "South Africa", "Turkey", "United Kingdom", "United States of America"))
oppo.g20 = subset(oppo,  oppo$country == "Argentina" | oppo$country == "Australia" | oppo$country == "Brazil" | oppo$country == "China" | oppo$country == "France" | oppo$country == "India" | oppo$country == "Indonesia" | oppo$country == "Japan" | oppo$country =="Canada" | oppo$country == "Korea (South)" | oppo$country == "Mexico" | oppo$country == "Germany" |oppo$country == "Saudi Arabia" | oppo$country == "South Africa" | oppo$country == "Russia" | oppo$country == "United States of America" | oppo$country == "Turkey" | oppo$country == "United Kingdom" | oppo$country == "Italy")
# it's an awful command but works, must do for now
head(oppo.g20)
# let's name the rows nicely
rownames(oppo.g20) = g20

rownames(oppo)=oppo$"country"
oppo$country=NULL
head(oppo)

#let's transpose
oppo.g20t = t(oppo.g20)

# remove the first column
oppo.g20$country = NULL
oppo.g20

# means per country
oppo.g20t = t(oppo.g20)
oppo.t=t(oppo)
means.oppo = c(rowMeans(oppo.g20, na.rm = TRUE))
means.oppo.p=c(rowMeans(oppo, na.rm=TRUE))
head(means.oppo.p)
# install matrixStats package
# install.packages("matrixStats")
library("matrixStats")
head(means.oppo)
means.oppo=c(means.oppo)
median.oppo = c(colMedians(oppo.g20t, na.rm = TRUE))
median.oppo.p=c(colMedians(oppo.t, na.rm=TRUE))
# create an mm dataframe
mm.oppo.df = data.frame(means.oppo, median.oppo)
mm.oppo.dfp= data.frame(means.oppo.p, median.oppo.p)

# subset the countries by the mean
subset(mm.oppo.df, mm.oppo.df$means.oppo < mean(mm.oppo.df[,1]))
subset(mm.oppo.df, mm.oppo.df$means.oppo > mean(mm.oppo.df[,1]))

# subset the countries by the median
subset(mm.oppo.df, mm.oppo.df$median.oppo < median(mm.oppo.df[,2]))
subset(mm.oppo.df, mm.oppo.df$median.oppo > median(mm.oppo.df[,2]))

# which country has the lowest/highest mean
rownames(mm.oppo.df[which(mm.oppo.df[,1] == min(mm.oppo.df[,1])),])
rownames(mm.oppo.df[which(mm.oppo.df[,1] == max(mm.oppo.df[,1])),])

# which country has the lowest/highest median
rownames(mm.oppo.df[which(mm.oppo.df[,2] == min(mm.oppo.df[,2])),])
rownames(mm.oppo.df[which(mm.oppo.df[,2] == max(mm.oppo.df[,2])),])

# plot means
plot(g20, mm.oppo.df[,1])

# is the data normally distributed?
hist(mm.oppo.df$means.oppo, freq = FALSE, xlab = 'country means', main = NULL)
g = seq(na.omit(min(mm.oppo.df$means.oppo), max(mm.oppo.df$means.oppo)), length=100) #Create a grid
dens.norm = dnorm(g, mean(mm.oppo.dfp$means.oppo.p), sd(mm.oppo.dfp$means.oppo.p)) #Compute the normal density on g
lines(g, dens.norm, col="red3", lwd=2)

hist(mm.oppo.dfp$means.oppo.p, freq = FALSE, xlab = 'country means', main = NULL)
g = seq(min(mm.oppo.dfp$means.oppo.p,na.rm = TRUE), max(mm.oppo.dfp$means.oppo.p,na.rm = TRUE), length=100) #Create a grid
dens.norm = dnorm(g, mean(mm.oppo.df$means.oppo), sd(mm.oppo.df$means.oppo)) #Compute the normal density on g
lines(g, dens.norm, col="red3", lwd=2)
oppo
#KDE using gaussian Kernel
mm.oppo.df
density(mm.oppo.dfp[,1],na.rm=TRUE)
plot(density(mm.oppo.dfp[,1],na.rm=TRUE))
#Emperical cdf
ecdf(mm.oppo.dfp[,1])
plot(ecdf(mm.oppo.dfp[,1]))
#Mean characteristics and measures of the sample
summary(mm.oppo.dfp[,1]) 
boxplot(mm.oppo.dfp[,1])
mm.oppo.a=mm.oppo.dfp[-c(89,91,74),]

which(rownames(mm.oppo.dfp)=="Taiwan")
which(rownames(mm.oppo.dfp)=="Tonga Islands")
which(rownames(mm.oppo.dfp)=="Puerto Rico")
