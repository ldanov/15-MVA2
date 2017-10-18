#established business ownership rates (import data)
fem = read.csv(file = "fem.csv", sep = ";", head = TRUE)
head(fem)
# let's change the "-" to NAs
fem[fem=="-"] = NA

# what is the class of variables? Can we perform any analysis?
is.numeric(fem[,2]) # not numeric so we have to transform them in order to do anything
fem[,2:15] = sapply(fem[,2:15], as.character)
fem[,2:15] = sapply(fem[,2:15], as.numeric)
is.numeric(fem[,2])

# let's name the columns nicely
fem.names = c("country", "2001":"2014")
colnames(fem) = fem.names

# let's try to extract the g20 countries
g20 = factor(c("Argentina", "Australia", "Brazil", "Canada", "China", "France", "Germany", "India", "Indonesia", "Italy", "Japan", "Korea (South)", "Mexico", "Russia", "Saudi Arabia", "South Africa", "Turkey", "United Kingdom", "United States of America"))
fem.g20 = subset(fem,  fem$country == "Argentina" | fem$country == "Australia" | fem$country == "Brazil" | fem$country == "China" | fem$country == "France" | fem$country == "India" | fem$country == "Indonesia" | fem$country == "Japan" | fem$country =="Canada" | fem$country == "Korea (South)" | fem$country == "Mexico" | fem$country == "Germany" |fem$country == "Saudi Arabia" | fem$country == "South Africa" | fem$country == "Russia" | fem$country == "United States of America" | fem$country == "Turkey" | fem$country == "United Kingdom" | fem$country == "Italy")
# it's an awful command but works, must do for now
head(fem.g20)
# let's name the rows nicely
rownames(fem.g20) = g20

rownames(fem)=fem$"country"
fem$country=NULL
head(fem)

#let's transpose
fem.g20t = t(fem.g20)

# remove the first column
fem.g20$country = NULL
fem.g20

# means per country
fem.g20t = t(fem.g20)
fem.t=t(fem)
means.fem = c(rowMeans(fem.g20, na.rm = TRUE))
means.fem.p=c(rowMeans(fem, na.rm=TRUE))
head(means.fem.p)
# install matrixStats package
# install.packages("matrixStats")
library("matrixStats")

#
median.fem = c(colMedians(fem.g20t, na.rm = TRUE))
median.fem.p=c(colMedians(fem.t, na.rm=TRUE))
# create an mm dataframe
mm.fem.df = data.frame(means.fem, median.fem)
mm.fem.dfp= data.frame(means.fem.p, median.fem.p)
# subset the countries by the mean
subset(mm.fem.df, mm.fem.df$means.fem < mean(mm.fem.df[,1]))
subset(mm.fem.df, mm.fem.df$means.fem > mean(mm.fem.df[,1]))

# subset the countries by the median
subset(mm.fem.df, mm.fem.df$median.fem < median(mm.fem.df[,2]))
subset(mm.fem.df, mm.fem.df$median.fem > median(mm.fem.df[,2]))

# which country has the lowest/highest mean
rownames(mm.fem.df[which(mm.fem.df[,1] == min(mm.fem.df[,1])),])
rownames(mm.fem.df[which(mm.fem.df[,1] == max(mm.fem.df[,1])),])

# which country has the lowest/highest median
rownames(mm.fem.df[which(mm.fem.df[,2] == min(mm.fem.df[,2])),])
rownames(mm.fem.df[which(mm.fem.df[,2] == max(mm.fem.df[,2])),])

# plot means
plot(g20, mm.fem.df[,1])

# is the data normally distributed?
hist(mm.fem.df$means.fem, freq = FALSE, xlab = 'country means', main = NULL)
g = seq(na.omit(min(mm.fem.df$means.fem), max(mm.fem.df$means.fem)), length=100) #Create a grid
dens.norm = dnorm(g, mean(mm.fem.dfp$means.fem.p), sd(mm.fem.dfp$means.fem.p)) #Compute the normal density on g
lines(g, dens.norm, col="red3", lwd=2)

hist(mm.fem.dfp$means.fem.p, freq = FALSE, xlab = 'country means', main = NULL)
g = seq(min(mm.fem.dfp$means.fem.p,na.rm = TRUE), max(mm.fem.dfp$means.fem.p,na.rm = TRUE), length=100) #Create a grid
dens.norm = dnorm(g, mean(mm.fem.df$means.fem), sd(mm.fem.df$means.fem)) #Compute the normal density on g
lines(g, dens.norm, col="red3", lwd=2)

#KDE using gaussian Kernel
mm.fem.df
density(mm.fem.dfp[,1],na.rm=TRUE)
plot(density(mm.fem.dfp[,1],na.rm=TRUE))
#Emperical cdf
ecdf(mm.fem.dfp[,1])
plot(ecdf(mm.fem.dfp[,1]))
#Mean characteristics and measures of the sample
summary(mm.fem.dfp[,1]) 
boxplot(mm.fem.dfp[,1])
mm.fem.df

mm.fem.a=mm.fem.dfp[-c(89,91,74),]

which(rownames(mm.fem.dfp)=="Taiwan")
which(rownames(mm.fem.dfp)=="Tonga Islands")
which(rownames(mm.fem.dfp)=="Puerto Rico")
