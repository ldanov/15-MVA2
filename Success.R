#successrtunity Entrepreneurship (import data)
success = read.csv(file = "success.csv", sep = ";", head = TRUE)
head(success)
# let's change the "-" to NAs
success[success=="-"] = NA

# what is the class of variables? Can we perform any analysis?
is.numeric(success[,2]) # not numeric so we have to transform them in order to do anything
success[,2:13] = sapply(success[,2:13], as.character)
success[,2:13] = sapply(success[,2:13], as.numeric)
is.numeric(success[,2])

# let's name the columns nicely
success.names = c("country", "2003":"2014")
colnames(success) = success.names

# let's try to extract the g20 countries
g20 = factor(c("Argentina", "Australia", "Brazil", "Canada", "China", "France", "Germany", "India", "Indonesia", "Italy", "Japan", "Korea (South)", "Mexico", "Russia", "Saudi Arabia", "South Africa", "Turkey", "United Kingdom", "United States of America"))
success.g20 = subset(success,  success$country == "Argentina" | success$country == "Australia" | success$country == "Brazil" | success$country == "China" | success$country == "France" | success$country == "India" | success$country == "Indonesia" | success$country == "Japan" | success$country =="Canada" | success$country == "Korea (South)" | success$country == "Mexico" | success$country == "Germany" |success$country == "Saudi Arabia" | success$country == "South Africa" | success$country == "Russia" | success$country == "United States of America" | success$country == "Turkey" | success$country == "United Kingdom" | success$country == "Italy")
# it's an awful command but works, must do for now
head(success.g20)
# let's name the rows nicely
rownames(success.g20) = g20

rownames(success)=success$"country"
success$country=NULL
head(success)

#let's transpose
success.g20t = t(success.g20)
success.t=t(success)

# remove the first column
success.g20$country = NULL
success.g20

# means per country
means.success = c(rowMeans(success.g20, na.rm = TRUE))
means.success.p=c(rowMeans(success, na.rm=TRUE))
head(means.success.p)
# install matrixStats package
# install.packages("matrixStats")
library("matrixStats")
head(means.success)
mm.success.df
means.success=c(means.success)
median.success = c(colMedians(success.g20t, na.rm = TRUE))
median.success.p=c(colMedians(success.t, na.rm=TRUE))
# create an mm dataframe
mm.success.df = data.frame(means.success, median.success)
mm.success.dfp= data.frame(means.success.p, median.success.p)

# subset the countries by the mean
subset(mm.success.df, mm.success.df$means.success < mean(mm.success.df[,1]))
subset(mm.success.df, mm.success.df$means.success > mean(mm.success.df[,1]))

# subset the countries by the median
subset(mm.success.df, mm.success.df$median.success < median(mm.success.df[,2]))
subset(mm.success.df, mm.success.df$median.success > median(mm.success.df[,2]))

# which country has the lowest/highest mean
rownames(mm.success.df[which(mm.success.df[,1] == min(mm.success.df[,1])),])
rownames(mm.success.df[which(mm.success.df[,1] == max(mm.success.df[,1])),])

# which country has the lowest/highest median
rownames(mm.success.df[which(mm.success.df[,2] == min(mm.success.df[,2])),])
rownames(mm.success.df[which(mm.success.df[,2] == max(mm.success.df[,2])),])

# plot means
plot(g20, mm.success.df[,1])

# is the data normally distributed?
hist(mm.success.df$means.success, freq = FALSE, xlab = 'country means', main = NULL)
g = seq(na.omit(min(mm.success.df$means.success), max(mm.success.df$means.success)), length=100) #Create a grid
dens.norm = dnorm(g, mean(mm.success.dfp$means.success.p), sd(mm.success.dfp$means.success.p)) #Compute the normal density on g
lines(g, dens.norm, col="red3", lwd=2)

hist(mm.success.dfp$means.success.p, freq = FALSE, xlab = 'country means', main = NULL)
g = seq(min(mm.success.dfp$means.success.p,na.rm = TRUE), max(mm.success.dfp$means.success.p,na.rm = TRUE), length=100) #Create a grid
dens.norm = dnorm(g, mean(mm.success.df$means.success), sd(mm.success.df$means.success)) #Compute the normal density on g
lines(g, dens.norm, col="red3", lwd=2)
success

#KDE using gaussian Kernel
mm.success.df
density(mm.success.dfp[,1],na.rm=TRUE)
plot(density(mm.success.dfp[,1],na.rm=TRUE))
#Emperical cdf
ecdf(mm.success.dfp[,1])
plot(ecdf(mm.success.dfp[,1]))
#Mean characteristics and measures of the sample
summary(mm.success.dfp[,1]) 
boxplot(mm.success.dfp[,1])
mm.success.a=mm.success.dfp[-c(89,91,74),]

which(rownames(mm.success.dfp)=="Taiwan")
which(rownames(mm.success.dfp)=="Tonga Islands")
which(rownames(mm.success.dfp)=="Puerto Rico")


