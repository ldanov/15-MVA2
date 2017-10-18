#Download the csv file
HDI.components = read.csv(file ="HDI.components.csv" , sep = ";", head = TRUE)
head(HDI.components)
#Substitute Missin Information
HDI.components[HDI.components==".."]=NA
head(HDI.components)
#Translate into numeric Data
HDI.components[,1] = sapply(HDI.components[,1], as.character)
HDI.components[,1] = sapply(HDI.components[,1], as.numeric)
HDI.components[,3] = sapply(HDI.components[,3], as.character)
HDI.components[,3] = sapply(HDI.components[,3], as.numeric)
HDI.components[,4] = sapply(HDI.components[,4], as.character)
HDI.components[,4] = sapply(HDI.components[,4], as.numeric)
HDI.components[,5] = sapply(HDI.components[,5], as.character)
HDI.components[,5] = sapply(HDI.components[,5], as.numeric)
HDI.components[,6] = sapply(HDI.components[,6], as.character)
HDI.components[,6] = sapply(HDI.components[,6], as.numeric)
HDI.components[,7] = sapply(HDI.components[,7], as.character)
HDI.components[,7] = sapply(HDI.components[,7], as.numeric)
is.atomic(HDI.components[,2])
#Name the columns
HDI.components.names=c("HDI.Rank","country","HDI.Value","Life.expectancy","Mean.Schooling","Exp.school","GNI.PP")
colnames(HDI.components)=HDI.components.names
head(HDI.components)
is.atomic(HDI.components[,2])

##Eliminate the non analyzed nations and create a row vector
g20 = factor(c("Argentina", "Australia", "Brazil", "Canada", "China", "France", "Germany", "India", "Indonesia", "Italy", "Japan", "Korea (South)", "Mexico", "Russia", "Saudi Arabia", "South Africa", "Turkey", "United Kingdom", "United States of America"))
HDI.components.g20 = subset(HDI.components, HDI.components$country == "Argentina" | HDI.components$country == "Australia" | HDI.components$country == "Brazil" | HDI.components$country == "Canada" | HDI.components$country == "China" | HDI.components$country == "France" | HDI.components$country == "Germany" | HDI.components$country == "India" | HDI.components$country == "Indonesia" | HDI.components$country =="Italy" | HDI.components$country == "Japan" | HDI.components$country == "Korea (Republic of)" | HDI.components$country == "Mexico" | HDI.components$country == "Russian Federation" | HDI.components$country == "Saudi Arabia" | HDI.components$country == "South Africa" | HDI.components$country == "Turkey" | HDI.components$country == "United Kingdom" | HDI.components$country == "United States")
head(HDI.components.g20)

#Sort alfabetically
HDI.components.g20=HDI.components.g20[order(HDI.components.g20$"country",decreasing=FALSE),]

#Summary statistics
summary(HDI.components.g20)
sd(HDI.components.g20[,2],na.rm=TRUE)
sd(HDI.components.g20[,3],na.rm=TRUE)
sd(HDI.components.g20[,4],na.rm=TRUE)
sd(HDI.components.g20[,5],na.rm=TRUE)
sd(HDI.components.g20[,6],na.rm=TRUE)

#determine the rows
rownames(HDI.components.g20)=g20
HDI.components.g20$country=NULL
HDI.components.g20

rownames(HDI.components)=HDI.components$"country"
HDI.components$country=NULL
HDI.components

#Transposed frame
HDI.components.g20t=t(HDI.components.g20)

#Compare Data
subset(HDI.components.g20,HDI.components.g20$HDI.Value < mean(HDI.components.g20[,2],na.rm=TRUE))
subset(HDI.components.g20,HDI.components.g20$HDI.Value > mean(HDI.components.g20[,2],na.rm=TRUE))
subset(HDI.components.g20,HDI.components.g20$HDI.Value < median(HDI.components.g20[,2],na.rm=TRUE))
subset(HDI.components.g20,HDI.components.g20$HDI.Value > median(HDI.components.g20[,2],na.rm=TRUE))

subset(HDI.components.g20,HDI.components.g20$Life.expectancy < mean(HDI.components.g20[,3],na.rm=TRUE))
subset(HDI.components.g20,HDI.components.g20$Life.expectancy > mean(HDI.components.g20[,3],na.rm=TRUE))
subset(HDI.components.g20,HDI.components.g20$Life.expectancy < median(HDI.components.g20[,3],na.rm=TRUE))
subset(HDI.components.g20,HDI.components.g20$Life.expectancy > median(HDI.components.g20[,3],na.rm=TRUE))

subset(HDI.components.g20,HDI.components.g20$Mean.Schooling < mean(HDI.components.g20[,4],na.rm=TRUE))
subset(HDI.components.g20,HDI.components.g20$Mean.Schooling > mean(HDI.components.g20[,4],na.rm=TRUE))
subset(HDI.components.g20,HDI.components.g20$Mean.Schooling < median(HDI.components.g20[,4],na.rm=TRUE))
subset(HDI.components.g20,HDI.components.g20$Mean.Schooling > median(HDI.components.g20[,4],na.rm=TRUE))

subset(HDI.components.g20,HDI.components.g20$Exp.school < mean(HDI.components.g20[,5],na.rm=TRUE))
subset(HDI.components.g20,HDI.components.g20$Exp.school > mean(HDI.components.g20[,5],na.rm=TRUE))
subset(HDI.components.g20,HDI.components.g20$Exp.school < median(HDI.components.g20[,5],na.rm=TRUE))
subset(HDI.components.g20,HDI.components.g20$Exp.school > median(HDI.components.g20[,5],na.rm=TRUE))

subset(HDI.components.g20,HDI.components.g20$GNI.PP < mean(HDI.components.g20[,6],na.rm=TRUE))
subset(HDI.components.g20,HDI.components.g20$GNI.PP > mean(HDI.components.g20[,6],na.rm=TRUE))
subset(HDI.components.g20,HDI.components.g20$GNI.PP < median(HDI.components.g20[,6],na.rm=TRUE))
subset(HDI.components.g20,HDI.components.g20$GNI.PP > median(HDI.components.g20[,6],na.rm=TRUE))

HDI.components.g20
# Determination of the values distribution
hist(HDI.components.g20$HDI.Value,freq=FALSE,xlab='HDI',main="HDI Distribution")
g=seq(min(HDI.components.g20$HDI.Value),max(HDI.components.g20$HDI.Value),length=100)
dens.nrom=dnorm(g,mean(HDI.components.g20$HDI.Value),sd(HDI.components.g20$HDI.Value))
lines(g,dens.nrom,col="red3", lwd=2)

hist(HDI.components.g20$Life.expectancy,freq=FALSE,xlab='Years',main="Life Expectancy")
g=seq(min(HDI.components.g20$Life.expectancy),max(HDI.components.g20$Life.expectancy),length=100)
dens.nrom=dnorm(g,mean(HDI.components.g20$Life.expectancy),sd(HDI.components.g20$Life.expectancy))
lines(g,dens.nrom,col="red3", lwd=2)

hist(HDI.components.g20$Mean.Schooling,freq=FALSE,xlab='Years',main="Mean Years of Schooling",) 
g=seq(min(HDI.components.g20$Mean.Schooling,na.rm=TRUE),max(HDI.components.g20$Mean.Schooling,na.rm=TRUE),length=100)
dens.nrom=dnorm(g,mean(HDI.components.g20$Mean.Schooling,na.rm=TRUE),sd(HDI.components.g20$Mean.Schooling,na.rm=TRUE))
lines(g,dens.nrom,col="red3", lwd=2)

hist(HDI.components.g20$GNI.PP,freq=FALSE,xlab='Years',main="GNI PP",) 
g=seq(min(HDI.components.g20$GNI.PP,na.rm=TRUE),max(HDI.components.g20$GNI.PP,na.rm=TRUE),length=100)
dens.nrom=dnorm(g,mean(HDI.components.g20$GNI.PP,na.rm=TRUE),sd(HDI.components.g20$GNI.PP,na.rm=TRUE))
lines(g,dens.nrom,col="red3", lwd=2)

hist(HDI.components.g20$Exp.school,freq=FALSE,xlab='Years',main="Expected Years of Schooling",) 
g=seq(min(HDI.components.g20$Exp.school),max(HDI.components.g20$Exp.school),length=100)
dens.nrom=dnorm(g,mean(HDI.components.g20$Exp.school,na.rm=TRUE),sd(HDI.components.g20$Exp.school,na.rm=TRUE))
lines(g,dens.nrom,col="red3", lwd=2)

#Which are the countries with the lower values
rownames(HDI.components.g20[which(HDI.components.g20[,1] == min(HDI.components.g20[,1])),])
rownames(HDI.components.g20[which(HDI.components.g20[,1] == max(HDI.components.g20[,1])),])
rownames(HDI.components.g20[which(HDI.components.g20[,2] == max(HDI.components.g20[,2],na.rm=TRUE)),])
rownames(HDI.components.g20[which(HDI.components.g20[,2] == min(HDI.components.g20[,2],na.rm=TRUE)),])
rownames(HDI.components.g20[which(HDI.components.g20[,3] == max(HDI.components.g20[,3],na.rm=TRUE)),])
rownames(HDI.components.g20[which(HDI.components.g20[,3] == min(HDI.components.g20[,3],na.rm=TRUE)),])
rownames(HDI.components.g20[which(HDI.components.g20[,4] == max(HDI.components.g20[,4],na.rm=TRUE)),])
rownames(HDI.components.g20[which(HDI.components.g20[,4] == min(HDI.components.g20[,4],na.rm=TRUE)),])
rownames(HDI.components.g20[which(HDI.components.g20[,5] == max(HDI.components.g20[,5],na.rm=TRUE)),])
rownames(HDI.components.g20[which(HDI.components.g20[,5] == min(HDI.components.g20[,5],na.rm=TRUE)),])
HDI.components.g20

#KDE using gaussian Kernel
HDI.components.g20
density(HDI.components.g20[,3],na.rm=TRUE)
plot(density(HDI.components.g20[,3],na.rm=TRUE))
#Emperical cdf
ecdf(HDI.components.g20[,3])
plot(ecdf(HDI.components.g20[,3]))
#Mean characteristics and measures of the sample
summary(HDI.components.g20[,3]) 
boxplot(HDI.components.g20[,3])

#Correlation Matrix 
cor.HDI.LE=cor(na.omit(HDI.components.g20))
cor.HDI.LE
HDI.components.g20

#Covariances
cov(HDI.components[,2],HDI.components[,3])
plot(HDI.components[,2]~HDI.components[,3])
lm.life=lm(HDI.components[,2]~HDI.components[,3]) #Regression


#Covariances Gender HDI.components.g20uality
cov(HDI.components[,2],HDI.components[,4]) #How to avoid NA result
plot(HDI.components[,2]~HDI.components[,4],main="Covariance of gender HDI.components.g20uality and HDI",xlab="HDI.components.g20uality in life expectancy", ylab="HDI")
lm.meanschool=lm(HDI.components.g20[,2]~HDI.components.g20[,4]) #Regression

#Covariances Income HDI.components.g20uality
cov(HDI.components [,2],HDI.components[,6])
plot(HDI.components[,2]~HDI.components[,6],main="Covariance of gender HDI.components.g20uality and HDI",xlab="HDI.components.g20uality in life expectancy", ylab="HDI")
lm.GNI=lm(HDI.components.g20[,2]~HDI.components.g20[,6]) #Regression


#Plot and do a linnear regression
lml=lm(HDI.components[,2]~HDI.components[,6] - 1)
summary(lml)
plot(HDI.components[,2],HDI.components[,6],type='p',col=8,pch=1,)
lines(HDI.components[,2],HDI.components[,6],fitted(lml,col=2,lwd=2)
head(HDI.components)

#Three dimentional plots
lm2<-lm(HDI.components$HDI.Value ~ HDI.components$Life.expectancy+HDI.components$GNI.PP)
install.packages("scatterplot3d")
library(scatterplot3d)
s3d=scatterplot3d(x=HDI.components$HDI.Value, y=HDI.components$Life.expectancy,z=HDI.components$GNI.PP, scale.y=.5,type="h",highlight.3d=TRUE,angle=55,pch=20,lwd=1,tick.marks=TRUE,col.grid="red",main="scatterplot3d-5")

fit = princomp(na.omit(HDI.components)) # fit PCA model
print(summary(fit, loadings = TRUE), 2) # display the result
     
