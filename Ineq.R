#Download the csv file
ineq = read.csv(file ="ineq.csv" , sep = ";", head = TRUE)
head(ineq)
#Substitute Missin Information
ineq[ineq==".."]=NA
ineq
#Translate into numeric Data
ineq[,1] = sapply(ineq[,1], as.character)
ineq[,1] = sapply(ineq[,1], as.numeric)
ineq[,3] = sapply(ineq[,3], as.character)
ineq[,3] = sapply(ineq[,3], as.numeric)
ineq[,4] = sapply(ineq[,4], as.character)
ineq[,4] = sapply(ineq[,4], as.numeric)
ineq[,5] = sapply(ineq[,5], as.character)
ineq[,5] = sapply(ineq[,5], as.numeric)
ineq[,6] = sapply(ineq[,6], as.character)
ineq[,6] = sapply(ineq[,6], as.numeric)
ineq[,7] = sapply(ineq[,7], as.character)
ineq[,7] = sapply(ineq[,7], as.numeric)
#Name the columns
ineq.names=c("HDI.rank","country","HDI.Value","Ineq.life.expectancy","ineq.educ","ineq.income","Quantil")
colnames(ineq)=ineq.names
head(ineq)
ineq

##Eliminate the non analyzed nations and create a row vector
g20 = factor(c("Argentina", "Australia", "Brazil", "Canada", "China", "France", "Germany", "India", "Indonesia", "Italy", "Japan", "Korea (South)", "Mexico", "Russia", "Saudi Arabia", "South Africa", "Turkey", "United Kingdom", "United States of America"))
ineq.g20 = subset(ineq, ineq$country == "Argentina" | ineq$country == "Australia" | ineq$country == "Brazil" | ineq$country == "Canada" | ineq$country == "China" | ineq$country == "France" | ineq$country == "Germany" | ineq$country == "India" | ineq$country == "Indonesia" | ineq$country =="Italy" | ineq$country == "Japan" | ineq$country == "Korea (Republic of)" | ineq$country == "Mexico" | ineq$country == "Russian Federation" | ineq$country == "Saudi Arabia" | ineq$country == "South Africa" | ineq$country == "Turkey" | ineq$country == "United Kingdom" | ineq$country == "United States")
ineq.g20

#Sort alfabetically
ineq.g20=ineq.g20[order(ineq.g20$"country",decreasing=FALSE),]

#determine the rows
rownames(ineq)=ineq[,2]
ineq$country=NULL
ineq

#Summary statistics
summary(ineq.g20)
sd(ineq.g20[,2],na.rm=TRUE)
sd(ineq.g20[,3],na.rm=TRUE)
sd(ineq.g20[,4],na.rm=TRUE)
sd(ineq.g20[,5],na.rm=TRUE)
ineq.g20

#determine the rows
rownames(ineq.g20)=g20
ineq.g20$country=NULL
ineq.g20

head(ineq)

#Transposed frame
ineq.g20t=t(ineq.g20)

#Compare Data
subset(ineq.g20,ineq.g20$HDI.Value < mean(ineq.g20[,2],na.rm=TRUE))
subset(ineq.g20,ineq.g20$HDI.Value > mean(ineq.g20[,2],na.rm=TRUE))
subset(ineq.g20,ineq.g20$HDI.Value < median(ineq.g20[,2],na.rm=TRUE))
subset(ineq.g20,ineq.g20$HDI.Value > median(ineq.g20[,2],na.rm=TRUE))

subset(ineq.g20,ineq.g20$Ineq.life.expectancy < mean(ineq.g20[,3],na.rm=TRUE))
subset(ineq.g20,ineq.g20$Ineq.life.expectancy > mean(ineq.g20[,3],na.rm=TRUE))
subset(ineq.g20,ineq.g20$Ineq.life.expectancy < median(ineq.g20[,3],na.rm=TRUE))
subset(ineq.g20,ineq.g20$Ineq.life.expectancy > median(ineq.g20[,3],na.rm=TRUE))

subset(ineq.g20,ineq.g20$ineq.educ < mean(ineq.g20[,4],na.rm=TRUE))
subset(ineq.g20,ineq.g20$ineq.educ > mean(ineq.g20[,4],na.rm=TRUE))
subset(ineq.g20,ineq.g20$ineq.educ < median(ineq.g20[,4],na.rm=TRUE))
subset(ineq.g20,ineq.g20$ineq.educ > median(ineq.g20[,4],na.rm=TRUE))

subset(ineq.g20,ineq.g20$ineq.income < mean(ineq.g20[,5],na.rm=TRUE))
subset(ineq.g20,ineq.g20$ineq.income > mean(ineq.g20[,5],na.rm=TRUE))
subset(ineq.g20,ineq.g20$ineq.income < median(ineq.g20[,5],na.rm=TRUE))
subset(ineq.g20,ineq.g20$ineq.income > median(ineq.g20[,5],na.rm=TRUE))

ineq.g20
# Determination of the values distribution
hist(ineq.g20$HDI.Value,freq=FALSE,xlab='HDI',main="HDI Distribution")
g=seq(min(ineq.g20$HDI.Value),max(ineq.g20$HDI.Value),length=100)
dens.nrom=dnorm(g,mean(ineq.g20$HDI.Value),sd(ineq.g20$HDI.Value))
lines(g,dens.nrom,col="red3", lwd=2)

hist(ineq.g20$Ineq.life.expectancy,freq=FALSE,xlab='HDI',main="HDI Distribution")
g=seq(min(ineq.g20$Ineq.life.expectancy),max(ineq.g20$Ineq.life.expectancy),length=100)
dens.nrom=dnorm(g,mean(ineq.g20$Ineq.life.expectancy),sd(ineq.g20$Ineq.life.expectancy))
lines(g,dens.nrom,col="red3", lwd=2)

hist(ineq.g20$ineq.educ,freq=FALSE,xlab='Inequality',main="Inequality in Education",) 
g=seq(min(ineq.g20$ineq.educ,na.rm=TRUE),max(ineq.g20$ineq.educ,na.rm=TRUE),length=100)
dens.nrom=dnorm(g,mean(ineq.g20$ineq.educ,na.rm=TRUE),sd(ineq.g20$ineq.educ,na.rm=TRUE))
lines(g,dens.nrom,col="red3", lwd=2)

hist(ineq.g20$ineq.income,freq=FALSE,xlab='Inequality',main="Inequality in Income",) 
g=seq(min(ineq.g20$ineq.income,na.rm=TRUE),max(ineq.g20$ineq.income,na.rm=TRUE),length=100)
dens.nrom=dnorm(g,mean(ineq.g20$ineq.income,na.rm=TRUE),sd(ineq.g20$ineq.income,na.rm=TRUE))
lines(g,dens.nrom,col="red3", lwd=2)

hist(ineq.g20$ineq.income,freq=FALSE,xlab='Inequality',main="Inequality in Income",) 
g=seq(min(ineq.g20$ineq.income,na.rm=TRUE),max(ineq.g20$ineq.income,na.rm=TRUE),length=100)
dens.nrom=dnorm(g,mean(ineq.g20$ineq.income,na.rm=TRUE),sd(ineq.g20$ineq.income,na.rm=TRUE))
lines(g,dens.nrom,col="red3", lwd=2)

#Which are the countries with the lower values
rownames(ineq.g20[which(ineq.g20[,1] == min(ineq.g20[,1])),])
rownames(ineq.g20[which(ineq.g20[,1] == max(ineq.g20[,1])),])
rownames(ineq.g20[which(ineq.g20[,2] == max(ineq.g20[,2],na.rm=TRUE)),])
rownames(ineq.g20[which(ineq.g20[,2] == min(ineq.g20[,2],na.rm=TRUE)),])
rownames(ineq.g20[which(ineq.g20[,3] == max(ineq.g20[,3],na.rm=TRUE)),])
rownames(ineq.g20[which(ineq.g20[,3] == min(ineq.g20[,3],na.rm=TRUE)),])
rownames(ineq.g20[which(ineq.g20[,4] == max(ineq.g20[,4],na.rm=TRUE)),])
rownames(ineq.g20[which(ineq.g20[,4] == min(ineq.g20[,4],na.rm=TRUE)),])
rownames(ineq.g20[which(ineq.g20[,5] == max(ineq.g20[,5],na.rm=TRUE)),])
rownames(ineq.g20[which(ineq.g20[,5] == min(ineq.g20[,5],na.rm=TRUE)),])
ineq.g20

#KDE using gaussian Kernel
gini.g20
density(ineq.g20[,3],na.rm=TRUE)
plot(density(ineq.g20[,3],na.rm=TRUE))
#Emperical cdf
ecdf(ineq.g20[,3])
plot(ecdf(ineq.g20[,3]))
#Mean characteristics and measures of the sample
summary(ineq.g20[,3]) 

#Boxplot comparison
boxplot(ineq.g20[,3:5], col="yellow",varwith=TRUE,outline=TRUE)

#Correlation Matrix 
cor.HDI.LE=cor(ineq.g20$HDI.Value,ineq.g20$Ineq.life.expectancy)
cor.HDI.LE=cor(na.omit(ineq.g20))
cor.HDI.LE
ineq.g20

#Covariances
cov(ineq.g20[,2],ineq.g20[,3])
pairs(ineq.g20) Kristian 
plot(ineq.g20[,2]~ineq.g20[,3])
summary(y) #Descriptive statistics for Y
fit=lm(ineq.g20[,2]~ineq.g20[,3]) #Regression
plot(fit)

#Covariances Gender Inequality
cov(ineq [,2],ineq[,3])
plot(ineq[,2]~ineq[,3],main="Covariance of gender inequality and HDI",xlab="Inequality in life expectancy", ylab="HDI")
summary(y) #Descriptive statistics for Y
lm(ineq[,2]~ineq[,3]) #Regression

#Covariances Income Inequality
cov(ineq [,2],ineq[,5])
plot(ineq[,2]~ineq[,5],main="Covariance of gender inequality and HDI",xlab="Gender Inequality", ylab="HDI")
summary(y) #Descriptive statistics for Y
fit=lm(ineq[,2]~ineq[,5]) #Regression
plot(fit)

#Covariances Education Inequality
cov(ineq [,2],ineq[,4])
plot(ineq[,2]~ineq[,4],main="Covariance of gender inequality and HDI",xlab="Inequality in life expectancy", ylab="HDI")
#Regression
fit=lm(ineq[,2]~ineq[,4])

#Plot and do a linnear regression
lml=lm(ineq[,2]~ineq[,3] - 1)
summary(lml)
plot(ineq[,2],ineq[,3],type='p',col=8,pch=1,)
abline(ineq[,2],fitted(lml,col=2,lwd=2)??
lines
ineq

#Three dimentional plots
ineq1=ineq$"HDI.rank"=NULL
lm2<-lm(ineq$HDI.Value ~ ineq$Ineq.life.expectancy+ineq$ineq.educ)
install.packages("scatterplot3d")
library(scatterplot3d)
s3d=scatterplot3d(ineq,type="h",highlight.3d=TRUE,angle=55,pch=20,lwd=1,tick.marks=TRUE,col.grid="red",main="scatterplot3d-5")
ineq.g20

head(ineq)
data.frame(ineq.g20, mm.oppo.df[,1],mm.nascent.df[,1],mm.fem.df[,1],mm.capab.df[,1])
