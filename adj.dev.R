#Download the csv file
dev.adj = read.csv(file ="dev.adj.csv" , sep = ";", head = TRUE)
head(dev.adj)

#Substitute Missin Information
dev.adj[dev.adj==".."]=NA
dev.adj

#Translate into numeric Data
dev.adj[,1] = sapply(dev.adj[,1], as.character)
dev.adj[,1] = sapply(dev.adj[,1], as.numeric)
dev.adj[,3] = sapply(dev.adj[,3], as.character)
dev.adj[,3] = sapply(dev.adj[,3], as.numeric)
dev.adj[,4] = sapply(dev.adj[,4], as.character)
dev.adj[,4] = sapply(dev.adj[,4], as.numeric)
is.atomic(dev.adj[,2])

#Name the columns
dev.adj.names=c("HDI.Rank","country","HDI","adjustment")
colnames(dev.adj)=dev.adj.names
head(dev.adj)
is.atomic(dev.adj[,2])

##Eliminate the non analyzed nations and create a row vector
dev.adj.g20 = subset(dev.adj, dev.adj$country == "Argentina" | dev.adj$country == "Australia" | dev.adj$country == "Brazil" | dev.adj$country == "Canada" | dev.adj$country == "China" | dev.adj$country == "France" | dev.adj$country == "Germany" | dev.adj$country == "India" | dev.adj$country == "Indonesia" | dev.adj$country =="Italy" | dev.adj$country == "Japan" | dev.adj$country == "Korea (Republic of)" | dev.adj$country == "Mexico" | dev.adj$country == "Russian Federation" | dev.adj$country == "Saudi Arabia" | dev.adj$country == "South Africa" | dev.adj$country == "Turkey" | dev.adj$country == "United Kingdom" | dev.adj$country == "United States")
head(dev.adj.g20)

#Sort alfabetically
dev.adj.g20=dev.adj.g20[order(dev.adj.g20$"country",decreasing=FALSE),]
dev.adj.g20

#Summary statistics
summary(dev.adj.g20)
sd(dev.adj.g20[,2],na.rm=TRUE)
sd(dev.adj.g20[,3],na.rm=TRUE)

#determine the rows
g20 = factor(c("Argentina", "Australia", "Brazil", "Canada", "China", "France", "Germany", "India", "Indonesia", "Italy", "Japan", "Korea (South)", "Mexico", "Russia", "Saudi Arabia", "South Africa", "Turkey", "United Kingdom", "United States of America"))
rownames(dev.adj.g20)=g20
dev.adj.g20$country=NULL
dev.adj.g20

rownames(dev.adj)=dev.adj$"country"
dev.adj$country=NULL
dev.adj

#Transposed frame
dev.adj.g20t=t(dev.adj.g20)

#Compare Data
subset(dev.adj.g20,dev.adj.g20$HDI < mean(dev.adj.g20[,2],na.rm=TRUE))
subset(dev.adj.g20,dev.adj.g20$HDI > mean(dev.adj.g20[,2],na.rm=TRUE))
subset(dev.adj.g20,dev.adj.g20$HDI < median(dev.adj.g20[,2],na.rm=TRUE))
subset(dev.adj.g20,dev.adj.g20$HDI> median(dev.adj.g20[,2],na.rm=TRUE))

subset(dev.adj.g20,dev.adj.g20$adjustment < mean(dev.adj.g20[,3],na.rm=TRUE))
subset(dev.adj.g20,dev.adj.g20$adjustment> mean(dev.adj.g20[,3],na.rm=TRUE))
subset(dev.adj.g20,dev.adj.g20$adjustment < median(dev.adj.g20[,3],na.rm=TRUE))
subset(dev.adj.g20,dev.adj.g20$adjustment > median(dev.adj.g20[,3],na.rm=TRUE))

dev.adj.g20
# Determination of the values distribution

hist(dev.adj.g20$HDI,freq=FALSE,xlab='HDI',main="HDI Distribution")
g=seq(min(dev.adj.g20$HDI),max(dev.adj.g20$HDI),length=100)
dens.nrom=dnorm(g,mean(dev.adj.g20$HDI),sd(dev.adj.g20$HDI))
lines(g,dens.nrom,col="red3", lwd=2)

hist(dev.adj.g20$adjustment,freq=FALSE,xlab='Years',main="Life Expectancy")
g=seq(na.omit(min(dev.adj.g20$adjustment),max(dev.adj.g20$adjustment),length=100))
dens.nrom=dnorm(na.omit(g,mean(dev.adj$adjustment),sd(dev.adj$adjustment)))
lines(g,dens.nrom,col="red3", lwd=2)

hist(dev.adj$HDI,freq=FALSE,xlab='HDI',main="HDI Distribution")
g=seq(omit.na(min(dev.adj$HDI),max(dev.adj$HDI),length=100))
dens.nrom=dnorm(g,mean(dev.adj.g20$HDI),sd(dev.adj.g20$HDI))
lines(g,dens.nrom,col="red3", lwd=2)



#Which are the countries with the lower values
rownames(dev.adj.g20[which(dev.adj.g20[,1] == min(dev.adj.g20[,1])),])
rownames(dev.adj.g20[which(dev.adj.g20[,1] == max(dev.adj.g20[,1])),])
rownames(dev.adj.g20[which(dev.adj.g20[,2] == max(dev.adj.g20[,2],na.rm=TRUE)),])
rownames(dev.adj.g20[which(dev.adj.g20[,2] == min(dev.adj.g20[,2],na.rm=TRUE)),])
rownames(dev.adj.g20[which(dev.adj.g20[,3] == max(dev.adj.g20[,3],na.rm=TRUE)),])
rownames(dev.adj.g20[which(dev.adj.g20[,3] == min(dev.adj.g20[,3],na.rm=TRUE)),])

dev.adj.g20
#KDE using gaussian Kernel
dev.adj.g20
density(dev.adj.g20[,2],na.rm=TRUE)
plot(density(dev.adj.g20[,2],na.rm=TRUE))
#Emperical cdf
ecdf(dev.adj.g20[,2])
plot(ecdf(dev.adj.g20[,2]))
#Mean characteristics and measures of the sample
summary(dev.adj.g20[,2]) 
boxplot(dev.adj.g20[,2])

#Boxplot comparison
boxplot(dev.adj.g20[,2:3], col="yellow",varwith=TRUE,outline=TRUE)

#Correlation Matrix 
cor.HDI.LE=cor(na.omit(dev.adj.g20))
cor.HDI.LE
dev.adj.g20

#Plot and do a linnear regression
lml=lm(dev.adj$HDI~dev.adj$adjustment-1)
summary(lml)
plot(dev.adj$HDI,dev.adj$adjustment,type='p',col=8,pch=1,)
lines(dev.adj.g20$adjustment,fitted(lml,col=2,lwd=2)
dev.adj$HDI     
      
      
#Ttest
ts=coef(lml)/sqrt(vcov(lml)) #Test statistic
cv=qt(0.9975,24-1) #Critical value at alpha=0.1,0.05

