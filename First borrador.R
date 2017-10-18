### let's try to import all the data
##established business ownership rates
ebor = read.csv(file ="ebor.csv" , sep = ";", head = TRUE)
ebor
#Eliminate the - sign and substitute it by a numeric NA
ebor[ebor=="-"]=NA

#Translate the data to Numeric variables
ebor[,2:15]<- sapply(ebor[,2:15],as.character)
ebor[,2:15] = sapply(ebor[,2:15], as.numeric)
ebor
#Name the Columns
ebor.names = c("country", "2001":"2014")
colnames(ebor) = ebor.names

#Eliminate the non analyzed nations and create a row vector
g20 = factor(c("Argentina", "Australia", "Brazil", "Canada", "China", "France", "Germany", "India", "Indonesia", "Italy", "Japan", "Korea (South)", "Mexico", "Russia", "Saudi Arabia", "South Africa", "Turkey", "United Kingdom", "United States of America"))
ebor.g20 = subset(ebor, ebor$country == "Argentina" | ebor$country == "Australia" | ebor$country == "Brazil" | ebor$country == "Canada" | ebor$country == "China" | ebor$country == "France" | ebor$country == "Germany" | ebor$country == "India" | ebor$country == "Indonesia" | ebor$country =="Italy" | ebor$country == "Japan" | ebor$country == "Korea (South)" | ebor$country == "Mexico" | ebor$country == "Russia" | ebor$country == "Saudi Arabia" | ebor$country == "South Africa" | ebor$country == "Turkey" | ebor$country == "United Kingdom" | ebor$country == "United States of America")
ebor.g20

#determine the rows
rownames(ebor.g20)=g20
ebor.g20$country=NULL
ebor.g20t=t(ebor.g20)
ebor.g20

#Summary the statistics of ebor.g20
rowSummary(ebor.g20)


#Determine the mean 
mean(ebor.g20$"2001", na.rm=TRUE)
mean(ebor.g20t[,1])
means1=c(colMeans(ebor.g20t[,1:19], na.rm=TRUE))
means.ebor=c(rowMeans(ebor.g20,na.rm=TRUE))
means.ebor


install.packages("matrixStats")
library("matrixStats")
median.ebor=c(colMedians(ebor.g20t,na.rm=TRUE))
mm.ebor.df=data.frame(means.ebor,median.ebor)
mm.ebor.df

subset(mm.ebor.df,mm.ebor.df$means.ebor < mean(mm.ebor.df[,1]))
subset(mm.ebor.df,mm.ebor.df$means.ebor > mean(mm.ebor.df[,1]))
subset(mm.ebor.df,mm.ebor.df$median.ebor < median(mm.ebor.df[,2]))
subset(mm.ebor.df,mm.ebor.df$median.ebor > median(mm.ebor.df[,2]))

hist(mm.ebor.df$means.ebor,freq=FALSE,xlab='country means', main=NULL)
g=seq(min(mm.ebor.df$means.ebor),max(mm.ebor.df$means.ebor),length=100)
dens.nrom=dnorm(g,mean(mm.ebor.df$means.ebor),sd(mm.ebor.df$means.ebor))
lines(g,dens.nrom,col="red3", lwd=2)

rownames(mm.ebor.df[which(mm.ebor.df[,1] == min(mm.ebor.df[,1])),])
rownames(mm.ebor.df[which(mm.ebor.df[,1] == max(mm.ebor.df[,1])),])
rownames(mm.ebor.df[which(mm.ebor.df[,2] == max(mm.ebor.df[,2])),])
rownames(mm.ebor.df[which(mm.ebor.df[,2] == min(mm.ebor.df[,2])),])
mm.ebor.df

mm.ebor.df
