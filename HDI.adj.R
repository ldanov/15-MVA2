#Download the csv file
HDI.adj = read.csv(file ="HDI.adj.csv" , sep = ";", head = TRUE)
HDI.adj
#Substitute Missin Information
HDI.adj[HDI.adj==".."]=NA
HDI.adj
#Translate into numeric Data
HDI.adj[,1] = sapply(HDI.adj[,1], as.character)
HDI.adj[,1] = sapply(HDI.adj[,1], as.numeric)
HDI.adj[,3] = sapply(HDI.adj[,3], as.character)
HDI.adj[,3] = sapply(HDI.adj[,3], as.numeric)
HDI.adj[,4] = sapply(HDI.adj[,4], as.character)
HDI.adj[,4] = sapply(HDI.adj[,4], as.numeric)
HDI.adj

is.numeric(HDI.adj[,2])
is.atomic(HDI.adj[,2])


HDI.adj[,2]= as.list(HDI.adj[,2])


#Name the columns
HDI.adj.names=c("HDI.rank","country","HDI","HDI.adj")
colnames(HDI.adj)=HDI.adj.names
head(HDI.adj)

##Eliminate the non analyzed nations and create a row vector
g20 = factor(c("Argentina", "Australia", "Brazil", "Canada", "China", "France", "Germany", "India", "Indonesia", "Italy", "Japan", "Korea (South)", "Mexico", "Russia", "Saudi Arabia", "South Africa", "Turkey", "United Kingdom", "United States of America"))
HDI.adj.g20 = subset(HDI.adj, HDI.adj$country == "Argentina" | HDI.adj$country == "Australia" | HDI.adj$country == "Brazil" | HDI.adj$country == "Canada" | HDI.adj$country == "China" | HDI.adj$country == "France" | HDI.adj$country == "Germany" | HDI.adj$country == "India" | HDI.adj$country == "Indonesia" | HDI.adj$country =="Italy" | HDI.adj$country == "Japan" | HDI.adj$country == "Korea (Republic of)" | HDI.adj$country == "Mexico" | HDI.adj$country == "Russian Federation" | HDI.adj$country == "Saudi Arabia" | HDI.adj$country == "South Africa" | HDI.adj$country == "Turkey" | HDI.adj$country == "United Kingdom" | HDI.adj$country == "United States")
HDI.adj.g20

#Summary statistics
summary(HDI.adj.g20)

#determine the rows
rownames(HDI.adj.g20)=g20
HDI.adj.g20$country=NULL
HDI.adj.g20

#Transposed frame
HDI.adj.g20t=t(HDI.adj.g20)

#Compare Data
subset(HDI.adj.g20,HDI.adj.g20$Value < mean(HDI.adj.g20[,2],na.rm=TRUE))
subset(HDI.adj.g20,HDI.adj.g20$Value > mean(HDI.adj.g20[,2],na.rm=TRUE))
subset(HDI.adj.g20,HDI.adj.g20$Value < median(HDI.adj.g20[,2],na.rm=TRUE))
subset(HDI.adj.g20,HDI.adj.g20$Value > median(HDI.adj.g20[,2],na.rm=TRUE))

# Determination of the values distribution
HDI.adj.g19=HDI.adj.g20[-c(15),]
hist(HDI.adj.g19$Value,freq=FALSE,xlab='HDI.adj',main="Distribution of HDI.adj means g20 countries")
g=seq(min(HDI.adj.g19$Value),max(HDI.adj.g19$Value),length=100)
dens.nrom=dnorm(g,mean(HDI.adj.g19$Value),sd(HDI.adj.g19$Value))
lines(g,dens.nrom,col="red3", lwd=2)

#Which are the countries with the lower values
rownames(HDI.adj.g20[which(HDI.adj.g20[,1] == min(HDI.adj.g20[,1])),])
rownames(HDI.adj.g20[which(HDI.adj.g20[,1] == max(HDI.adj.g20[,1])),])
rownames(HDI.adj.g20[which(HDI.adj.g20[,2] == max(HDI.adj.g20[,2],na.rm=TRUE)),])
rownames(HDI.adj.g20[which(HDI.adj.g20[,2] == min(HDI.adj.g20[,2],na.rm=TRUE)),])
