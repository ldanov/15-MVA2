# Cluster analysis for the sample
summary(analysis)
colnames (analysis)
analysis<-subset(analysis, select=-classif.g20)
Cluster.data.s=scale(analysis)
scaledAnalysis=scale(analysis)
summary(scaledAnalysis)
summary(analysis)
group1 <- subset(scaledAnalysis, select = c(HDI.Value) )
group2 <- subset(scaledAnalysis, select = c(Ineq.life.expectancy, ineq.educ ,  ineq.income) )
group3 <- subset(scaledAnalysis, select = c(mm.oppo.df...1. , mm.capab.df...1. , mm.ebor.df...1. ,  mm.success.df...1. , mm.nascent.df...1.) )


distance1=dist(group1, method="manhattan")
distance2=dist(group2, method="manhattan")
distance3=dist(group3, method="manhattan")

#https://stat.ethz.ch/R-manual/R-devel/library/cluster/html/agnes.html
#Group 1 Manhattan Distances, different Algorithms
plot(hclust(distance1, method = "ward.D"), main = "Ward D linkage")
plot(hclust(distance1, method = "ward.D2"), main = "Ward D2 linkage")
plot(hclust(distance1, method = "single"), main = "Single linkage")
plot(hclust(distance1, method = "complete"), main = "Complete linkage")
plot(hclust(distance1, method = "average"), main = "Average linkage")
plot(hclust(distance1, method = "mcquitty"), main = "MCquitty linkage")
plot(hclust(distance1, method = "median"), main = "Median linkage")
plot(hclust(distance1, method = "centroid"), main = "Centroid")

#Group 2 Manhattan Distances, different Algorithms
plot(hclust(distance2, method = "ward.D"), main = "Ward D linkage")
plot(hclust(distance2, method = "ward.D2"), main = "Ward D2 linkage")
plot(hclust(distance2, method = "single"), main = "Single linkage")
plot(hclust(distance2, method = "complete"), main = "Complete linkage")
plot(hclust(distance2, method = "average"), main = "Average linkage")
plot(hclust(distance2, method = "mcquitty"), main = "MCquitty linkage")
plot(hclust(distance2, method = "median"), main = "Median linkage")
plot(hclust(distance2, method = "centroid"), main = "Centroid")

#Group 3 Manhattan Distances, different Algorithms
plot(hclust(distance3, method = "ward.D"), main = "Ward D linkage")
plot(hclust(distance3, method = "ward.D2"), main = "Ward D2 linkage")
plot(hclust(distance3, method = "single"), main = "Single linkage")
plot(hclust(distance3, method = "complete"), main = "Complete linkage")
plot(hclust(distance3, method = "average"), main = "Average linkage")
plot(hclust(distance3, method = "mcquitty"), main = "MCquitty linkage")
plot(hclust(distance3, method = "median"), main = "Median linkage")
plot(hclust(distance3, method = "centroid"), main = "Centroid")
