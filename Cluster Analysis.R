# Cluster analysis for the population.
Cluster.data=scale(analysis.p)
head(analysis.p)
d=dist(Cluster.data, method="euclidean")
d
fit.ward=hclust(d, method="ward.D")
plot(fit)

# Cluster analysis for the sample
Cluster.data.s=scale(analysis)
print(analysis      )
head(analysis)
d.s=dist(Cluster.data.s, method="euclidean")
d
fit.s=hclust(d.s, method="ward.D")
plot(fit.s)
summary(analysis)


plot(hclust(d.s, method = "single"), main = "Single linkage")
plot(hclust(d.s, method = "complete"), main = "Complete linkage")
plot(hclust(d.s, method = "average"), main = "Average linkage")

# PAM PLOTS
pam=pam(analysis, 3)
plot(pam)

#aGLOMERATIVE NESTING
d.an.1=dist(analysis)
d.an=daisy(analysis)
sum(d.an.1-d.an)
an=agnes(d.an.1)
plot(an)
table(cutree(an,3), analysis$classif.g20)
splom(~analysis, groups=analysis$classif.g20, auto.key=TRUE)

# MODEL BASED CLUSTERING
analysis
fit.s.m=Mclust(na.exclude(analysis[,1]))
fit.s.m
plot(fit.s.m, pch=c(col.g20), col=col.g20)
2
summary(fit.s.m)


#Ploting cluster solutions
# K-Means Clustering with 5 clusters
fit.cp <- kmeans(na.omit(analysis.m, 1))
analysis.m=as.matrix(analysis)
head(analysis.m)
is.matrix(analysis.m)
# Cluster Plot against 1st 2 principal components

# vary parameters for most readable graph
library(cluster) 
clusplot(analysis, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions
library(fpc)
plotcluster(mydata, fit$cluster)


