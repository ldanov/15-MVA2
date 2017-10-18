head(ineq)
vector=rownames(ineq)

vector[62]="Malaysia"
vector[64]="Trinidad & Tobago"
vector[5]="United States of America"
vector[113]="Bolivia"
vector[86]="Bosnia & Herzegovina"
vector[15]="Hong Kong"
vector[75]="Iran"
vector[16]= "Korea (South)"
vector[85]= "Macedonia" 
vector[107]="Palestine" 
vector[57]="Russia"
vector[38]="Slovak Republic" 
vector[119]="Syria"
vector[122]="Vietnam"
vector[67]="Venezuela"  

analysis=data.frame(ineq, vector)
fitted.ineq = subset(analysis, vector%in%rownames(oppo))
fitted.ineq
dim(fitted.ineq)

Dif=setdiff(rownames(oppo), fitted.ineq$vector)
Dif=Dif[order(Dif,decreasing=FALSE)]
Dif

classif.g20=c("high","high","Medium","high","Medium","high","high","Low","Low","high","high","high","Medium","Medium","high","Low","Medium","high","high")
Separate in groups according to HDI
x="high"
y="medium"
z="low"
xc=rep(x,41)
xy=rep(y,33)
xz=rep(z,26)
classif=c(xc,xy,xz)
analysis.p[,13]=as.numeric

xg20=

analysis.p=data.frame(fitted.ineq,mm.femratio.a[,1], mm.oppo.a[,1],mm.nascent.a[,1],mm.fem.a[,1],mm.capab.a[,1],mm.ebor.a[,1],mm.success.a[,1],mm.necessity.a[,1],classif)
head(analysis.p)
analysis.p$vector=NULL
analysis.p$Quantil=NULL
is.numeric(analysis.p)
head(analysis.p)
cor=cor(na.omit(analysis.p))
pairs(na.omit(analysis.p))
boxplot(analysis.p)
cork=cor(na.omit(analysis.p), method="kendall")
cors=cor(na.omit(analysis.p), method="spearman")

cov.oppo=cov(analysis.p [,1],analysis.p[,6], use="complete.obs")
cov.nascent=cov(analysis.p [,1],analysis.p[,7], use="complete.obs")
cov.fem=cov(analysis.p [,1],analysis.p[,8], use="complete.obs")
cov.capab=cov(analysis.p [,1],analysis.p[,9], use="complete.obs")
cov.ebor=cov(analysis.p [,1],analysis.p[,10], use="complete.obs")
cov.ebor=cov(analysis.p [,1],analysis.p[,11], use="complete.obs")
cov.ebor=cov(analysis.p [,1],analysis.p[,12], use="complete.obs")
plot(analysis.p[,1]~analysis.p[,6],main="Covariance of HDI and Perseived oportunities",xlab="persieved oportunities", ylab="HDI")   
plot(analysis.p[,1]~analysis.p[,7],main="Covariance of HDI and nascent entrepreneurship",xlab="Nascent Entrepreneurship", ylab="HDI") 
plot(analysis.p[,1]~analysis.p[,8],main="Covariance of HDI and Female Entrepreneurship",xlab="Female Entrepreneurship", ylab="HDI") 
plot(analysis.p[,1]~analysis.p[,9],main="Covariance of HDI and perseived capabilities",xlab="persieved capabilities", ylab="HDI") 
plot(analysis.p[,1]~analysis.p[,10],main="Covariance of HDI and Business Ownership Rate",xlab="Business Ownership Rate", ylab="HDI") 
plot(analysis.p[,1]~analysis.p[,11],main="Covariance of HDI and Regard For Success",xlab="Regard for success", ylab="HDI") 
plot(analysis.p[,1]~analysis.p[,12],main="Covariance of HDI and necessity driven entrepreneurship",xlab="Necessity Entrepreneurship", ylab="HDI") 
fit=lm(ineq[,5]~ineq[,1])
abline(fit)

analysis=data.frame(ineq.g20,mm.femratio.df[,1], mm.oppo.df[,1],mm.nascent.df[,1],mm.fem.df[,1],mm.capab.df[,1],mm.ebor.df[,1],mm.success.df[,1],mm.necessity.df[,1],classif.g20)
analysis[,1]=NULL
analysis[,5]=NULL
head(analysis.p)
head(analysis)
analysis
analysis1 = subset(ineq, ineq$country == "Argentina" | ineq$country == "Australia" | ineq$country == "Brazil" | ineq$country == "Canada" | ineq$country == "China" | ineq$country == "France" | ineq$country == "Germany" | ineq$country == "India" | ineq$country == "Indonesia" | ineq$country =="Italy" | ineq$country == "Japan" | ineq$country == "Korea (Republic of)" | ineq$country == "Mexico" | ineq$country == "Russian Federation" | ineq$country == "Saudi Arabia" | ineq$country == "South Africa" | ineq$country == "Turkey" | ineq$country == "United Kingdom" | ineq$country == "United States")
#Correlation Matrix 
cor.analysis=cor(analysis$HDI.Value,ineq.g20$Ineq.life.expectancy)
cor.HDI.LE=cor(na.omit(analysis))
cor.HDI.LE
#Covariances Education Inequality
cov(analysis [,4],analysis[,6], use="complete.obs")
plot(na.omit(analysis[,4]~analysis[,6], data=analysis, main="Covariance of gender inequality and HDI",xlab="Inequality in life expectancy", ylab="HDI"))
#Regression
plot.lm(ineq[,2]~ineq[,4]) ??

library(lattice)
#Build the plot
rgb.palette
rgb.palette <- colorRampPalette(c("black", "yellow","red"), space = "rgb")
levelplot(cor, main=" correlation matrix", scales=list(x=list(rot=90)), xlab="", ylab="", col.regions=rgb.palette(10000), cuts=200, at=seq(-1,1,0.01))
#Remember las = 
cor
rgb.palette <- colorRampPalette(c("green", "black"), space = "Lab")
levelplot(cor.HDI.LE, main=" correlation matrix", xlab="", ylab="", col.regions=rgb.palette(200), cuts=100, at=seq(-1,1,0.01))

#Correlation Tests
cor.test(analysis.p [,1],analysis.p[,6],, alternative = c("two.sided", "less", "greater"),method = c("pearson"),exact = NULL, conf.level = 0.95, continuity = FALSE)

cor.test(analysis.p [,1],analysis.p[,6],, alternative = c("two.sided", "less", "greater"),method = c("spearman"),exact = NULL, conf.level = 0.95, continuity = FALSE)
cor.test(analysis.p [,1],analysis.p[,6],, alternative = c("two.sided", "less", "greater"),method = c("kendall"),exact = NULL, conf.level = 0.95, continuity = FALSE)

regression.HDI = analysis[,1:4]

regression.HDI=lm(analysis.p[,1]~analysis.p[,2]*analysis.p[,3]*analysis.p[,4])
regression.HDI
plot(regression.HDI)

