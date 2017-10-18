head(ineq)
vector=rownames(ineq)
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
ineq
fitted.ineq = subset(analysis, vector%in%rownames(oppo))
fitted.ineq
dim(fitted.ineq)

Dif=setdiff(rownames(oppo), fitted.ineq$vector)
Dif=Dif[order(Dif,decreasing=FALSE)]
Dif

analysis.p=data.frame(fitted.ineq, mm.oppo.a[,1],mm.nascent.a[,1],mm.fem.a[,1],mm.capab.a[,1])
dim(mm.capab.a)
dim(mm.fem.a)
dim(mm.nascent.a)
dim(mm.oppo.a)
dim(fitted.ineq)
cor=cor(na.omit(analysis.p))
cor
head(analysis.p)
cov(analysis.p [,4],analysis.p[,7])
plot(analysis.p[,7]~analysis.p[,9],main="Covariance of gender inequality and HDI",xlab="Inequality in life expectancy", ylab="HDI")
analysis.p      
dim(fitted.ineq)
analysis.p$vector=NULL
analysis.p

analysis.p
oppo
means.opp
oppo[=NULL
analysis=data.frame(ineq.g20, mm.oppo.df[,1],mm.nascent.df[,1],mm.fem.df[,1],mm.capab.df[,1])
head(analysis)
analysis1 = subset(ineq, ineq$country == "Argentina" | ineq$country == "Australia" | ineq$country == "Brazil" | ineq$country == "Canada" | ineq$country == "China" | ineq$country == "France" | ineq$country == "Germany" | ineq$country == "India" | ineq$country == "Indonesia" | ineq$country =="Italy" | ineq$country == "Japan" | ineq$country == "Korea (Republic of)" | ineq$country == "Mexico" | ineq$country == "Russian Federation" | ineq$country == "Saudi Arabia" | ineq$country == "South Africa" | ineq$country == "Turkey" | ineq$country == "United Kingdom" | ineq$country == "United States")
#Correlation Matrix 
cor.analysis=cor(analysis$HDI.Value,ineq.g20$Ineq.life.expectancy)
cor.HDI.LE=cor(na.omit(analysis))
cor.HDI.LE
ineq.g20

#Covariances Education Inequality
cov(analysis [,4],analysis[,7])
plot(analysis[,4]~analysis[,7],main="Covariance of gender inequality and HDI",xlab="Inequality in life expectancy", ylab="HDI")
#Regression
fit=lm(ineq[,2]~ineq[,4])