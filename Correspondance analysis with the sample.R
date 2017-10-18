Correspondance Analysis for G20 sample
for (i in 1:ncol(analysis)){analysis[,i] = as.numeric(analysis[,i]) }
col.g20 = analysis[,ncol(analysis)]           # region numbers (High, Medium ,Low)
col.g20
analysis
# (Northeast(blue), Midwest(green), South(cyan) and West(red))
# Northeast (square), Midwest (circle), South (triangle) and West (cross)
color.g20 = col.g20
color.g20
color.g20[color.g20==1]=1
color.g20[color.g20==2]=2
color.g20[color.g20==3]=3

analysis.c = analysis[,6:11]
head(analysis.c)
# labels for regions
col.g20
xrows= (row.names(analysis.c))
analysis
xrows
xcols=c("oppo","nascent","female","capabilities","ebor","success")
colnames(analysis.c) = xcols
rownames(analysis.c) = xrows
head(analysis.c)
head(analysis)

a.g20    = rowSums(analysis.c,na.rm=TRUE)
a.g20
b.g20     = colSums(analysis.c, na.rm=TRUE)
b.g20
e.g20     = as.matrix(a.g20)%*%b.g20/sum(a.g20)
e.g20
analysis.c=na.omit(analysis.c)
#chi-matrix
cc    = (analysis.c-e.g20)/sqrt(e.g20)
cc
#singular value decomposition
sv    = svd(cc)
g     = sv$u
l     = sv$d
d     = sv$v
#eigenvalues
eigen    = l*l
eigen
a.g20
r1
#cumulated percentage of the variance
aux.g20   = cumsum(eigen)/sum(eigen)
perc.g20  = cbind(eigen,aux.g20)
r1.g20    = matrix(as.matrix(l),nrow=nrow(g),ncol=ncol(g),byrow=T)*g #multiplies each column of g with each corresponding element of l
r.g20     = (r1.g20/matrix(sqrt(as.matrix(a.g20)),nrow=nrow(g),ncol=ncol(g),byrow=F))*(-1) #divides each row of r1 with each corresponding element of sqrt(a.g20)
s1.g20    = matrix(l,nrow=nrow(d),ncol=ncol(d),byrow=T)*d #multiplies each column of d with each corresponding element of l
s.g20     = (s1.g20/matrix(sqrt(b.g20),nrow=nrow(d),ncol=ncol(d),byrow=F))*(-1) #divides each row of s1 with each corresponding element of sqrt(b.g20)
car.g20   = matrix(matrix(a.g20),nrow=nrow(r.g20),ncol=ncol(r.g20),byrow=F)*r.g20^2/matrix(l^2,nrow=nrow(r.g20),ncol=ncol(r.g20),byrow=T) #contribution in r
cas.g20   = matrix(matrix(b.g20),nrow=nrow(s.g20),ncol=ncol(s.g20),byrow=F)*s.g20^2/matrix(l^2,nrow=nrow(s.g20),ncol=ncol(s.g20),byrow=T) #contribution in s
rr.g20 = t(t(r[,1:2])*sign(r[10,1:2]))
ss.g20 = t(t(s[,1:2])*sign(s[3,1:2]))
plot(rr.g20[,1],rr.g20[,2],xlab=expression(list(r.g20[1],s.g20[1])),ylab=expression(list(r.g20[2],s.g20[2])),main="Entrepreneurial",pch=c(col.g20), col=col.g20,ylim=c(min(rr.g20[,2],ss.g20[,2]),max(rr.g20[,2],ss.g20[,2])),xlim=c(min(rr.g20[,1],ss.g20[,1]),max(rr.g20[,1],ss.g20[,1])))
text(ss.g20[,1],ss.g20[,2],xcols,cex=1.0,col="red",xpd=NA)
points(ss.g20[,1],ss.g20[,2],type="n")
text(rr.g20[,1],rr.g20[,2],xrows,col="black",pos=4,xpd=NA,cex=0.5)
abline(h=0,v=0,lwd=2)
analysis.g20
