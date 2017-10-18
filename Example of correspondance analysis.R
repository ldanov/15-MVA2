Do correspondance analysis
for (i in 1:ncol(analysis.p)){analysis.p[,i] = as.numeric(analysis.p[,i]) }
col = analysis.p[,ncol(analysis.p)]      		 # region numbers (High, Medium ,Low)
col
analysis.p
# (Northeast(blue), Midwest(green), South(cyan) and West(red))
# Northeast (square), Midwest (circle), South (triangle) and West (cross)
color = col
color
color[color==1]=1
color[color==2]=2
color[color==3]=3

col = as.numeric(col)+c(rep(-1,37),rep(0,13))
analysis.pc = analysis.p[,6:12]
# labels for regions
col
xrows= (row.names(analysis.pc))
analysis.p
xrows
xcols=c("oppo","nascent","female","capabilities","ebor","success","necessity")
colnames(analysis.pc) = xcols
rownames(analysis.pc) = xrows
head(analysis.pc)
head(analysis.p)
wak   = 1                      # set to 0/1 to ex/include Alaska!!
wak   = c(rep(1,nrow(analy)-2),wak,1)
analysis.pc     = subset(analysis.pc,wak==1)  
xrows = subset(xrows,wak==1)
col   = subset(col,wak==1)   
a     = rowSums(analysis.pc,na.rm=TRUE)
a
b     = colSums(analysis.pc, na.rm=TRUE)
b
e     = as.matrix(a)%*%b/sum(a)
e
analysis.pc=na.omit(analysis.pc)
#chi-matrix
cc    = (analysis.pc-e)/sqrt(e)
cc
#singular value decomposition
sv    = svd(cc)
g     = sv$u
l     = sv$d
d     = sv$v
#eigenvalues
ll    = l*l
#cumulated percentage of the variance
aux   = cumsum(ll)/sum(ll)
perc  = cbind(ll,aux)
r1    = matrix(as.matrix(l),nrow=nrow(g),ncol=ncol(g),byrow=T)*g #multiplies each column of g with each corresponding element of l
r     = (r1/matrix(sqrt(as.matrix(a)),nrow=nrow(g),ncol=ncol(g),byrow=F))*(-1) #divides each row of r1 with each corresponding element of sqrt(a)
s1    = matrix(l,nrow=nrow(d),ncol=ncol(d),byrow=T)*d #multiplies each column of d with each corresponding element of l
s     = (s1/matrix(sqrt(b),nrow=nrow(d),ncol=ncol(d),byrow=F))*(-1) #divides each row of s1 with each corresponding element of sqrt(b)
car   = matrix(matrix(a),nrow=nrow(r),ncol=ncol(r),byrow=F)*r^2/matrix(l^2,nrow=nrow(r),ncol=ncol(r),byrow=T) #contribution in r
cas   = matrix(matrix(b),nrow=nrow(s),ncol=ncol(s),byrow=F)*s^2/matrix(l^2,nrow=nrow(s),ncol=ncol(s),byrow=T) #contribution in s
rr = t(t(r[,1:2])*sign(r[50,1:2]))
ss = t(t(s[,1:2])*sign(s[7,1:2]))
plot(rr[,1],rr[,2],xlab=expression(list(r[1],s[1])),ylab=expression(list(r[2],s[2])),main="Entrepreneurial",pch=c(col), col=color,ylim=c(min(rr[,2],ss[,2]),max(rr[,2],ss[,2])),xlim=c(min(rr[,1],ss[,1]),max(rr[,1],ss[,1])))
text(ss[,1],ss[,2],xcols,cex=1.0,col="red",xpd=NA)
points(ss[,1],ss[,2],type="n")
text(rr[,1],rr[,2],xrows,col="black",pos=4,xpd=NA,cex=0.5)
abline(h=0,v=0,lwd=2)



