###--- BSCinterand by Xu Yafei HU Berlin

library(rgl)
ng=33 # number of grid
x=seq(0,1,length.out=ng)  # grid elements
y=seq(0,1,length.out=ng)
###  
fhat=function(x,y){
v=numeric()
v=x*x*y*y
return(v)
}
###  
outer931=outer(x,y,fhat)

###  
open3d()
par3d(cex=3) #  
persp3d(x,y, outer931,
theta =35, phi = 35,
col="blue",
xlab = "x", ylab = "y", zlab = "(x*y)^2",front="fill",back="line",alpha=0.9,
ticktype = "detailed",aspect=c(1,1,1.3), cex.axis=3,cex.lab=5)
#grid3d(c("x", "y+", "z"),alpha=0.7)
axes3d(yat=c(0,.3,.6,.9),xat=c(0,.3,.6,.9),cex=11) 
box3d()#加框


