###--- BSCmultimodal by Xu Yafei HU Berlin

library(rgl)
ng=33 # number of grid
x=seq(0,5,length.out=ng)  # grid elements
y=seq(0,5,length.out=ng)
### 
fhat=function(x,y){
v=numeric()
v=0.03*sin(x)*sin(y)-0.05*sin(2*x)*sin(y)+0.01*sin(x)*sin(2*y)+0.09*sin(2*x)*sin(2*y)
return(v)
}
### 
outer931=outer(x,y,fhat)

### 
open3d()
persp3d(x,y, outer931,
theta =35, phi = 35,
col="blue",

xlab = "X", ylab = "Y", zlab = "Z",front="fill",back="line",alpha=0.9,
ticktype = "detailed",aspect=c(1,1,1.3))
#grid3d(c("x", "y+", "z"),alpha=0.7)
axes3d(yat=c(33.0,47.2,61.5),xat=c(4780.9,9288.6,13796.4))
box3d()