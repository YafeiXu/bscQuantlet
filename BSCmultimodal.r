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

xlab = "x1", ylab = "x2", zlab = "f(x1,x2)",front="fill",back="line",alpha=0.9,
ticktype = "detailed",aspect=c(1,1,1.3))
box3d()