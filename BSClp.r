###--- BSClp by Xu Yafei HU Berlin

library(rgl)

ng=33 # number of grid
x=seq(0,200,length.out=ng)  # grid elements
y=seq(0,200,length.out=ng)
###  
fhat=function(x,y){
v=numeric()
v=2*x+4*y
return(v)
}
###  
outer931=outer(x,y,fhat)

###  
open3d()
persp3d(x,y, outer931,
theta =35+180, phi = 35+30,
col="blue",
xlab = "x1", ylab = "x2", zlab = "f(x,y)",front="fill",back="line",alpha=0.9,
ticktype = "detailed",aspect=c(1,1,1.3))

box3d() 


