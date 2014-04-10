###--- BSCrosenbrock by Xu Yafei HU Berlin

library(rgl)

ng=33 # number of grid
x=seq(-2,2,length.out=ng)  # grid elements
y=seq(-1,3,length.out=ng)
###  
fhat=function(x,y){
v=numeric()
v=(1-x)^3 + 100*(y-x^2)^2
return(v)
}
###  
outer931=outer(x,y,fhat)

###  
open3d()
persp3d(x,y, outer931,
theta =35+180, phi = 35+30,
col="blue",
xlab = "x", ylab = "y", zlab = "f(x,y)",front="fill",back="line",alpha=0.9,
ticktype = "detailed",aspect=c(1,1,1.3))
#grid3d(c("x", "y+", "z"),alpha=0.7)
axes3d(yat=c(-1,0,1,2,3),xat=c(-2,-1,0,1,2)) 
box3d()  
points3d(x=1,y=1,z=0,col="red",lwd=5,size=15)


