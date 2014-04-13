###--- BSCnlp by Xu Yafei HU Berlin

library(rgl)

ng=33 # number of grid
x=seq(0,4,length.out=ng)  # grid elements
y=seq(0,3,length.out=ng)
###  
fhat=function(x,y){
v=numeric()
v=sqrt(5*x)+sqrt(3*y)
return(v)
}
###  
outer931=outer(x,y,fhat)

###  
open3d()
par3d(cex=3) # 扩大字体大小
persp3d(x,y, outer931,
theta =35+180, phi = 35+30,
col="blue",
xlab = "x1", ylab = "x2", zlab = "f(x,y)",front="fill",back="line",alpha=0.9,
ticktype = "detailed",aspect=c(1,1,1.3))

box3d() 

planes3d(a=3,b=5,c=0,d=-10,col="green")
points3d(x=2.4510595, y =0.5293643, z = 4.760952, col=2,size=155)
###


