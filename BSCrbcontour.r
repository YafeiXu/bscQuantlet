###--- BSCrbcontour by Xu Yafei HU Berlin

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
contour(x,y, outer931,xlab = "x", ylab = "y",
        xlim = range(-2,2, finite = TRUE),
        ylim = range(-1,3, finite = TRUE),
        zlim = range(0,6, finite = TRUE),levels=c(seq(0,6,length.out=10),seq(6,60,length.out=8),seq(60,3000,length.out=60))) 
points(1,1,pch=3,cex=15,col="red")
points(1,1,pch=10,cex=15,col="red")
text(0,1.4,"Global Minimum Point (1,1)")