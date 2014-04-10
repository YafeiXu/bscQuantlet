###--- BSCcontour by Xu Yafei HU Berlin

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
contour(x,y, outer931,xlab = "x1", ylab = "x2", nlevels=12) 
