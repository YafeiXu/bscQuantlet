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
col="green",

xlab = "x1", ylab = "x2", zlab = "f(x1,x2)",front="fill",back="line",alpha=0.9,
ticktype = "detailed",aspect=c(1,1,1.3))
box3d()
# 2.336762 2.223286 0.136698
points3d(x=2.336762, y = 2.223286, z =0.136698,col=2,size=155)

# 0.7679166 3.977475 0.1180093
points3d(x=0.7679166, y = 3.977475, z =0.1180093,col=2,size=155)

# 3.946424 4.059899 0.136698
points3d(x=3.946424, y = 4.059899, z =0.136698,col=2,size=155)

# 0.8721164 0.7346915 0.07819546 
points3d(x=0.8721164, y = 0.7346915, z =0.07819546 ,col=2,size=155)

# 3.834899 0.6115384 0.03794587 
points3d(x=3.834899, y = 0.6115384, z =0.03794587  ,col=2,size=155)

# 0.7679163 2.305711 -0.1180093  
points3d(x=0.7679163, y =2.305711, z =-0.1180093  ,col="blue",size=155)

# 2.336761 4.059899 -0.136698
points3d(x=2.336761, y =4.059899, z =-0.136698  ,col="blue",size=155)

# 2.44829 0.6115371 -0.03794587 
points3d(x=2.44829, y =0.6115371, z =-0.03794587   ,col="blue",size=155)

# 3.946424 2.223286 -0.136698
points3d(x=3.946424, y =2.223286, z =-0.136698  ,col="blue",size=155)

### optimization 5 maxima and 4 minima 



################
library(optimx)
fkt <- function (x){0.03*sin(x[1])*sin(x[2])-0.05*sin(2*x[1])*sin(x[2])+0.01*sin(x[1])*sin(2*x[2])+0.09*sin(2*x[1])*sin(2*x[2])}

optimx(c(2.1,2.1), fkt, lower=c(2,2), upper=c(3,3),
method=c("L-BFGS-B"),
control=list(maximize = TRUE),
)

# result
#  Maximizing -- use negfn and neggr
#                 p1       p2    value fevals gevals niter convcode kkt1 kkt2 xtimes
#  L-BFGS-B 2.336762 2.223286 0.136698      7      7    NA        0 TRUE TRUE      0



################
library(optimx)
fkt <- function (x){0.03*sin(x[1])*sin(x[2])-0.05*sin(2*x[1])*sin(x[2])+0.01*sin(x[1])*sin(2*x[2])+0.09*sin(2*x[1])*sin(2*x[2])}

optimx(c(.5,4), fkt, lower=c(0,3), upper=c(1,5),
method=c("L-BFGS-B"),
control=list(maximize = TRUE),
)

# result
#  Maximizing -- use negfn and neggr
#                  p1       p2     value fevals gevals niter convcode kkt1 kkt2 xtimes
#  L-BFGS-B 0.7679166 3.977475 0.1180093      6      6    NA        0 TRUE TRUE      0

################
library(optimx)
fkt <- function (x){0.03*sin(x[1])*sin(x[2])-0.05*sin(2*x[1])*sin(x[2])+0.01*sin(x[1])*sin(2*x[2])+0.09*sin(2*x[1])*sin(2*x[2])}

optimx(c(3.5,4.1), fkt, lower=c(3.4,3.5), upper=c(4.5,4.5),
method=c("L-BFGS-B"),
control=list(maximize = TRUE),
)

# result
#  Maximizing -- use negfn and neggr
#                 p1       p2    value fevals gevals niter convcode kkt1 kkt2 xtimes
#  L-BFGS-B 3.946424 4.059899 0.136698      7      7    NA        0 TRUE TRUE      0


################
library(optimx)
fkt <- function (x){0.03*sin(x[1])*sin(x[2])-0.05*sin(2*x[1])*sin(x[2])+0.01*sin(x[1])*sin(2*x[2])+0.09*sin(2*x[1])*sin(2*x[2])}

optimx(c(.5,.5), fkt, lower=c(.5,0), upper=c(1.5,1),
method=c("L-BFGS-B"),
control=list(maximize = TRUE),
)

# result
#  Maximizing -- use negfn and neggr
#                  p1        p2      value fevals gevals niter convcode kkt1 kkt2 xtimes
#  L-BFGS-B 0.8721164 0.7346915 0.07819546      7      7    NA        0 TRUE TRUE      0

################
library(optimx)
fkt <- function (x){0.03*sin(x[1])*sin(x[2])-0.05*sin(2*x[1])*sin(x[2])+0.01*sin(x[1])*sin(2*x[2])+0.09*sin(2*x[1])*sin(2*x[2])}

optimx(c(4,.5), fkt, lower=c(3,0), upper=c(5,1),
method=c("L-BFGS-B"),
control=list(maximize = TRUE),
)

# result
#  Maximizing -- use negfn and neggr
#                 p1        p2      value fevals gevals niter convcode kkt1 kkt2 xtimes
#  L-BFGS-B 3.834899 0.6115384 0.03794587      6      6    NA        0 TRUE TRUE      0


################
library(optimx)
fkt <- function (x){0.03*sin(x[1])*sin(x[2])-0.05*sin(2*x[1])*sin(x[2])+0.01*sin(x[1])*sin(2*x[2])+0.09*sin(2*x[1])*sin(2*x[2])}

optimx(c(.5,2.1), fkt, lower=c(0,1), upper=c(2,3),
method=c("L-BFGS-B"),
control=list(maximize = F),
)

# resullt
#                  p1       p2      value fevals gevals niter convcode kkt1 kkt2 xtimes
#  L-BFGS-B 0.7679163 2.305711 -0.1180093      6      6    NA        0 TRUE TRUE      0


################
library(optimx)
fkt <- function (x){0.03*sin(x[1])*sin(x[2])-0.05*sin(2*x[1])*sin(x[2])+0.01*sin(x[1])*sin(2*x[2])+0.09*sin(2*x[1])*sin(2*x[2])}

optimx(c(2.5,4.5), fkt, lower=c(2,3.5), upper=c(3,4.5),
method=c("L-BFGS-B"),
control=list(maximize = F),
)
# resullt
#                 p1       p2     value fevals gevals niter convcode kkt1 kkt2 xtimes
#  L-BFGS-B 2.336761 4.059899 -0.136698      7      7    NA        0 TRUE TRUE      0


################
library(optimx)
fkt <- function (x){0.03*sin(x[1])*sin(x[2])-0.05*sin(2*x[1])*sin(x[2])+0.01*sin(x[1])*sin(2*x[2])+0.09*sin(2*x[1])*sin(2*x[2])}

optimx(c(2.5,.5), fkt, lower=c(2,0), upper=c(3,1),
method=c("L-BFGS-B"),
control=list(maximize = F),
)

# resullt
#                p1        p2       value fevals gevals niter convcode kkt1 kkt2 xtimes
#  L-BFGS-B 2.44829 0.6115371 -0.03794587      6      6    NA        0 TRUE TRUE      0


################
library(optimx)
fkt <- function (x){0.03*sin(x[1])*sin(x[2])-0.05*sin(2*x[1])*sin(x[2])+0.01*sin(x[1])*sin(2*x[2])+0.09*sin(2*x[1])*sin(2*x[2])}

optimx(c(4,2.1), fkt, lower=c(3.5,2), upper=c(4.5,3),
method=c("L-BFGS-B"),
control=list(maximize = F),
)

# resullt
#                p1       p2     value fevals gevals niter convcode kkt1 kkt2 xtimes
# L-BFGS-B 3.946424 2.223286 -0.136698      5      5    NA        0 TRUE TRUE      0


