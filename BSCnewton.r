# BSCnewton

install.packages("animation")
library(animation)
newt=newton.method(FUN = function(x) x^2 - 4,
init = 10, rg = c(-1, 10), tol = 0.001, 
interact = FALSE, col.lp = c("blue", "red", "red"))
newt$root
newt$value	