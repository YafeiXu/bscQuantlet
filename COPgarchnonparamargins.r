### COPgarchnonparamargins
#slide 29-6

rm(list=ls(all=TRUE))
library("HAC")
    InitValues.sim = list()
    InitValues.sim$T               = 2000
    InitValues.sim$pi              = c(0.2,0.1,0.7)# initial prob matrix
    InitValues.sim$tranmatrix      = matrix(c(0.982,0.010,0.008,0.008,0.984,0.008,0.003,0.002,0.995),3,3,byrow = TRUE)# transition matrix
    InitValues.sim$model = list()
    InitValues.sim$model[[1]] = hac(type = 1, tree = list("V3", list(V1 = "V1", V2 = "V2", theta = 1.3), theta = 1.05))
    InitValues.sim$model[[2]] = hac(type = 1, tree = list("V1", list(V1 = "V2", V2 = "V3", theta = 2.0), theta = 1.35))
    InitValues.sim$model[[3]] = hac(type = 1, tree = list("V2", list(V1 = "V1", V2 = "V3", theta = 4.5), theta = 2.85))
    InitValues.sim$M               = length(InitValues.sim$model)# number of hidden states     
    max.iter.steps = 10


isGarch = T
setwd("c:/hmm/simulationstudy/simulationstudy/try") 
load(file = paste("c:/hmm/simulationstudy/simulationstudy/HMM_Simul_", if(isGarch){"Garch"}else{"noGarch"}, "_dim3_new.dat", sep = "")) # pls load the data set 

all.ress = full.res
params = diffs = list()
for(iii in 1:length(all.ress))
for(jjj in 1:(length(all.ress[[1]]$result))){
            dif.p = dif.m = 0        
            if(!all.ress[[iii]]$result[[jjj]]$model.degenerates){
                if(all.ress[[iii]]$result[[jjj]]$correct.structures[[max.iter.steps]] == 1)
    				for(ii in 1:length(all.ress[[iii]]$result[[jjj]]$hacs[[max.iter.steps]])){
                        if(is.null(params[jjj][[1]]))params[jjj] = 0
                        params[[jjj]] = rbind(params[[jjj]], get.params(all.ress[[iii]]$result[[jjj]]$hacs[[max.iter.steps]][[ii]]))
                    }
            }
			if(all.ress[[iii]]$result[[jjj]]$iterations > 1){
				for(i in 1:length(all.ress[[iii]]$result[[jjj]]$trans.matr)){
					dif.m = c(dif.m, sum(abs(all.ress[[iii]]$result[[jjj]]$trans.matr[[i]] - InitValues.sim$tranmatrix)))
					
					if(all.ress[[iii]]$result[[jjj]]$correct.structures[i] == 1){
						dif.lp = 0
						for(ii in 1:length(all.ress[[iii]]$result[[jjj]]$hacs[[i]])){
							dif.lp = dif.lp + sum(abs(get.params(all.ress[[iii]]$result[[jjj]]$hacs[[i]][[ii]]) - get.params(InitValues.sim$model[[ii]])))
                        }
					}else{dif.lp = NA}
					dif.p = c(dif.p, dif.lp)
				}
				dif.m = dif.m[-1]
				dif.p = dif.p[-1]
                if(isGarch){
    				dif.q = as.vector(colSums(sign(abs(all.ress[[iii]]$result[[jjj]]$qstars - matrix(rep(as.vector(all.ress[[iii]]$sample$Lx_markov[-1]), dim(all.ress[[iii]]$result[[jjj]]$qstars)[2]), nrow = length(as.vector(all.ress[[iii]]$sample$Lx_markov[-1])))))))
                }else{
    				dif.q = as.vector(colSums(sign(abs(all.ress[[iii]]$result[[jjj]]$qstars - matrix(rep(as.vector(all.ress[[iii]]$sample$Lx_markov), dim(all.ress[[iii]]$result[[jjj]]$qstars)[2]), nrow = length(as.vector(all.ress[[iii]]$sample$Lx_markov)))))))
                }                
				if(length(which(all.ress[[iii]]$result[[jjj]]$correct.structures == 0)) != 0){
					dif.m[which(all.ress[[iii]]$result[[jjj]]$correct.structures == 0)] = NA
					dif.q[which(all.ress[[iii]]$result[[jjj]]$correct.structures == 0)] = NA
				}
				if(all.ress[[iii]]$result[[jjj]]$model.degenerates){
					dif.m = c(dif.m, rep(NA, max.iter.steps - length(all.ress[[iii]]$result[[jjj]]$correct.structures)))
					dif.p = c(dif.p, rep(NA, max.iter.steps - length(all.ress[[iii]]$result[[jjj]]$correct.structures)))
					dif.q = c(dif.q, rep(NA, max.iter.steps - length(all.ress[[iii]]$result[[jjj]]$correct.structures)))
				}
            }else{
                dif.p = dif.q = dif.m = rep(NA, max.iter.steps)
            }
    if(is.null(diffs[jjj][[1]])){
        diffs[[jjj]] = list()
        diffs[[jjj]]$m = dif.m
        diffs[[jjj]]$p = dif.p
        diffs[[jjj]]$q = dif.q
    }else{
        diffs[[jjj]]$m = rbind(diffs[[jjj]]$m, dif.m)
        diffs[[jjj]]$p = rbind(diffs[[jjj]]$p, dif.p)
        diffs[[jjj]]$q = rbind(diffs[[jjj]]$q, dif.q)
    }
}

for(i in 1:length(params))params[[i]] = params[[i]][-1,]
names(params) = names(all.ress[[1]]$result) # [-c(10,11)]

###
cors.str = list()
for(iii in 1:length(all.ress))
for(jjj in 1:(length(all.ress[[1]]$result))){
    if(is.null(cors.str[jjj][[1]])){
        cors.str[[jjj]] = c(all.ress[[iii]]$result[[jjj]]$correct.structures, rep(0, max.iter.steps - length(all.ress[[iii]]$result[[jjj]]$correct.structures)))
    }else{
        cors.str[[jjj]] = rbind(cors.str[[jjj]], c(all.ress[[iii]]$result[[jjj]]$correct.structures, rep(0, max.iter.steps - length(all.ress[[iii]]$result[[jjj]]$correct.structures))))
    }
}
names(cors.str) = names(all.ress[[1]]) # [-c(10,11)]
cors.str = as.data.frame(lapply(cors.str, FUN = colSums)) / length(all.ress)

appl.res = function(difff, fun, ind, ...){
    r = 0
    for(i in 1:length(difff)){
        r = rbind(r, apply(difff[[i]][[ind]], 2, FUN = fun, ...))
    }
    r[-1,]
}


lines1 = c(1, 3, 5)
lines2 = c(2, 4, 6)
alpha = 0.1
i=1
line1 = lines1[i]
line2 = lines2[i]
        
layout(t(as.matrix(1:3)))

what.dif = 1
main.line = appl.res(diffs, mean, what.dif, na.rm = TRUE)
l.q = appl.res(diffs, quantile, what.dif, na.rm = TRUE, probs = alpha)
u.q = appl.res(diffs, quantile, what.dif, na.rm = TRUE, probs = 1-alpha)
plot(main.line[line1,], ylim = range(main.line[-c(3,6,9),]), type = "l", lwd = 2, col = "black", lty = "dashed", ylab = expression(abs(abs(hat(P) - P))), xlab = "iterations")#, axes = FALSE, frame = TRUE)
axis(2)
axis(1)
lines(main.line[line1,], type = "l", lwd = 2, col = "black", lty = "dashed")
lines(main.line[line2,], type = "l", lwd = 2, col = "black", lty = "solid")
        
what.dif = 2
main.line = appl.res(diffs, mean, what.dif, na.rm = TRUE)
l.q = appl.res(diffs, quantile, what.dif, na.rm = TRUE, probs = alpha)
u.q = appl.res(diffs, quantile, what.dif, na.rm = TRUE, probs = 1-alpha)
plot(main.line[line1,], ylim = range(main.line[-c(3,6,9),]), type = "l", lwd = 2, col = "black", lty = "dashed", ylab = expression(abs(abs(hat(theta) - theta))), xlab = "iterations", axes = FALSE, frame = TRUE)
axis(1)
axis(2)
lines(main.line[line1,], type = "l", lwd = 2, col = "black", lty = "dashed")
lines(main.line[line2,], type = "l", lwd = 2, col = "black", lty = "solid")
        
what.dif = 3
main.line = 100 * appl.res(diffs, mean, what.dif, na.rm = TRUE)/InitValues.sim$T
l.q = appl.res(diffs, quantile, what.dif, na.rm = TRUE, probs = alpha)/InitValues.sim$T
u.q = appl.res(diffs, quantile, what.dif, na.rm = TRUE, probs = 1-alpha)/InitValues.sim$T
plot(main.line[line1,] * 100, ylim = range(main.line[-c(3,6,9),]), type = "l", lwd = 2, col = "black", lty = "dashed", ylab = "misspesified stated, in %", xlab = "iterations")
lines(main.line[line1,], type = "l", lwd = 2, col = "black", lty = "dashed")
lines(main.line[line2,], type = "l", lwd = 2, col = "black", lty = "solid")


