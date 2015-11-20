library(parallel)
library(Rdsm)
## this script is adapted from documentation of Rdsm package
## and Matloff's book:
## Parallel Computing for Data Science with Examples in R and Beyond

r = 3000
s = 1000

## define slow matrix multiplication to make timing easier
slowMult = function(M, v) {
	out = numeric(nrow(M))
	for (i in 1:nrow(M)) {
		for (j in 1:ncol(M)) {
			out[i] = out[i] + M[i, j] * v[j]
        }
    }
	return(out)
}
## let's make it an operator
`%*slow%` = slowMult

## need a matrix and a vector
v = rnorm(s)
M = matrix(rnorm(r*s), nrow=r, ncol=s)

t0 = Sys.time()
w = M %*slow% v
t1 = Sys.time()
dt = t1 - t0

## define function to handle distribution of work across threads
slowMultThread = function(w, M, v) {
	require(parallel)
	## determine which rows this thread will handle
	idx = splitIndices(nrow(M), myinfo$nwrkrs)[[myinfo$id]]
	w[idx, ] = M[idx, ] %*slow% v[ , ]
	## don't do expensive return of result
	return(0)
}

t0cl = Sys.time()
cl = makeCluster(3)     ## how many cores to use
mgrinit(cl)
## cluster needs to define its (shared) versions of w, M, v
mgrmakevar(cl, "Mcl", nrow(M), ncol(M))
Mcl[ , ] = M
mgrmakevar(cl, "vcl", length(v), 1)
vcl[ , ] = v
mgrmakevar(cl, "wcl", nrow(M), 1)
## cluster needs to know about slowMult and friends
clusterExport(cl, c("slowMult", "%*slow%", "slowMultThread"))
## parallel step to do the multiplication
clusterEvalQ(cl, slowMultThread(wcl, Mcl, vcl))
stoprdsm(cl)
t1cl = Sys.time()
dtcl = t1cl - t0cl

## compare execution times
cat("Serial:  ", round(dt, 1), "seconds\nParallel: ",
		round(dtcl, 1), "seconds\n")

## check that answer is same either way
randInds = sample(length(w), 10)
cbind(w[randInds], wcl[randInds])
sum(abs(w - wcl[ , ]))
