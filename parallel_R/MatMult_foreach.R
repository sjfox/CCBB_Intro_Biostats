library(foreach)
library(doMC)

r = 3000
s = 1000
nPieces = 5
rFrac = round(r / nPieces)

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

registerDoMC(cores=3) ## tell foreach how many cores to use
t0mc = Sys.time()
## parallel step to map the multiplication across blocks
wPieces = foreach (i=1:nPieces) %dopar% {
	## note syntax: (1) i=... instead of i in ... and (2) %dopar%
	lower = (i-1) * rFrac + 1
	upper = ifelse(i==nPieces, r, i*rFrac)
	return(M[lower:upper, ] %*slow% v)
}
## serial step to reduce the pieces into aggregated whole
wmc = Reduce(f=c, x=wPieces)
t1mc = Sys.time()
dtmc = t1mc - t0mc

## compare execution times
cat("Serial:   ", round(dt, 1), "seconds\nParallel: ",
		round(dtmc, 1), "seconds\n")

## check that answer is same either way
randInds = sample(length(w), 10)
cbind(w[randInds], wmc[randInds])
sum(abs(w - wmc))
