library(parallel)

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

t0mc = Sys.time()
## serial step to separate M into blocks
Mblocks = list()
for (i in 1:nPieces) {
	lower = (i-1) * rFrac + 1
	upper = ifelse(i==nPieces, r, i*rFrac)
	Mblocks[[i]] = M[lower:upper, ]
}
## parallel step to map the multiplication across blocks
wPieces = mclapply(
	X = Mblocks,
	FUN = function(Mb) {Mb %*slow% v},
	mc.cores = 3
)
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

