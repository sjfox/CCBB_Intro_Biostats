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

t0cl = Sys.time()
cl = makeCluster(3)     ## how many cores to use
## serial step to separate M into blocks
Mblocks = list()
for (i in 1:nPieces) {
	lower = (i-1) * rFrac + 1
	upper = ifelse(i==nPieces, r, i*rFrac)
	Mblocks[[i]] = M[lower:upper, ]
}
## cluster nodes needs to know about `%*slow%` and v
clusterExport(cl, "%*slow%")
clusterExport(cl, "v")
## parallel step to map the multiplication across blocks
wPieces = clusterApply(
	cl = cl,
	x = Mblocks,
	fun = function(x) {x %*slow% v}
)
## serial step to reduce the pieces into aggregated whole
wcl = Reduce(f=c, x=wPieces)
t1cl = Sys.time()
dtcl = t1cl - t0cl

## compare execution times
cat("Serial:   ", round(dt, 1), "seconds\nParallel: ",
		round(dtcl, 1), "seconds\n")

## check that answer is same either way
randInds = sample(length(w), 10)
cbind(w[randInds], wcl[randInds])
sum(abs(w - wcl))
