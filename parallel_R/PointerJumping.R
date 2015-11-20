library(parallel)
library(Rdsm)

forest = c(
	"1" = 7,
	"2" = 3,
	"3" = 4,
	"4" = 4,
	"5" = 5,
	"6" = 11,
	"7" = 3,
	"8" = 4,
	"9" = 5,
	"10" = 3,
	"11" = 9,
	"12" = 6,
	"13" = 7
)
line = c(1, 1:999)

iterate = function(pointers) {
	out = pointers[pointers]
	names(out) = names(pointers)
	return(out)
}
iterN = function(pointers, n) {
	out = pointers
	for (i in 1:n) {
		out = iterate(out)
    }
	return(out)
}

iterParBar = function(nwpntcl, pntcl, ncl) {
	require(parallel)
	## determine which rows this thread will handle
	idx = splitIndices(length(pntcl), myinfo$nwrkrs)[[myinfo$id]]
	for (i in 1:ncl[1, 1]) {
		Sys.sleep(0.01 * exp(2*rnorm(1)))
		nwpntcl[idx] = pntcl[ pntcl[idx] ]
		## first barrier to keep iterations in sync
		barr()
		pntcl[idx] = nwpntcl[idx]
		## second barrier to keep iterations in sync
		barr()
    }
	## don't do expensive return of result
	return(0)
}
iterParN = function(forest, n, cores=3) {
	cl = makeCluster(cores)
	mgrinit(cl)
	makebarr(cl, barrback=TRUE)
	mgrmakevar(cl, "pntcl", length(forest), 1)
	pntcl[ , ] = forest
	mgrmakevar(cl, "nwpntcl", length(forest), 1)
	mgrmakevar(cl, "ncl", 1, 1)
	ncl[ , ] = n
	clusterExport(cl, "iterParBar")
	clusterEvalQ(cl, iterParBar(nwpntcl, pntcl, ncl))
	stoprdsm(cl)
	unlink("barr*")
	return(pntcl[ , ])
}
rootsSerial = iterN(line, 10)
rootsParallel = iterParN(line, 10, cores=4)
all(rootsSerial == rootsParallel)

