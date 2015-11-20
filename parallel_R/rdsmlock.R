library(Rdsm)

## example from Rdsm::rdsmlock

# unreliable function
s = function(n) {
    for (i in 1:n) {
        tot[1, 1] = tot[1, 1] + 1
    }
}

library(parallel)
c2 = makeCluster(2)
clusterExport(c2, "s")
mgrinit(c2)
mgrmakevar(c2, "tot", 1, 1)
tot[1, 1] = 0
clusterEvalQ(c2, s(1000))
tot[1, 1]  # should be 2000, but likely far from it

s1 = function(n) {
    require(Rdsm)
    for (i in 1:n) {
        rdsmlock("totlock")
        tot[1, 1] = tot[1, 1] + 1
        rdsmunlock("totlock")
    }
}

mgrmakelock(c2, "totlock")
tot[1, 1] = 0
clusterExport(c2, "s1")
clusterEvalQ(c2, s1(1000))
tot[1, 1]  # will print out 2000, the correct number
