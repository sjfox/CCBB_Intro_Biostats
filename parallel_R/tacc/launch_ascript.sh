#!/bin/bash

module load launcher

## -----------------------------------------------------------------
## launcher_creator.py arguments (selected):
## -n <jobName>
jobName=ascript
## -j <jobFile>
jobFile=ascript.cmd
## -t <jobTime>
jobTime=00:05:00
## -a <alloc>
alloc=DNAdenovo
## -q <queue>
queue=development
## -w <wayness>
wayness=16
## -m <modules>
modules="module load R"
## -----------------------------------------------------------------

/corral-repl/utexas/BioITeam/bin/launcher_creator.py -n $jobName -j $jobFile -t $jobTime -a $alloc -q $queue -w $wayness -m "$modules"
sbatch ascript.slurm
