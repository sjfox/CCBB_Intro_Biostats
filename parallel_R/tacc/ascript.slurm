#!/bin/bash
#SBATCH -J ascript
#SBATCH -n 2
#SBATCH -N 1
#SBATCH -p development
#SBATCH -o ascript.o%j
#SBATCH -e ascript.e%j
#SBATCH -t 00:05:00
#SBATCH -A DNAdenovo

module load launcher
module load R



export EXECUTABLE=$TACC_LAUNCHER_DIR/init_launcher 
export CONTROL_FILE=ascript.cmd
export WORKDIR=.
# 
# Variable description:
#
#  EXECUTABLE     = full path to the job launcher executable
#  CONTROL_FILE   = text input file which specifies
#                   executable for each process
#                   (should be located in WORKDIR)
#  WORKDIR        = location of working directory
#
#      <------ End Setup Parameters ------>
#--------------------------------------------------------
#--------------------------------------------------------

#----------------
# Error Checking
#----------------

if [ ! -e $WORKDIR ]; then
    echo " "
    echo "Error: unable to change to working directory."
	echo "       $WORKDIR"
	echo " "
	echo "Job not submitted."
	exit
fi

if [ ! -f $EXECUTABLE ]; then
	echo " "
	echo "Error: unable to find launcher executable $EXECUTABLE."
	echo " "
	echo "Job not submitted."
	exit
fi

if [ ! -f $WORKDIR/$CONTROL_FILE ]; then
	echo " "
	echo "Error: unable to find input control file $CONTROL_FILE."
	echo " "
	echo "Job not submitted."
	exit
fi


#----------------
# Job Submission
#----------------

cd $WORKDIR/
echo " WORKING DIR:   $WORKDIR/"

$TACC_LAUNCHER_DIR/paramrun $EXECUTABLE $CONTROL_FILE

echo " "
echo " Parameteric Job Complete"
echo " "
