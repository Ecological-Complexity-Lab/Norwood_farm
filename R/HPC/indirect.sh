# these are variables to be used in the job queueing by the HPC:
#$ -q shai.q@bhn27
#$ -cwd
#$ -N NORWOODFARM
#$ -l h_vmem=8G

# running the desired R script

/gpfs0/shai/projects/R4/R-4.2.0/bin/Rscript HPC_Indirect_effects.R
# $1 -> the argument entered when running the script via command line
#       eg.: sh run_example.sh 5 ----> the value of $1 will be 5