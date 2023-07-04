#!/bin/sh
### Note: No commands may be executed until after the #PBS lines
### Account information
#PBS -W group_list=pr_12345 -A pr_12345
### Job name (comment out the next line to get the name of the script used as the job name)
#PBS -N test
### Output files (comment out the next 2 lines to get the job name used instead)
#PBS -e test.err
#PBS -o test.log
### Only send mail when job is aborted or terminates abnormally
#PBS -m n
### Number of nodes
#PBS -l nodes=1:ppn=8
### Memory
#PBS -l mem=120gb
### Requesting time - format is <days>:<hours>:<minutes>:<seconds> (here, 12 hours)
#PBS -l walltime=12:00:00

# Go to the directory from where the job was submitted (initial directory is $HOME)
echo Working directory is $PBS_O_WORKDIR
cd $PBS_O_WORKDIR

### Here follows the user commands:
# Define number of processors
NPROCS=`wc -l < $PBS_NODEFILE`
echo This job has allocated $NPROCS nodes

# Load all required modules for the job
module load tools
module load perl/5.20.2
module load gcc/7.4.0
module load intel/perflibs
module load R/4.0.0

# Redirect standard output and error
exec 1> sims.log
exec 2> sims.log

# This is where the work is done
# Make sure that this script is not bigger than 64kb ~ 150 lines,
# otherwise put in separate script and execute from here
Rscript main/run_computerome.R

#qsub -W group_list=ku_00179 -A ku_00179 -l nodes=2:ppn=40:thinnode,mem=150gb,walltime=600 main/run_job.sh

#qsub -W group_list=ku_00179 -A ku_00179 -l nodes=1:ppn=40:thinnode,mem=150gb,walltime=3600 main/run_job.sh

#qsub -W group_list=ku_00179 -A ku_00179 -l nodes=1:ppn=40,mem=16gb,walltime=3600 main/run_job.sh

