#! /bin/csh -x

#BSUB -P P93300606
#BSUB -W 00:30                 # wall-clock time (hrs:mins)
#BSUB -n 144                   # number of tasks in job         
#BSUB -R "span[ptile=16]"      # run 16 MPI tasks per node
#BSUB -J myjob                 # job name
#BSUB -o myjob.%J.out          # output file name in which %J is replaced by the job ID
#BSUB -e myjob.%J.err          # error file name in which %J is replaced by the job ID
#BSUB -q small                 # queue

### PATH
setenv I /glade/p/work/abozec/hycom/HYCOM_CESM/INPUT_hep20/
setenv E 100
setenv E1 10.0

setenv W ~/scratch/HEP20_run/expt_${E1}/data/
setenv SRC /glade/p/work/feiliu/regional_hycom/hycom_cesm/sorc/

mkdir -p ${W}/
cp ${I}/* ${W}/
cp ${SRC}/hycom ${W}/

echo "Enter working directory: ${W}"
cd ${W}

## run the model
setenv NOMP 1
setenv NMPI 144
cp patch_input.144 patch.input
setenv OMP_NUM_THREADS           $NOMP
setenv MPI_COLL_OPT_ON           1
setenv MPICH_RANK_REORDER_METHOD 1
setenv MPICH_MAX_SHORT_MSG_SIZE  65536
setenv MPICH_UNEX_BUFFER_SIZE    90M
setenv MPICH_VERSION_DISPLAY     1
setenv MPICH_ENV_DISPLAY         1
setenv NO_STOP_MESSAGE

rm output
module purge
module load ncarenv/1.0 ncarbinlibs/1.1 intel/16.0.0 ncarcompilers/1.0 netcdf/4.3.0 mkl/11.0.1 impi/4.0.3.008
setenv ESMFMKFILE /glade/u/home/mvr/ESMF_INSTALL_SNAPSHOT60/lib/esmf.mk  
mpirun.lsf ./hycom > output
