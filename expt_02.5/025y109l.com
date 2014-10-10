#!/bin/csh
#PBS -l procs=146
#PBS -l walltime=08:00:00
#PBS -A omd
#PBS -N GLBa0.24_zeus
#PBS -e ${HOME}/hycom/GLBa0.24/expt_02.5/data/SCRATCH5/out2009_l1
#PBS -o ${HOME}/hycom/GLBa0.24/expt_02.5/data/SCRATCH5/out2009_l1
#PBS -q batch
module purge
module load intel/12-12.0.4.191
module load mpt
setenv MPI_BUFS_PER_PROC 256
setenv MPI_BUFS_PER_HOST 256
setenv MPI_GROUP_MAX 256
# --- check that the C comment command is available.
#
C >& /dev/null
if (! $status) then
  if (-e ${home}/hycom/ALL/bin/C) then
    set path = ( ${path} ${home}/hycom/ALL/bin )
  else
    echo "Please put the command hycom/ALL/bin/C in your path"
  endif
endif
#
set echo
set time = 1
set timestamp
C
C --- Experiment GLBa0.24 - 01.X series
C --- 32 layer sig2star HYCOM for fully global region.
C
C --- 02.0 - As 01.1 but CFSR forcing
C ---        KPP, variable kapref, cfsr winds,thermal,precip forcing
C ---            cfsr 2001-2002 6-hrly wind anomalies,
C ---            QSCAT    lin. regr. correction to wind speed and stresses,
C ---            no       correction for lwflux and shwflx
C ---            no       correction for precip
C ---        Initialize from Jan GDEM3 but then use PHC3.0,levtop=2 for
C ---            SSS relaxation.
C --- 02.1 - As 02.0 but rerun from year 001 b with flux offset obtained from 020 years 5,6
C --- 02.2 - Interannual run with cfsr forcing 1993-2009. Initialized from expt 02.1 restart at end of 006 l.
C
C --- Preamble, script keys on O/S name.
C
C --- Set parallel configuration, see ../README/README.expt_parallel.
C --- NOMP = number of OpenMP threads, 0 for no OpenMP, 1 for inactive OpenMP
C --- NMPI = number of MPI    tasks,   0 for no MPI
C
setenv OS `uname`
switch ($OS)
case 'Linux':
    which yod
    if (! $status) then
      setenv OS XT3
    endif
    which aprun
    if (! $status) then
C --- XT4 or XT5
      setenv OS XT4
    endif
    breaksw
endsw
switch ($OS)
case 'SunOS':
case 'Linux':
    setenv NOMP 0
    setenv NMPI 146
    breaksw
#case 'OSF1':
#    setenv NOMP 0
#    setenv NMPI 243
#    breaksw
#case 'XT3':
#case 'XT4':
#    setenv NOMP 0
#    setenv NMPI 243
#    breaksw
#case 'IRIX64':
#case 'AIX':
#    setenv NOMP 0
#    setenv NMPI 243
#    breaksw
#case 'unicos':
#    setenv ACCT `newacct -l | awk '{print $4}'`
#    setenv NOMP 0
#    setenv NMPI 0
#    breaksw
default:
    echo 'Unknown Operating System: ' $OS
    exit (1)
endsw
C
C --- modify NOMP and NMPI based on batch limits
C
if ( $?LSB_MCPU_HOSTS ) then
# LSF batch system
# if ( $?LSB_INITIAL_NUM_PROCESSORS) then
#   setenv NCPU $LSB_INITIAL_NUM_PROCESSORS
# else
#   setenv NCPU `echo $LSB_MCPU_HOSTS | awk '{print $2+$4+$6+$8+$10+$12}'`
# endif
# if      ($NMPI == 0) then
#   setenv NOMP $NCPU
# else if ($NOMP == 0) then
#   setenv NMPI $NCPU
# else
#   setenv NMPI `echo $NCPU $NOMP | awk '{print int($1/$2)}'`
# endif
else if ( $?GRD_TOTAL_MPI_TASKS ) then
# GRD batch system
  if      ($NMPI == 0) then
    echo "error - NMPI=0, but running in a MPI batch queue"
    exit
  else
    setenv NMPI $GRD_TOTAL_MPI_TASKS
  endif
else if ( $?NSLOTS ) then
# codine or GRD batch system
  if      ($NMPI == 0) then
    setenv NOMP $NSLOTS
  else if ($NOMP == 0) then
    setenv NMPI $NSLOTS
  else
    setenv NMPI `echo $NSLOTS $NOMP | awk '{print int($1/$2)}'`
  endif
endif
echo "NOMP is " $NOMP " and NMPI is " $NMPI
C
C --- R is region name.
C --- V is source code version number.
C --- T is topography number.
C --- K is number of layers.
C --- E is expt number.
C --- P is primary path.
C --- D is permanent directory.
C --- S is scratch   directory, must not be the permanent directory.
C
setenv R GLBa0.24
setenv V 2.2.35
setenv T 13
setenv K 32
setenv E 025
setenv P hycom/${R}/expt_02.5/data
setenv D ~/$P
C
switch ($OS)
case 'SunOS':
    if (-e /net/hermes/scrb) then
#                  NRLSSC
      setenv S     /net/hermes/scrb/${user}/$P
    else if (-e /scr) then
#                  NAVO MSRC
      mkdir        /scr/${user}
      chmod a+rx   /scr/${user}
      setenv S     /scr/${user}/$P
    else
#                  Single Disk
      setenv S     ~/$P/SCRATCH
    endif
    breaksw
case 'Linux':
    if (-e /export/a/$user) then
#              NRLSSC
      setenv S /export/a/${user}/$P
    else if ( -e /ptmp ) then
#                  Zeus
      setenv S /ptmp/${user}/${P}
      setenv H /scratch2/portfolios/NCEPDEV/marine/save/${user}
      setenv D ${H}/${P}
      setenv HT /NCEPDEV/hpssuser/g01/wx21zdg
    else if (-e /scr) then
#                  NAVO MSRC
      mkdir        /scr/${user}
      chmod a+rx   /scr/${user}
      setenv S     /scr/${user}/$P
    else
#              Single Disk
      setenv S ~/$P/SCRATCH
    endif
    breaksw
case 'XT3':
case 'XT4':
    if      (-e /work) then
#                  ERDC MSRC
      mkdir        /work/${user}
      chmod a+rx   /work/${user}
      setenv S     /work/${user}/$P
    else if (-e /scr) then
#                  NAVO MSRC
      mkdir        /scr/${user}
      chmod a+rx   /scr/${user}
      setenv S     /scr/${user}/$P
    else
#                  Single Disk
      setenv S     ~/$P/SCRATCH
    endif
    mkdir -p         $S
    lfs setstripe -d $S
    lfs setstripe    $S 1048576 -1 8
    breaksw
case 'OSF1':
    if      (-e /work) then
#                  ERDC MSRC
      mkdir        /work/${user}
      chmod a+rx   /work/${user}
      setenv S     /work/${user}/$P
    else if (-e /workspace) then
#                  ASC MSRC
      mkdir        /workspace/${user}
      chmod a+rx   /workspace/${user}
      setenv S     /workspace/${user}/$P
    else
#                  Single Disk
      setenv S     ~/$P/SCRATCH
    endif
    breaksw
case 'IRIX64':
    if      (-e /scr) then
#                  NAVO MSRC
      mkdir        /scr/${user}
      chmod a+rx   /scr/${user}
      setenv S     /scr/${user}/$P
    else if (-e /workspace) then
#                  ASC MSRC
      mkdir        /workspace/${user}
      chmod a+rx   /workspace/${user}
      setenv S     /workspace/${user}/$P
    else
#                  Single Disk
      setenv S     ~/$P/SCRATCH
    endif
    breaksw
case 'AIX':
    if      (-e /gpfs/work) then
#                  ERDC MSRC, under PBS
      mkdir        /gpfs/work/${user}
      chmod a+rx   /gpfs/work/${user}
      setenv S     /gpfs/work/${user}/$P
      setenv POE  pbspoe
    else if (-e /scr) then
#                  NAVO MSRC, under LoadLeveler or LSF
      mkdir        /scr/${user}
      chmod a+rx   /scr/${user}
      setenv S     /scr/${user}/$P
      if ($?LSB_JOBINDEX) then
        setenv POE mpirun.lsf
      else
        setenv POE poe
      endif
    else
#                  ARL MSRC, under GRD
      mkdir        /usr/var/tmp/${user}
      chmod a+rx   /usr/var/tmp/${user}
      setenv S     /usr/var/tmp/${user}/$P
      setenv POE  grd_poe
    endif
    breaksw
case 'unicos':
    if      (-e /work) then
#                  ERDC MSRC
      mkdir        /work/${user}
      chmod a+rx   /work/${user}
      mkdir        /work/${user}/$R
      chgrp $ACCT  /work/${user}/$R
      setenv S     /work/${user}/$P
    else
      mkdir        /tmp/${user}
      chmod a+rx   /tmp/${user}
      mkdir        /tmp/${user}/$R
      chgrp $ACCT  /tmp/${user}/$R
      setenv S     /tmp/${user}/$P
    endif
    breaksw
endsw
C
mkdir -p $S
cd       $S
C
C --- For whole year runs.
C ---   Y01 initial model year of this run.
C ---   YXX is the last model year of this run, and the first of the next run.
C ---   A and B are identical, typically blank.
C --- For part year runs.
C ---   A is this part of the year, B is next part of the year.
C ---   Y01 is the start model year of this run.
C ---   YXX is the end   model year of this run, usually Y01.
C --- For a few hour/day run
C ---   A   is the start day and hour, of form "dDDDhHH".
C ---   B   is the end   day and hour, of form "dXXXhYY".
C ---   Y01 is the start model year of this run.
C ---   YXX is the end   model year of this run, usually Y01.
C --- Note that these variables are set by the .awk generating script.
C
setenv A "l"
setenv B "l"
setenv Y01 "109"
setenv YXX "109"
C
echo "Y01 =" $Y01 "YXX = " $YXX  "A =" ${A} "B =" ${B}
C
C --- local input files.
C
if (-e ${D}/../${E}y${Y01}${A}.limits) then
  /bin/cp ${D}/../${E}y${Y01}${A}.limits limits
else
#  use "LIMITI"  when starting a run after day zero.
#  use "LIMITS9" (say) for a 9-day run
  echo "LIMITS" | awk -f ${D}/../${E}.awk y01=${Y01} ab=${A} >! limits
endif
cat limits
C
if (-e ${D}/../ports.input_${Y01}${A}) then
  /bin/cp ${D}/../ports.input_${Y01}${A} ports.input
else
  /bin/cp ${D}/../ports.input ports.input
endif
C
if (-e ${D}/../tracer.input_${Y01}${A}) then
  /bin/cp ${D}/../tracer.input_${Y01}${A} tracer.input
else
  /bin/cp ${D}/../tracer.input tracer.input
endif
C
if (-e ${D}/../blkdat.input_${Y01}${A}) then
  /bin/cp ${D}/../blkdat.input_${Y01}${A} blkdat.input
else
  /bin/cp ${D}/../blkdat.input blkdat.input
endif
if (-e ./cice) then
  if (-e ${D}/../ice_in_${Y01}${A}) then
    /bin/cp ${D}/../ice_in_${Y01}${A} ice_in
  else
    /bin/cp ${D}/../ice_in ice_in
  endif
endif
if ($NMPI != 0) then
  setenv NPATCH `echo $NMPI | awk '{printf("%04d", $1)}'`
# setenv NPATCH `echo $NMPI | awk '{printf("%03d", $1)}'`
  /bin/rm -f patch.input
  /bin/cp ${D}/../../topo/partit/depth_${R}_${T}.${NPATCH}  patch.input
# /bin/cp ${D}/../../topo/partit/depth_${R}_${T}.${NPATCH}u patch.input
C
  /bin/rm -f archt.input
  if (-e ${D}/../archt.input_${Y01}${A}) then
    /bin/cp ${D}/../archt.input_${Y01}${A} archt.input
  else if (-e ${D}/../archt.input) then
    /bin/cp ${D}/../archt.input archt.input
  else
    touch archt.input
  endif
  if (! -z archt.input) then
    if (-e ./ARCHT) then
      /bin/mv ./ARCHT ./ARCHT_$$
    endif
    mkdir ./ARCHT
    switch ($OS)
    case 'XT3':
    case 'XT4':
      lfs setstripe ./ARCHT 1048576 -1 8
      breaksw
    endsw
    cd    ./ARCHT
    /bin/cp ../archt.input  .
    /bin/cp ../patch.input  .
    /bin/ln ../regional.*.? .
    ${home}/hycom/ALL/topo/src/topo_subset < archt.input
    cat patch.subreg | xargs mkdir
    /bin/rm -f regional.*.?
    cd ..
  endif
endif
C
C --- check that iexpt from blkdat.input agrees with E from this script.
C
setenv EB `grep "'iexpt ' =" blk* | awk '{printf("%03d", $1)}'`
if ($EB != $E) then
  cd $D/..
  /bin/mv LIST LIST_BADRUN
  echo "BADRUN" > LIST
  exit
endif
#C
#C --- turn on detailed debugging.
#C
#touch PIPE_DEBUG
C
C --- pget, pput "copy" files between scratch and permanent storage.
C --- Can both be cp if the permanent filesystem is mounted locally.
C
switch ($OS)
case 'SunOS':
#case 'Linux':
case 'OSF1':
case 'AIX':
case 'unicos':
    if      (-e ~wallcraf/bin/pget_navo) then
      setenv pget ~wallcraf/bin/pget_navo
      setenv pput ~wallcraf/bin/pput_navo
    else if (-e ~wallcraf/bin/pget) then
      setenv pget ~wallcraf/bin/pget
      setenv pput ~wallcraf/bin/pput
    else
      setenv pget /bin/cp
      setenv pput /bin/cp
    endif
    breaksw
case 'IRIX64':
    setenv pget /bin/cp
    setenv pput /bin/cp
    breaksw
case 'XT3':
case 'XT4':
#   setenv pget /bin/cp
    setenv pget ~wallcraf/bin/pget
    setenv pput ~wallcraf/bin/pput
    breaksw
default:
    setenv pget /bin/cp
    setenv pput /bin/cp
endsw
C
C --- input files from file server.
C
touch regional.depth.a regional.depth.b
if (-z regional.depth.a) then
   ${pget} ${D}/../../topo/depth_${R}_${T}.a regional.depth.a &
endif
if (-z regional.depth.b) then
   ${pget} ${D}/../../topo/depth_${R}_${T}.b regional.depth.b &
endif
C
touch regional.grid.a regional.grid.b
if (-z regional.grid.a) then
   ${pget} ${D}/../../topo/regional.grid.a regional.grid.a &
endif
if (-z regional.grid.b) then
   ${pget} ${D}/../../topo/regional.grid.b regional.grid.b &
endif
if (-e ./cice) then
C
C --- CICE non-synoptic files.
C
  touch regional.cice.r cice.prec_lanl_12.r cice.rhoa_ncar85-88_12.r
  if (-z regional.cice.r) then
     ${pget} ${D}/../../topo/regional.cice.r regional.cice.r &
  endif
  if (-z cice.prec_lanl_12.r) then
     ${pget} ${D}/../../force/cice/cice.prec_lanl_12.r cice.prec_lanl_12.r &
  endif
  if (-z cice.rhoa_ncar85-88_12.r) then
     ${pget} ${D}/../../force/cice/cice.rhoa_ncar85-88_12.r cice.rhoa_ncar85-88_12.r &
  endif
endif
C
if (! -e ./wind) then
C
C --- Climatological atmospheric forcing.
C
  setenv FN ncep_cfsr-sec2_1993-2009_mn6hra

  touch forcing.tauewd.a forcing.taunwd.a forcing.wndspd.a
  touch forcing.radflx.a forcing.shwflx.a forcing.vapmix.a forcing.precip.a
  touch forcing.airtmp.a forcing.seatmp.a forcing.surtmp.a
  touch forcing.tauewd.b forcing.taunwd.b forcing.wndspd.b
  touch forcing.radflx.b forcing.shwflx.b forcing.vapmix.b forcing.precip.b
  touch forcing.airtmp.b forcing.seatmp.b forcing.surtmp.b
  if (-z forcing.tauewd.a || -z forcing.tauewd.b) then
htar -xvf ${HT}/${D}/../../force/${FN}/tauewd.tar
mv tauewd.a forcing.tauewd.a
mv tauewd.b forcing.tauewd.b
  endif
  if (-z forcing.taunwd.a || -z forcing.taunwd.b ) then
htar -xvf ${HT}/${D}/../../force/${FN}/taunwd.tar
mv taunwd.a forcing.taunwd.a
mv taunwd.b forcing.taunwd.b
  endif
  if (-z forcing.wndspd.a || -z forcing.wndspd.b ) then
htar -xvf ${HT}/${D}/../../force/${FN}/wndspd.tar
mv wndspd.a forcing.wndspd.a
mv wndspd.b forcing.wndspd.b
  endif

setenv FN ncep_cfsr-sea_1993-2009_mn6hr
  if (-z forcing.vapmix.a || -z forcing.vapmix.b ) then
htar -xvf ${HT}/${D}/../../force/${FN}/vapmix.tar
mv vapmix.a forcing.vapmix.a
mv vapmix.b forcing.vapmix.b
  endif
  setenv AO ""
# setenv AO "_037c"
  if (-z forcing.airtmp.a) then
     ${pget} ${D}/../../force/${FN}/airtmp${AO}.a forcing.airtmp.a &
  endif
  if (-z forcing.airtmp.b) then
     ${pget} ${D}/../../force/${FN}/airtmp${AO}.b forcing.airtmp.b &
  endif
  setenv PO "-regress-gpcp"
# setenv PO ""
# setenv PO "_zero"
  if (-z forcing.precip.a) then
     ${pget} ${D}/../../force/${FN}/precip${PO}.a forcing.precip.a &
  endif
  if (-z forcing.precip.b) then
     ${pget} ${D}/../../force/${FN}/precip${PO}.b forcing.precip.b &
  endif
# setenv FO "-regress-isccp_fd"
  setenv FO ""
  if (-z forcing.radflx.a) then
     ${pget} ${D}/../../force/${FN}/radflx${FO}.a forcing.radflx.a &
  endif
  if (-z forcing.radflx.b) then
     ${pget} ${D}/../../force/${FN}/radflx${FO}.b forcing.radflx.b &
  endif
  if (-z forcing.shwflx.a) then
     ${pget} ${D}/../../force/${FN}/shwflx${FO}.a forcing.shwflx.a &
  endif
  if (-z forcing.shwflx.b) then
     ${pget} ${D}/../../force/${FN}/shwflx${FO}.b forcing.shwflx.b &
  endif
  if (-z forcing.surtmp.a) then
     ${pget} ${D}/../../force/${FN}/surtmp.a      forcing.surtmp.a &
  endif
  if (-z forcing.surtmp.b) then
     ${pget} ${D}/../../force/${FN}/surtmp.b      forcing.surtmp.b &
  endif
# setenv FS $FN
# setenv FS RS_SST-mn6hr
  setenv FS PF_SST-mn6hr
  if (-z forcing.seatmp.a) then
     ${pget} ${D}/../../force/${FS}/seatmp.a      forcing.seatmp.a &
  endif
  if (-z forcing.seatmp.b) then
     ${pget} ${D}/../../force/${FS}/seatmp.b      forcing.seatmp.b &
  endif
endif
C
C --- time-invarent heat flux offset
C
#setenv OF ""
#setenv OF "_${E}"
setenv OF "_010i"
if ($OF != "") then
  touch  forcing.offlux.a
  touch  forcing.offlux.b
  if (-z forcing.offlux.a) then
     ${pget} ${D}/../../force/offset/offlux${OF}.a forcing.offlux.a &
  endif
  if (-z forcing.offlux.b) then
     ${pget} ${D}/../../force/offset/offlux${OF}.b forcing.offlux.b &
  endif 
endif 
C
touch  forcing.rivers.a
touch  forcing.rivers.b
if (-z forcing.rivers.a) then
   ${pget} ${D}/../../force/rivers/rivers_${T}.a forcing.rivers.a &
endif
if (-z forcing.rivers.b) then
   ${pget} ${D}/../../force/rivers/rivers_${T}.b forcing.rivers.b &
endif
C
touch  forcing.kpar.a
touch  forcing.kpar.b
if (-z forcing.kpar.a) then
   ${pget} ${D}/../../force/seawifs/kpar.a forcing.kpar.a &
endif
if (-z forcing.kpar.b) then
   ${pget} ${D}/../../force/seawifs/kpar.b forcing.kpar.b &
endif
C
touch relax.rmu.a relax.saln.a relax.temp.a relax.intf.a
touch relax.rmu.b relax.saln.b relax.temp.b relax.intf.b
if (-z relax.rmu.a) then
   ${pget} ${D}/../../relax/${E}/relax_rmu.a relax.rmu.a  &
endif
if (-z relax.rmu.b) then
   ${pget} ${D}/../../relax/${E}/relax_rmu.b relax.rmu.b  &
endif
if (-z relax.saln.a) then
   ${pget} ${D}/../../relax/${E}/relax_sal.a relax.saln.a &
endif
if (-z relax.saln.b) then
   ${pget} ${D}/../../relax/${E}/relax_sal.b relax.saln.b &
endif
if (-z relax.temp.a) then
   ${pget} ${D}/../../relax/${E}/relax_tem.a relax.temp.a &
endif
if (-z relax.temp.b) then
   ${pget} ${D}/../../relax/${E}/relax_tem.b relax.temp.b &
endif
if (-z relax.intf.a) then
   ${pget} ${D}/../../relax/${E}/relax_int.a relax.intf.a &
endif
if (-z relax.intf.b) then
   ${pget} ${D}/../../relax/${E}/relax_int.b relax.intf.b &
endif
C
touch tbaric.a
touch tbaric.b
if (-z tbaric.a) then
   ${pget} ${D}/../../relax/${E}/tbaric.a tbaric.a  &
endif
if (-z tbaric.b) then
   ${pget} ${D}/../../relax/${E}/tbaric.b tbaric.b  &
endif
C
setenv XS ""
#setenv XS "010"
if ($XS != "") then
  touch  relax.ssh.a
  if (-z relax.ssh.a) then
     ${pget} ${D}/../../relax/SSH/relax_ssh_${XS}.a relax.ssh.a &
  endif
  touch  relax.ssh.b
  if (-z relax.ssh.b) then
     ${pget} ${D}/../../relax/SSH/relax_ssh_${XS}.b relax.ssh.b &
  endif
endif
#C
#touch diwlat.a
#touch diwlat.b
#if (-z diwlat.a) then
#   ${pget} ${D}/../../relax/${E}/diwlat.a diwlat.a  &
#endif
#if (-z diwlat.b) then
#   ${pget} ${D}/../../relax/${E}/diwlat.b diwlat.b  &
#endif
C
touch iso.sigma.a
touch iso.sigma.b
if (-z iso.sigma.a) then
   ${pget} ${D}/../../relax/${E}/iso_sigma.a iso.sigma.a  &
endif
if (-z iso.sigma.b) then
   ${pget} ${D}/../../relax/${E}/iso_sigma.b iso.sigma.b  &
endif
#C
#touch iso.top.a
#touch iso.top.b
#if (-z iso.top.a) then
#   ${pget} ${D}/../../relax/${E}/iso_top.a iso.top.a  &
#endif
#if (-z iso.top.b) then
#   ${pget} ${D}/../../relax/${E}/iso_top.b iso.top.b  &
#endif
#C
#touch thkdf4.a
#touch thkdf4.b
#if (-z thkdf4.a) then
#   ${pget} ${D}/../../relax/${E}/thkdf4.a thkdf4.a  &
#endif
#if (-z thkdf4.b) then
#   ${pget} ${D}/../../relax/${E}/thkdf4.b thkdf4.b  &
#endif
#C
#touch veldf2.a
#touch veldf2.b
#if (-z veldf2.a) then
#   ${pget} ${D}/../../relax/${E}/veldf2.a veldf2.a  &
#endif
#if (-z veldf2.b) then
#   ${pget} ${D}/../../relax/${E}/veldf2.b veldf2.b  &
#endif
#C
#touch veldf4.a
#touch veldf4.b
#if (-z veldf4.a) then
#   ${pget} ${D}/../../relax/${E}/veldf4.a veldf4.a  &
#endif
#if (-z veldf4.b) then
#   ${pget} ${D}/../../relax/${E}/veldf4.b veldf4.b  &
#endif
#C
#touch tidal.rh.a
#touch tidal.rh.b
#if (-z tidal.rh.a) then
#   ${pget} ${D}/../../relax/${E}/tidal.rh.a tidal.rh.a  &
#endif
#if (-z tidal.rh.b) then
#   ${pget} ${D}/../../relax/${E}/tidal.rh.b tidal.rh.b  &
#endif
C
C --- restart input
C
touch   restart_in.a restart_in.b restart_out.a restart_out.b restart_out1.a restart_out1.b
if (-z restart_in.b) then
  setenv RI "       0.00"
else
  setenv RI `head -2 restart_in.b | tail -1 | awk  '{printf("%11.2f\n", $5)}'`
endif
if (-z restart_out.b) then
  setenv RO "       0.00"
else
  setenv RO `head -2 restart_out.b | tail -1 | awk  '{printf("%11.2f\n", $5)}'`
endif
if (-z restart_out1.b) then
  setenv R1 "       0.00"
else
  setenv R1 `head -2 restart_out1.b | tail -1 | awk  '{printf("%11.2f\n", $5)}'`
endif
setenv LI `awk  '{printf("%11.2f\n", $1)}' limits`
C
if (`echo $LI | awk '{if ($1 <= 0.0) print 1; else print 0}'`) then
C --- no restart needed
  /bin/rm restart_in.a   restart_in.b
  /bin/rm restart_out.a  restart_out.b
  /bin/rm restart_out1.a restart_out1.b
else if (`echo $LI $RI | awk '{if ($1-0.1 < $2 && $1+0.1 > $2) print 1; else print 0}'`) then
C --- restart is already in restart_in
  /bin/rm restart_out.a  restart_out.b
  /bin/rm restart_out1.a restart_out1.b
else if (`echo $LI $RO | awk '{if ($1-0.1 < $2 && $1+0.1 > $2) print 1; else print 0}'`) then
C --- restart is in restart_out
  /bin/mv restart_out.a  restart_in.a
  /bin/mv restart_out.b  restart_in.b
  /bin/rm restart_out1.a restart_out1.b
else if (`echo $LI $R1 | awk '{if ($1-0.1 < $2 && $1+0.1 > $2) print 1; else print 0}'`) then
C ---   restart is in restart_out1
  /bin/mv restart_out1.a restart_in.a
  /bin/mv restart_out1.b restart_in.b
  /bin/rm restart_out.a  restart_out.b
else
C ---   get restart from permenant storage
  /bin/rm restart_in.a   restart_in.b
  /bin/rm restart_out.a  restart_out.b
  /bin/rm restart_out1.a restart_out1.b
  ${pget} ${D}/restart_${Y01}${A}.a restart_in.a &
  ${pget} ${D}/restart_${Y01}${A}.b restart_in.b &
endif
if (-e ./cice) then
C
C --- CICE restart input
C
touch   cice.restart_in cice.restart_out
if (-z cice.restart_in) then
  setenv RI "       0.00"
else
  setenv RI `cice_stat cice.restart_in  | awk  '{printf("%11.2f\n", $4)}'`
endif
if (-z cice.restart_out) then
  setenv RO "       0.00"
else
  setenv RO `cice_stat cice.restart_out | awk  '{printf("%11.2f\n", $4)}'`
endif
setenv LI `awk  '{printf("%11.2f\n", $1)}' limits`
C
if (`echo $LI $RI | awk '{if ($1-0.1 < $2 && $1+0.1 > $2) print 1; else print 0}'`) then
C --- restart is already in cice.restart_in
  /bin/rm cice.restart_out
else if (`echo $LI $RO | awk '{if ($1-0.1 < $2 && $1+0.1 > $2) print 1; else print 0}'`) then
C --- restart is in cice.restart_out
  /bin/mv cice.restart_out  cice.restart_in
else
C ---   get restart from permenant storage
  /bin/rm cice.restart_in
  /bin/rm cice.restart_out
  ${pget} ${D}/cice.restart_${Y01}${A} cice.restart_in &
endif
echo "cice.restart_in" >! cice.restart_file
endif
C
C --- model executable
C
if      ($NMPI == 0 && $NOMP == 0) then
  setenv TYPE one
else if ($NMPI == 0) then
  setenv TYPE omp
else if ($NOMP == 0) then
  if ( ! $?TYPE ) then
    setenv TYPE mpi
  endif
else
  setenv TYPE ompi
endif
if (-e ./cice) then
  setenv TYPE cice
  setenv HEXE hycom_cice
else
  setenv HEXE hycom
endif
/bin/cp ${D}/../../src_${V}_${K}_${TYPE}/${HEXE} . &
C
C --- summary printout
C
touch   summary_out
/bin/mv summary_out summary_old
C
C --- heat transport output
C
touch   flxdp_out.a flxdp_out.b
/bin/mv flxdp_out.a flxdp_old.a
/bin/mv flxdp_out.b flxdp_old.b
C
touch   ovrtn_out
/bin/mv ovrtn_out ovrtn_old
C
C --- clean up old archive files, typically from batch system rerun.
C
mkdir KEEP
touch archv.dummy.b
foreach f (arch*)
  /bin/mv $f KEEP/$f
end
C
C --- Nesting input archive files.
C
if (-e ./nest) then
  cd ./nest
  touch rmu.a rmu.b
  if (-z rmu.a) then
     ${pget} ${D}/../../relax/${E}/nest_rmu.a rmu.a &
  endif
  if (-z rmu.b) then
     ${pget} ${D}/../../relax/${E}/nest_rmu.b rmu.b &
  endif
  touch   arch.dummy.b
  /bin/rm arch*.[ab]
  touch archv_${Y01}${A}.tar
  if (-z archv_${Y01}${A}.tar) then
    ${pget} ${D}/nest/archv_${Y01}${A}.tar archv_${Y01}${A}.tar
  endif
  tar xvf archv_${Y01}${A}.tar
  cd ..
endif
C
C --- let all file copies complete.
C
wait
C
C --- zero file length means no rivers.
C
if (-z forcing.rivers.a) then
   /bin/rm forcing.rivers.[ab]
endif
C
C --- Just in time atmospheric forcing.
C
if (-e ./wind) then
  if (! -e ./flux) then
    echo './flux must exist if ./wind does'
    exit
  endif
C
C --- Check to see if wind and flux files exist, if not make them and wait.
C
  /bin/rm -f forcing.tauewd.a forcing.taunwd.a forcing.wndspd.a
  /bin/rm -f forcing.tauewd.b forcing.taunwd.b forcing.wndspd.b
  /bin/rm -f ./wind/${E}w${Y01}${A}
  if (-e     ./wind/tauewd_${Y01}${A}.a && \
      -e     ./wind/tauewd_${Y01}${A}.b && \
      -e     ./wind/taunwd_${Y01}${A}.a && \
      -e     ./wind/taunwd_${Y01}${A}.b    ) then
    /bin/ln  ./wind/tauewd_${Y01}${A}.a forcing.tauewd.a
    /bin/ln  ./wind/taunwd_${Y01}${A}.a forcing.taunwd.a
    /bin/ln  ./wind/tauewd_${Y01}${A}.b forcing.tauewd.b
    /bin/ln  ./wind/taunwd_${Y01}${A}.b forcing.taunwd.b
    if (-e     ./wind/wndspd_${Y01}${A}.a && \
        -e     ./wind/wndspd_${Y01}${A}.b    ) then
      /bin/ln  ./wind/wndspd_${Y01}${A}.a forcing.wndspd.a
      /bin/ln  ./wind/wndspd_${Y01}${A}.b forcing.wndspd.b
    endif
  else
    cd ./wind
    touch ${E}w${Y01}${A}
    /bin/rm -f ${E}w${Y01}${A}.com ${E}w${Y01}${A}.log
    awk -f $D/../${E}.awk y01=${Y01} ab=${A} $D/../${E}W.com > ${E}w${Y01}${A}.com
    csh ${E}w${Y01}${A}.com >& ${E}w${Y01}${A}.log &
    cd ..
  endif
  if (-e ./wspd) then
    /bin/rm -f forcing.wndspd.a
    /bin/rm -f forcing.wndspd.b
    /bin/rm -f ./wspd/${E}s${Y01}${A}
    if (-e     ./wspd/wndspd_${Y01}${A}.a && \
        -e     ./wspd/wndspd_${Y01}${A}.b    ) then
      /bin/ln  ./wspd/wndspd_${Y01}${A}.a forcing.wndspd.a
      /bin/ln  ./wspd/wndspd_${Y01}${A}.b forcing.wndspd.b
    else
      cd ./wspd
      touch ${E}s${Y01}${A}
      /bin/rm -f ${E}s${Y01}${A}.com ${E}s${Y01}${A}.log
      awk -f $D/../${E}.awk y01=${Y01} ab=${A} $D/../${E}S.com > ${E}s${Y01}${A}.com
      csh ${E}s${Y01}${A}.com >& ${E}s${Y01}${A}.log &
      cd ..
    endif
  endif
  if (-e ./wvel) then
    /bin/rm -f forcing.wndspd.a
    /bin/rm -f forcing.wndspd.b
    /bin/rm -f ./wvel/${E}v${Y01}${A}
    if (-e     ./wvel/wndspd_${Y01}${A}.a && \
        -e     ./wvel/wndspd_${Y01}${A}.b    ) then
      /bin/ln  ./wvel/wndspd_${Y01}${A}.a forcing.wndspd.a
      /bin/ln  ./wvel/wndspd_${Y01}${A}.b forcing.wndspd.b
    else
      cd ./wvel
      touch ${E}v${Y01}${A}
      /bin/rm -f ${E}v${Y01}${A}.com ${E}v${Y01}${A}.log
      awk -f $D/../${E}.awk y01=${Y01} ab=${A} $D/../${E}V.com > ${E}v${Y01}${A}.com
      csh ${E}v${Y01}${A}.com >& ${E}v${Y01}${A}.log &
      cd ..
    endif
  endif
  if (-e ./grad) then
    /bin/rm -f ./grad/${E}g${Y01}${A}
    if (-e     ./grad/glbrad_${Y01}${A}.a && \
        -e     ./grad/glbrad_${Y01}${A}.b    ) then
C
C     this segments glbrad already exists
C
    else
      cd ./grad
      touch ${E}g${Y01}${A}
      /bin/rm -f ${E}g${Y01}${A}.com ${E}g${Y01}${A}.log
      awk -f $D/../${E}.awk y01=${Y01} ab=${A} $D/../${E}G.com > ${E}g${Y01}${A}.com
      csh ${E}g${Y01}${A}.com >& ${E}g${Y01}${A}.log &
      cd ..
    endif
  endif
  if (-e ./ssta) then
    /bin/rm -f forcing.surtmp.a
    /bin/rm -f forcing.surtmp.b
    /bin/rm -f ./ssta/${E}p${Y01}${A}
    if (-e     ./ssta/surtmp_${Y01}${A}.a && \
        -e     ./ssta/surtmp_${Y01}${A}.b    ) then
      /bin/ln  ./ssta/surtmp_${Y01}${A}.a forcing.surtmp.a
      /bin/ln  ./ssta/surtmp_${Y01}${A}.b forcing.surtmp.b
    else
      cd ./ssta
      touch ${E}t${Y01}${A}
      /bin/rm -f ${E}t${Y01}${A}.com ${E}t${Y01}${A}.log
      awk -f $D/../${E}.awk y01=${Y01} ab=${A} $D/../${E}T.com > ${E}t${Y01}${A}.com
      csh ${E}t${Y01}${A}.com >& ${E}t${Y01}${A}.log &
      cd ..
    endif
  endif
  if (-e ./ssto) then
    /bin/rm -f forcing.seatmp.a
    /bin/rm -f forcing.seatmp.b
    /bin/rm -f ./ssto/${E}p${Y01}${A}
    if (-e     ./ssto/seatmp_${Y01}${A}.a && \
        -e     ./ssto/seatmp_${Y01}${A}.b    ) then
      /bin/ln  ./ssto/seatmp_${Y01}${A}.a forcing.seatmp.a
      /bin/ln  ./ssto/seatmp_${Y01}${A}.b forcing.seatmp.b
    else
      cd ./ssto
      touch ${E}o${Y01}${A}
      /bin/rm -f ${E}o${Y01}${A}.com ${E}o${Y01}${A}.log
      awk -f $D/../${E}.awk y01=${Y01} ab=${A} $D/../${E}O.com > ${E}o${Y01}${A}.com
      csh ${E}o${Y01}${A}.com >& ${E}o${Y01}${A}.log &
      cd ..
    endif
  endif
  if (-e ./pcip) then
    /bin/rm -f forcing.precip.a
    /bin/rm -f forcing.precip.b
    /bin/rm -f ./pcip/${E}p${Y01}${A}
    if (-e     ./pcip/precip_${Y01}${A}.a && \
        -e     ./pcip/precip_${Y01}${A}.b    ) then
      /bin/ln  ./pcip/precip_${Y01}${A}.a forcing.precip.a
      /bin/ln  ./pcip/precip_${Y01}${A}.b forcing.precip.b
    else
      cd ./pcip
      touch ${E}p${Y01}${A}
      /bin/rm -f ${E}p${Y01}${A}.com ${E}p${Y01}${A}.log
      awk -f $D/../${E}.awk y01=${Y01} ab=${A} $D/../${E}P.com > ${E}p${Y01}${A}.com
      csh ${E}p${Y01}${A}.com >& ${E}p${Y01}${A}.log &
      cd ..
    endif
  endif
  if (-e ./flxt) then
    /bin/rm -f forcing.radflx.a
    /bin/rm -f forcing.radflx.b
    /bin/rm -f ./flxt/${E}p${Y01}${A}
    if (-e     ./flxt/totflx_${Y01}${A}.a && \
        -e     ./flxt/totflx_${Y01}${A}.b    ) then
      /bin/ln  ./flxt/totflx_${Y01}${A}.a forcing.radflx.a
      /bin/ln  ./flxt/totflx_${Y01}${A}.b forcing.radflx.b
    else
      cd ./flxt
      touch ${E}q${Y01}${A}
      /bin/rm -f ${E}p${Y01}${A}.com ${E}p${Y01}${A}.log
      awk -f $D/../${E}.awk y01=${Y01} ab=${A} $D/../${E}Q.com > ${E}q${Y01}${A}.com
      csh ${E}q${Y01}${A}.com >& ${E}q${Y01}${A}.log &
      cd ..
    endif
  endif
  if (-e ./flux) then
    /bin/rm -f forcing.airtmp.a forcing.shwflx.a forcing.vapmix.a
    /bin/rm -f forcing.airtmp.b forcing.shwflx.b forcing.vapmix.b
    if (! -e ./pcip) then
      /bin/rm -f forcing.precip.a
      /bin/rm -f forcing.precip.b
      touch ./flux/precip_${Y01}${A}.a
      touch ./flux/precip_${Y01}${A}.b
    endif
    if (! -e ./flxt) then
      /bin/rm -f forcing.radflx.a
      /bin/rm -f forcing.radflx.b
      touch ./flux/radflx_${Y01}${A}.a
      touch ./flux/radflx_${Y01}${A}.b
    endif
    /bin/rm -f ./flux/${E}f${Y01}${A}
    if (-e     ./flux/airtmp_${Y01}${A}.a && \
        -e     ./flux/airtmp_${Y01}${A}.b && \
#        -e     ./flux/precip_${Y01}${A}.a && \
#        -e     ./flux/precip_${Y01}${A}.b && \
        -e     ./flux/radflx_${Y01}${A}.a && \
        -e     ./flux/radflx_${Y01}${A}.b && \
        -e     ./flux/shwflx_${Y01}${A}.a && \
        -e     ./flux/shwflx_${Y01}${A}.b && \
        -e     ./flux/vapmix_${Y01}${A}.a && \
        -e     ./flux/vapmix_${Y01}${A}.b    ) then
      /bin/ln  ./flux/airtmp_${Y01}${A}.a forcing.airtmp.a
      /bin/ln  ./flux/airtmp_${Y01}${A}.b forcing.airtmp.b
      /bin/ln  ./flux/shwflx_${Y01}${A}.a forcing.shwflx.a
      /bin/ln  ./flux/shwflx_${Y01}${A}.b forcing.shwflx.b
      /bin/ln  ./flux/vapmix_${Y01}${A}.a forcing.vapmix.a
      /bin/ln  ./flux/vapmix_${Y01}${A}.b forcing.vapmix.b
      if (! -e ./pcip) then
        /bin/ln ./flux/precip_${Y01}${A}.b forcing.precip.b
        /bin/ln ./flux/precip_${Y01}${A}.a forcing.precip.a
      endif
      if (! -e ./flxt) then
        /bin/ln ./flux/radflx_${Y01}${A}.a forcing.radflx.a
        /bin/ln ./flux/radflx_${Y01}${A}.b forcing.radflx.b
      endif
    else
      cd ./flux
      touch ${E}f${Y01}${A}
      /bin/rm -f ${E}f${Y01}${A}.com ${E}f${Y01}${A}.log
      awk -f $D/../${E}.awk y01=${Y01} ab=${A} $D/../${E}F.com > ${E}f${Y01}${A}.com
      csh ${E}f${Y01}${A}.com >& ${E}f${Y01}${A}.log &
      cd ..
    endif
  endif
  wait
  if (-e    ./wind/${E}w${Y01}${A}) then
    /bin/ln ./wind/tauewd_${Y01}${A}.a forcing.tauewd.a
    /bin/ln ./wind/taunwd_${Y01}${A}.a forcing.taunwd.a
    /bin/ln ./wind/tauewd_${Y01}${A}.b forcing.tauewd.b
    /bin/ln ./wind/taunwd_${Y01}${A}.b forcing.taunwd.b
    if (-e    ./wind/wndspd_${Y01}${A}.a && \
        -e    ./wind/wndspd_${Y01}${A}.b    ) then
      /bin/ln ./wind/wndspd_${Y01}${A}.a forcing.wndspd.a
      /bin/ln ./wind/wndspd_${Y01}${A}.b forcing.wndspd.b
    endif
  endif
  if (-e ./wvel) then
    if (-e ./wvel/${E}v${Y01}${A}) then
      /bin/ln ./wvel/wndspd_${Y01}${A}.a forcing.wndspd.a
      /bin/ln ./wvel/wndspd_${Y01}${A}.b forcing.wndspd.b
    endif
  endif
  if (-e ./wspd) then
    if (-e ./wspd/${E}s${Y01}${A}) then
      /bin/ln ./wspd/wndspd_${Y01}${A}.a forcing.wndspd.a
      /bin/ln ./wspd/wndspd_${Y01}${A}.b forcing.wndspd.b
    endif
  endif
  if (-e ./ssta) then
    if (-e ./ssta/${E}t${Y01}${A}) then
      /bin/ln ./ssta/surtmp_${Y01}${A}.a forcing.surtmp.a
      /bin/ln ./ssta/surtmp_${Y01}${A}.b forcing.surtmp.b
    endif
  endif
  if (-e ./ssto) then
    if (-e ./ssto/${E}t${Y01}${A}) then
      /bin/ln ./ssto/seatmp_${Y01}${A}.a forcing.seatmp.a
      /bin/ln ./ssto/seatmp_${Y01}${A}.b forcing.seatmp.b
    endif
  endif
  if (-e ./pcip) then
    if (-e ./pcip/${E}p${Y01}${A}) then
      /bin/ln ./pcip/precip_${Y01}${A}.a forcing.precip.a
      /bin/ln ./pcip/precip_${Y01}${A}.b forcing.precip.b
    endif
  endif
  if (-e ./flxt) then
    if (-e ./flxt/${E}q${Y01}${A}) then
      /bin/ln ./flxt/totflx_${Y01}${A}.a forcing.radflx.a
      /bin/ln ./flxt/totflx_${Y01}${A}.b forcing.radflx.b
    endif
  endif
  if (-e ./flux) then
    if (-e    ./flux/${E}f${Y01}${A}) then
      /bin/ln ./flux/airtmp_${Y01}${A}.a forcing.airtmp.a
      /bin/ln ./flux/airtmp_${Y01}${A}.b forcing.airtmp.b
      /bin/ln ./flux/shwflx_${Y01}${A}.a forcing.shwflx.a
      /bin/ln ./flux/shwflx_${Y01}${A}.b forcing.shwflx.b
      /bin/ln ./flux/vapmix_${Y01}${A}.a forcing.vapmix.a
      /bin/ln ./flux/vapmix_${Y01}${A}.b forcing.vapmix.b
      if (! -e ./pcip) then
        /bin/ln ./flux/precip_${Y01}${A}.b forcing.precip.b
        /bin/ln ./flux/precip_${Y01}${A}.a forcing.precip.a
      endif
      if (! -e ./flxt) then
        /bin/ln ./flux/radflx_${Y01}${A}.a forcing.radflx.a
        /bin/ln ./flux/radflx_${Y01}${A}.b forcing.radflx.b
      endif
    endif
  endif
C
C --- CICE.
C
  if (-e ./cice) then
    if (-e ./cice/lwdflx_${Y01}${A}.r) then
C
C --- this segments lwdflx already exists.
C
    else
      cd ./cice
      setenv IDM 168
      setenv JDM 280
      /bin/rm lwdflx_${Y01}${A}.?
      hycom_expr netQlw_${Y01}${A}.a surtmp4_${Y01}${A}.a ${IDM} ${JDM} 1.0 567.0e-10 lwdflx_${Y01}${A}.a >! lwdflx_${Y01}${A}.b
      hycom2raw8 lwdflx_${Y01}${A}.a ${IDM} ${JDM} lwdflx_${Y01}${A}.r >! lwdflx_${Y01}${A}.B
      if (-e ./SAVE) then
        foreach f ( lwdflx_${Y01}${A}.[rB] )
          ln ${f} ./SAVE/${f}
        end
      endif
      cd ..
    endif
    /bin/rm cice/*${Y01}${A}.[Aab]
    foreach t ( airtmp glbrad netrad lwdflx vapmix wndewd wndnwd )
      /bin/rm -f                       cice.${t}.r
      /bin/ln ./cice/${t}_${Y01}${A}.r cice.${t}.r
    end
  endif
C
C --- If the winds or fluxes for the next segment dont exist, 
C --- interpolate them to model grid while current segment is running.
C
  if (-e ./wind/tauewd_${YXX}${B}.a && \
      -e ./wind/tauewd_${YXX}${B}.b && \
      -e ./wind/taunwd_${YXX}${B}.a && \
      -e ./wind/taunwd_${YXX}${B}.b    ) then
C
C --- next segments winds already exist.
C
  else
    cd ./wind
    /bin/rm -f ${E}w${YXX}${B}.com ${E}w${YXX}${B}.log
    awk -f $D/../${E}.awk y01=${YXX} ab=${B} $D/../${E}W.com > ${E}w${YXX}${B}.com
    csh ${E}w${YXX}${B}.com >& ${E}w${YXX}${B}.log &
    cd ..
  endif
  if (-e ./wspd) then
    if (-e ./wspd/wndspd_${YXX}${B}.a && \
        -e ./wspd/wndspd_${YXX}${B}.b    ) then
C
C ---   next segments wndspd already exists.
C
    else
      cd ./wspd
      /bin/rm -f ${E}s${YXX}${B}.com ${E}s${YXX}${B}.log
      awk -f $D/../${E}.awk y01=${YXX} ab=${B} $D/../${E}S.com > ${E}s${YXX}${B}.com
      csh ${E}s${YXX}${B}.com >& ${E}s${YXX}${B}.log &
      cd ..
    endif
  endif
  if (-e ./wvel) then
    if (-e ./wvel/wndspd_${YXX}${B}.a && \
        -e ./wvel/wndspd_${YXX}${B}.b    ) then
C
C ---   next segments wndspd already exists.
C
    else
      cd ./wvel
      /bin/rm -f ${E}v${YXX}${B}.com ${E}v${YXX}${B}.log
      awk -f $D/../${E}.awk y01=${YXX} ab=${B} $D/../${E}V.com > ${E}v${YXX}${B}.com
      csh ${E}v${YXX}${B}.com >& ${E}v${YXX}${B}.log &
      cd ..
    endif
  endif
  if (-e ./grad) then
    if (-e ./grad/glbrad_${YXX}${B}.a && \
        -e ./grad/glbrad_${YXX}${B}.b    ) then
C
C ---   next segments glbrad already exists.
C
    else
      cd ./grad
      /bin/rm -f ${E}g${YXX}${B}.com ${E}g${YXX}${B}.log
      awk -f $D/../${E}.awk y01=${YXX} ab=${B} $D/../${E}G.com > ${E}g${YXX}${B}.com
      csh ${E}g${YXX}${B}.com >& ${E}g${YXX}${B}.log &
      cd ..
    endif
  endif
  if (-e ./ssta) then
    if (-e ./ssta/surtmp_${YXX}${B}.a && \
        -e ./ssta/surtmp_${YXX}${B}.b    ) then
C
C ---   next segments surtmp already exists.
C
    else
      cd ./ssta
      /bin/rm -f ${E}t${YXX}${B}.com ${E}t${YXX}${B}.log
      awk -f $D/../${E}.awk y01=${YXX} ab=${B} $D/../${E}T.com > ${E}t${YXX}${B}.com
      csh ${E}t${YXX}${B}.com >& ${E}t${YXX}${B}.log &
      cd ..
    endif
  endif
  if (-e ./ssto) then
    if (-e ./ssto/seatmp_${YXX}${B}.a && \
        -e ./ssto/seatmp_${YXX}${B}.b    ) then
C
C ---   next segments seatmp already exists.
C
    else
      cd ./ssto
      /bin/rm -f ${E}o${YXX}${B}.com ${E}o${YXX}${B}.log
      awk -f $D/../${E}.awk y01=${YXX} ab=${B} $D/../${E}O.com > ${E}o${YXX}${B}.com
      csh ${E}o${YXX}${B}.com >& ${E}o${YXX}${B}.log &
      cd ..
    endif
  endif
  if (-e ./pcip) then
    if (-e ./pcip/precip_${YXX}${B}.a && \
        -e ./pcip/precip_${YXX}${B}.b    ) then
C
C ---   next segments pcip already exists.
C
    else
      cd ./pcip
      /bin/rm -f ${E}p${YXX}${B}.com ${E}p${YXX}${B}.log
      awk -f $D/../${E}.awk y01=${YXX} ab=${B} $D/../${E}P.com > ${E}p${YXX}${B}.com
      csh ${E}p${YXX}${B}.com >& ${E}p${YXX}${B}.log &
      cd ..
    endif
  endif
  if (-e ./flxt) then
    if (-e ./flxt/totflx_${YXX}${B}.a && \
        -e ./flxt/totflx_${YXX}${B}.b    ) then
C
C ---   next segments flxt already exist.
C
    else
      cd ./flxt
      /bin/rm -f ${E}q${YXX}${B}.com ${E}q${YXX}${B}.log
      awk -f $D/../${E}.awk y01=${YXX} ab=${B} $D/../${E}Q.com > ${E}q${YXX}${B}.com
      csh ${E}q${YXX}${B}.com >& ${E}q${YXX}${B}.log &
      cd ..
    endif
  endif
  if (-e ./flux) then
    if (! -e ./pcip) then
      touch ./flux/precip_${YXX}${B}.a
      touch ./flux/precip_${YXX}${B}.b
    endif
    if (! -e ./flxt) then
      touch ./flux/radflx_${YXX}${B}.a
      touch ./flux/radflx_${YXX}${B}.b
    endif
    if (-e ./flux/airtmp_${YXX}${B}.a && \
        -e ./flux/airtmp_${YXX}${B}.b && \
#        -e ./flux/precip_${YXX}${B}.a && \
#        -e ./flux/precip_${YXX}${B}.b && \
        -e ./flux/radflx_${YXX}${B}.a && \
        -e ./flux/radflx_${YXX}${B}.b && \
        -e ./flux/shwflx_${YXX}${B}.a && \
        -e ./flux/shwflx_${YXX}${B}.b && \
        -e ./flux/vapmix_${YXX}${B}.a && \
        -e ./flux/vapmix_${YXX}${B}.b    ) then
C
C ---   next segments fluxes already exist.
C
    else
      cd ./flux
      /bin/rm -f ${E}f${YXX}${B}.com ${E}f${YXX}${B}.log
      awk -f $D/../${E}.awk y01=${YXX} ab=${B} $D/../${E}F.com > ${E}f${YXX}${B}.com
      csh ${E}f${YXX}${B}.com >& ${E}f${YXX}${B}.log &
      cd ..
    endif
  endif
endif
C
C --- Nesting input archive files for next segment.
C
if (-e ./nest) then
  cd ./nest
  touch archv_${YXX}${B}.tar
  if (-z archv_${YXX}${B}.tar) then
    ${pget} ${D}/nest/archv_${YXX}${B}.tar archv_${YXX}${B}.tar &
  endif
  cd ..
endif
C
chmod ug+x ${HEXE}
/bin/ls -laFq
C
if (-e ./nest) then
  ls -laFq nest
endif
C
C ---  Check to make sure restart file is there
C
if (`echo $LI | awk '{print ($1 > 0.0)}'` && -z restart_in.a) then
  cd $D/..
  /bin/mv LIST LIST_BADRUN
  echo "BADRUN" > LIST
  exit
endif
C
if ($NMPI == 0) then
C
C --- run the model, without MPI or SHMEM
C
if ($NOMP == 0) then
  setenv NOMP 1
endif
C
switch ($OS)
case 'SunOS':
C
C   --- $NOMP CPUs/THREADs, if compiled for OpenMP.
C
    /bin/rm -f core
    touch core
    env OMP_NUM_THREADS=$NOMP ./${HEXE}
    breaksw
case 'Linux':
C
C   --- $NOMP CPUs/THREADs, if compiled for OpenMP.
C
    /bin/rm -f core
    touch core
    env OMP_NUM_THREADS=$NOMP MPSTKZ=8M ./${HEXE}
    breaksw
case 'OSF1':
C
C   --- $NOMP CPUs/THREADs, if compiled for OpenMP.
C
    /bin/rm -f core
    touch core
    env OMP_NUM_THREADS=$NOMP ./${HEXE}
    breaksw
case 'IRIX64':
C
C   --- $NOMP CPUs/THREADs, if compiled for OpenMP.
C
    /bin/rm -f core
    touch core
    setenv FILENV .assign
    assign -R
    assign -s sbin u:18
    assign -V
    env OMP_NUM_THREADS=$NOMP ./${HEXE}
    assign -V
    assign -R
    breaksw
case 'AIX':
C
C   --- $NOMP CPUs/THREADs, if compiled for IBM OpenMP.
C
    /bin/rm -f core
    touch core
    setenv SPINLOOPTIME     500
    setenv YIELDLOOPTIME    500
    setenv XLSMPOPTS       "parthds=${NOMP}:spins=0:yields=0"
    ./${HEXE}
    breaksw
#case 'AIX':
#C
#C   --- $NOMP CPUs/THREADs, if compiled for KAI OpenMP.
#C
#    /bin/rm -f core
#    touch core
#    env OMP_NUM_THREADS=$NOMP ./${HEXE}
#    breaksw
case 'unicos':
C
C   --- $NOMP CPUs/THREADs, if compiled for OpenMP.
C
    /bin/rm -f core
    touch core
    assign -V
    env OMP_NUM_THREADS=$NOMP ./${HEXE}
    if (! -z core)  debug -s ${HEXE} core
    assign -V
    assign -R
    breaksw
endsw
else
C
C --- run the model, with MPI or SHMEM and perhaps also with OpenMP.
C
touch patch.input
if (-z patch.input) then
C
C --- patch.input is always required for MPI or SHMEM.
C
  cd $D/..
  /bin/mv LIST LIST_BADRUN
  echo "BADRUN" > LIST
  exit
endif
C
switch ($OS)
case 'SunOS':
C
C   --- $NMPI MPI tasks and $NOMP THREADs, if compiled for OpenMP.
C
    /bin/rm -f core
    touch core
    setenv OMP_NUM_THREADS $NOMP
#   mpirun -np $NMPI ./${HEXE}
    pam ./${HEXE}
    breaksw
case 'Linux':
C
C   --- $NMPI MPI tasks and $NOMP THREADs, if compiled for OpenMP.
C
    /bin/rm -f core
    touch core
    setenv OMP_NUM_THREADS $NOMP
#    mpirun -np $NMPI ./${HEXE}
     mpiexec_mpt -np $NMPI ./${HEXE}
    breaksw
case 'XT3':
C
C   --- $NMPI MPI tasks.
C
    /bin/rm -f core
    touch core
    setenv NO_STOP_MESSAGE
#   work-around for a lustre bug: pause for 20 seconds
### sleep 20
    setenv MPICH_RANK_REORDER_METHOD	1
    setenv MPI_COLL_OPT_ON		1
#   setenv IOBUF_PARAMS '%stdout,%stderr,*'
    sleep 120
    time yod -small_pages -np $NMPI ./${HEXE}
    breaksw
case 'XT4':
C
C   --- $NMPI MPI tasks.
C
    /bin/rm -f core
    touch core
    setenv NO_STOP_MESSAGE
#   work-around for a lustre bug: pause for 20 seconds
#   sleep 20
    setenv MPICH_RANK_REORDER_METHOD	1
    setenv MPI_COLL_OPT_ON		1
    time aprun -n $NMPI ./${HEXE}
    breaksw
case 'OSF1':
C
C   --- $NMPI MPI tasks and $NOMP THREADs, if compiled for OpenMP.
C
    /bin/rm -f core
    touch core
    setenv OMP_NUM_THREADS $NOMP
#   mpirun -np $NMPI ./${HEXE}
    time prun -n $NMPI ./${HEXE}
    breaksw
case 'IRIX64':
if ($TYPE == "shmem") then
C
C   --- $NMPI SHMEM tasks
C
    /bin/rm -f core
    touch core
    setenv FILENV .assign
    assign -R
    assign -s sbin u:18
    assign -V
    setenv OMP_NUM_THREADS	1
    setenv SMA_DSM_TOPOLOGY	free
    setenv SMA_DSM_VERBOSE	1
    setenv SMA_VERSION		1
    env NPES=$NMPI ./${HEXE}
    assign -V
    assign -R
    breaksw
else
C
C   --- $NMPI MPI tasks and $NOMP THREADs, if compiled for OpenMP.
C
    /bin/rm -f core
    touch core
    setenv FILENV .assign
    assign -R
    assign -s sbin u:18
    assign -V
    setenv OMP_NUM_THREADS	$NOMP
    setenv MPI_DSM_VERBOSE	1
    setenv MPI_REQUEST_MAX	8192
    mpirun -np $NMPI ./${HEXE}
#   mpirun -np $NMPI ./${HEXE} < /dev/null
    assign -V
    assign -R
    breaksw
endif
case 'AIX':
C
C   --- $NMPI MPI tasks and $NOMP THREADs, if compiled for IBM OpenMP.
C
    /bin/rm -f core
    touch core
    setenv SPINLOOPTIME		500
    setenv YIELDLOOPTIME	500
    setenv XLSMPOPTS		"parthds=${NOMP}:spins=0:yields=0"
    setenv MP_SHARED_MEMORY	yes
    setenv MP_SINGLE_THREAD	yes
#   setenv MP_SINGLE_THREAD	no
    setenv MP_EAGER_LIMIT	65536
#   setenv MP_EUILIB		us
#   list where the MPI job will run
#   env MP_LABELIO=YES $POE hostname
    if      (-e /site/bin/launch) then
      setenv MEMORY_AFFINITY    MCM
      setenv UL_MODE            PURE_MPI
      setenv UL_TARGET_CPU_LIST AUTO_SELECT
      time $POE /site/bin/launch ./${HEXE}
    else
      time $POE ./${HEXE}
    endif
    breaksw
#case 'AIX':
#C
#C   --- $NMPI MPI tasks and $NOMP THREADs, if compiled for KAI OpenMP.
#C
#    /bin/rm -f core
#    touch core
#    setenv OMP_NUM_THREADS	$NOMP
#    setenv MP_SHARED_MEMORY	yes
#    setenv MP_SINGLE_THREAD	yes
#    setenv MP_EAGER_LIMIT	65536
#    setenv MP_EUILIB		us
#    setenv MP_EUIDEVICE		css0
##   list where the MPI job will run
#    env MP_LABELIO=YES $POE hostname
#    time $POE ./${HEXE}
#    breaksw
default:
    echo "This O/S," $OS ", is not configured for MPI/SHMEM"
    exit (1)
endsw
endif
C
touch   PIPE_DEBUG
/bin/rm PIPE_DEBUG
C
C --- archive output in a separate tar directory
C
touch archv.dummy.a archv.dummy.b archv.dummy.txt
touch archm.dummy.a archm.dummy.b archm.dummy.txt
touch arche.dummy.a arche.dummy.b arche.dummy.txt
touch cice.dummy.nc
C
if (-e ./SAVE) then
  foreach t ( v m e )
    foreach f (arch${t}.*.a)
      /bin/ln ${f} SAVE/${f}
    end
    foreach f (arch${t}.*.b)
      /bin/ln ${f} SAVE/${f}
    end
    foreach f (arch${t}.*.txt)
      /bin/ln ${f} SAVE/${f}
    end
  end
  foreach f (cice.*.nc)
    /bin/ln -f ${f} SAVE/${f}
  end
endif
C
foreach t ( v m e )
  mkdir ./tar${t}_${Y01}${A}
switch ($OS)
case 'XT3':
case 'XT4':
  lfs setstripe ./tar${t}_${Y01}${A} 1048576 -1 8
  breaksw
endsw
  foreach f (arch${t}.*.a)
    /bin/mv ${f} ./tar${t}_${Y01}${A}/${E}_${f}
  end
  foreach f (arch${t}.*.b)
    /bin/mv ${f} ./tar${t}_${Y01}${A}/${E}_${f}
  end
  foreach f (arch${t}.*.txt)
    /bin/mv ${f} ./tar${t}_${Y01}${A}/${E}_${f}
  end
  date
end
foreach f (cice.*.nc)
  /bin/mv ${f} ./tarc_${Y01}${A}/${E}_${f}
end
C 
if (! -z archt.input) then
  if (-e ./tart_${Y01}${A}) then
    /bin/mv ./tart_${Y01}${A} ./tart_${Y01}${A}_$$
  endif
  /bin/mv ./ARCHT ./tart_${Y01}${A}
endif
C
C --- add last day to next months tar directory, for actual day months only
C
setenv DL `awk  '{printf("%15.2f\n", $2)}' limits`
setenv DA `echo 3 1.0 1.0 $DL $DL | ~/hycom/ALL/bin/hycom_nest_dates | head -1`
foreach t ( v c )
  mkdir ./tar${t}_${YXX}${B}
  ln -f ./tar${t}_${Y01}${A}/${E}_arch?.${DA}.* ./tar${t}_${YXX}${B}
end
C
C --- build and run or submit the tar script
C
awk -f $D/../${E}.awk y01=${Y01} ab=${A} $D/../${E}A.com >! tar_${Y01}${A}.com1
sed -e "s/PBS -o XXX.log/PBS -o tar_${Y01}${A}.log/" tar_${Y01}${A}.com1  >! tar_${Y01}${A}.com
rm -f tar_${Y01}${A}.com1
#qsub tar_${Y01}${A}.com 
#csh tar_${Y01}${A}.com >&! tar_${Y01}${A}.log &
#llsubmit ./tar_${Y01}${A}.com
#~wallcraf/bin/q_navo tar_${Y01}${A}.com
C
C --- heat transport statistics output
C
if (-e flxdp_out.a) then
  ${pput} flxdp_out.a ${S}/flxdp_${Y01}${A}.a
endif
if (-e flxdp_out.b) then
  ${pput} flxdp_out.b ${S}/flxdp_${Y01}${A}.b
endif
if (-e ovrtn_out) then
  ${pput} ovrtn_out ${S}/ovrtn_${Y01}${A}
endif
C
C --- restart output
C
if (-e restart_out.a) then
  ${pput} restart_out.a ${S}/restart_${YXX}${B}.a
endif
if (-e restart_out.b) then
  ${pput} restart_out.b ${S}/restart_${YXX}${B}.b
endif
endif
if (-e ./cice) then
C
C --- CICE restart output, assumes single-month runs
C
  /bin/mv cice.restart*01 cice.restart_out
  if (-e cice.restart_out) then
    ${pput} cice.restart_out ${S}/cice.restart_${YXX}${B}
  endif
endif
C
if (-e ./wind) then
C
C --- Delete just in time wind and flux files.
C
  touch summary_out
  tail -1 summary_out
  tail -1 summary_out | grep -c "^normal stop"
  if ( `tail -1 summary_out | grep -c "^normal stop"` == 1 ) then
#    /bin/rm -f ./wind/*_${Y01}${A}.[ab]
#    /bin/rm -f ./wspd/*_${Y01}${A}.[ab]
#    /bin/rm -f ./wvel/*_${Y01}${A}.[ab]
#    /bin/rm -f ./grad/*_${Y01}${A}.[ab]
#    /bin/rm -f ./flux/*_${Y01}${A}.[ab]
#    /bin/rm -f ./flxt/*_${Y01}${A}.[ab]
#    /bin/rm -f ./pcip/*_${Y01}${A}.[ab]
#    /bin/rm -f ./ssta/*_${Y01}${A}.[ab]
#    /bin/rm -f ./ssto/*_${Y01}${A}.[ab]
#    /bin/rm -f ./cice/*_${Y01}${A}.[rB]
  endif
C
  if (-e ./wind/${E}w${Y01}${A}.com) then
    /bin/mv ./wind/${E}w${Y01}${A}.{com,log} $D/..
  endif
  if (-e ./wspd/${E}s${Y01}${A}.com) then
    /bin/mv ./wspd/${E}s${Y01}${A}.{com,log} $D/..
  endif
  if (-e ./wvel/${E}v${Y01}${A}.com) then
    /bin/mv ./wvel/${E}v${Y01}${A}.{com,log} $D/..
  endif
  if (-e ./grad/${E}g${Y01}${A}.com) then
    /bin/mv ./grad/${E}g${Y01}${A}.{com,log} $D/..
  endif
  if (-e ./flux/${E}f${Y01}${A}.com) then
    /bin/mv ./flux/${E}f${Y01}${A}.{com,log} $D/..
  endif
  if (-e ./flxt/${E}q${Y01}${A}.com) then
    /bin/mv ./flxt/${E}q${Y01}${A}.{com,log} $D/..
  endif
  if (-e ./pcip/${E}p${Y01}${A}.com) then
    /bin/mv ./pcip/${E}p${Y01}${A}.{com,log} $D/..
  endif
  if (-e ./ssta/${E}t${Y01}${A}.com) then
    /bin/mv ./ssta/${E}t${Y01}${A}.{com,log} $D/..
  endif
  if (-e ./ssto/${E}o${Y01}${A}.com) then
    /bin/mv ./ssto/${E}o${Y01}${A}.{com,log} $D/..
  endif
C
C --- Wait for wind and flux interpolation of next segment.
C
  wait
C
  if (-e ./wind/${E}w${YXX}${B}.com) then
    /bin/mv ./wind/${E}w${YXX}${B}.{com,log} $D/..
  endif
  if (-e ./wspd/${E}s${Y01}${A}.com) then
    /bin/mv ./wspd/${E}s${Y01}${A}.{com,log} $D/..
  endif
  if (-e ./wvel/${E}v${Y01}${A}.com) then
    /bin/mv ./wvel/${E}v${Y01}${A}.{com,log} $D/..
  endif
  if (-e ./grad/${E}g${Y01}${A}.com) then
    /bin/mv ./grad/${E}g${Y01}${A}.{com,log} $D/..
  endif
  if (-e ./flux/${E}f${YXX}${B}.com) then
    /bin/mv ./flux/${E}f${YXX}${B}.{com,log} $D/..
  endif
  if (-e ./flxt/${E}q${YXX}${B}.com) then
    /bin/mv ./flxt/${E}q${YXX}${B}.{com,log} $D/..
  endif
  if (-e ./pcip/${E}p${YXX}${B}.com) then
    /bin/mv ./pcip/${E}p${YXX}${B}.{com,log} $D/..
  endif
  if (-e ./ssta/${E}t${YXX}${B}.com) then
    /bin/mv ./ssta/${E}t${YXX}${B}.{com,log} $D/..
  endif
  if (-e ./ssto/${E}o${YXX}${B}.com) then
    /bin/mv ./ssto/${E}o${YXX}${B}.{com,log} $D/..
  endif
endif
C
C --- wait for nesting .tar file.
C
if (-e ./nest) then
  wait
endif
C
C --- HYCOM error stop is implied by the absence of a normal stop.
C
touch summary_out
tail -1 summary_out
tail -1 summary_out | grep -c "^normal stop"
if ( `tail -1 summary_out | grep -c "^normal stop"` == 0 ) then
  cd $D/..
  /bin/mv LIST LIST_BADRUN
  echo "BADRUN" > LIST
endif
C
C --- wait for tar bundles to complete
C
wait
C
C  --- END OF MODEL RUN SCRIPT
C
