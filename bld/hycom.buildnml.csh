#! /bin/csh -f

set NX = $OCN_NX
set NY = $OCN_NY

 #******************************************************************#
 # If the user changes any input datasets - be sure to give it a    #
 # unique filename. Do not duplicate any existing input files       #
 #******************************************************************#

cd $RUNDIR
mkdir -p OUTPUT

## store output files
ls archv.* >& /dev/null
if ( $status == 0 ) then
  mv archv.* OUTPUT/
endif
ls archm.* >& /dev/null
if ( $status == 0 ) then
  mv archm.* OUTPUT/
endif
ls *cice.h.*.nc >& /dev/null
if ( $status == 0 ) then
  mv *cice.h.*.nc OUTPUT/
endif
if (-e ovrtn_out) then
  rm ovrtn_out
endif

## Get input files
## NCAR Machine
if ($MACH == "yellowstone") then
  echo "We are on" ${MACH}
  echo "Getting ocean input files"
## get the input file depending on the grid
  if ($OCN_GRID == "gh72") then
      if (-e /glade/p/work/abozec/hycom/HYCOM_CESM/INPUT_glbt072/) then
        cp -p -f /glade/p/work/abozec/hycom/HYCOM_CESM/INPUT_glbt072/* .
      else
        echo "error copying hycom input data file, abort"
        exit 2
      endif
  else if ($OCN_GRID == "g16") then
      if (-e /glade/p/work/abozec/hycom/HYCOM_CESM/INPUT_h98/) then
        cp -p -f /glade/p/work/abozec/hycom/HYCOM_CESM/INPUT_h98/* .
      else
        echo "error copying hycom input data file, abort"
        exit 2
      endif
  endif
## NAVY Machines
else  if ($MACH == "shepard") then
  echo "We are on" ${MACH}
  echo "Getting ocean input files"
## get the input file depending on the grid
  if ($OCN_GRID == "gh72") then
      if (-e /p/work1/abozec/hycom/HYCOM_CESM/INPUT_glbt072/) then
        cp -p -f /p/work1/abozec/hycom/HYCOM_CESM/INPUT_glbt072/* .
      else
        echo "error copying hycom input data file, abort"
        exit 2
      endif
  else if ($OCN_GRID == "g16") then
      if (-e /p/work1/abozec/hycom/HYCOM_CESM/INPUT_h98/) then
        cp -p -f /p/work1/abozec/hycom/HYCOM_CESM/INPUT_h98/* .
      else
        echo "error copying hycom input data file, abort"
        exit 2
      endif
  endif
else  if ($MACH == "kilrain") then
  echo "We are on" ${MACH}
  echo "Getting ocean input files"
## get the input file depending on the grid
  if ($OCN_GRID == "gh72") then
      if (-e /scr/abozec/HYCOM_CESM/INPUT_glbt072/) then
        cp -p -f /scr/abozec/HYCOM_CESM/INPUT_glbt072/* .
      else
        echo "error copying hycom input data file, abort"
        exit 2
      endif
  else if ($OCN_GRID == "g16") then
      if (-e /scr/abozec/HYCOM_CESM/INPUT_h98/) then
        cp -p -f /scr/abozec/HYCOM_CESM/INPUT_h98/* .
      else
        echo "error copying hycom input data file, abort"
        exit 2
      endif
  endif
endif

## determine if we need precip_fact
if (-e ${CASEROOT}/env_run.xml) then
  setenv cpl_epbal `awk -F '[""]' '/CPL_EPBAL/ {print $4}' ${CASEROOT}/env_run.xml`
else
  setenv cpl_epbal "off"
endif
echo 'CPL_EPBAL:' ${cpl_epbal}
if (${cpl_epbal} == ocn) then
  echo 'copy blkdat.input for GHY compset' 
  cp -f blkdat.input_G blkdat.input
else
  echo 'copy blkdat.input for BHY compset'
  cp -f blkdat.input_B blkdat.input
endif

set base_filename = "hycom_in"

set inst_counter = 1

while ($inst_counter <= $NINST_OCN)

    set inst_string = " "
    if ($NINST_OCN > 1) then
        set inst_string = $inst_counter
        if ($inst_counter <= 999) set inst_string = 0$inst_string
        if ($inst_counter <=  99) set inst_string = 0$inst_string
        if ($inst_counter <=   9) set inst_string = 0$inst_string
        set inst_string = _$inst_string
    endif

    set in_filename = ${base_filename}${inst_string}

cat >! ${in_filename} << EOF
$NX                  !  i-direction global dimension
$NY                  !  j-direction global dimension
4                    !  decomp_type  1=1d-by-lat, 2=1d-by-lon, 3=2d, 4=2d evensquare, 11=segmented
0                    !  num of pes for i (type 3 only)
0                    !  length of segments (type 4 only)
EOF

    @ inst_counter = $inst_counter + 1

end


