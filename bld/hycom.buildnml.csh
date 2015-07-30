#! /bin/csh -f

set NX = $OCN_NX
set NY = $OCN_NY

 #******************************************************************#
 # If the user changes any input datasets - be sure to give it a    #
 # unique filename. Do not duplicate any existing input files       #
 #******************************************************************#

cd $RUNDIR
mkdir -p OUTPUT

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

if (-e /glade/p/work/abozec/hycom/HYCOM_CESM/INPUT_h98/) then
  cp -p -f /glade/p/work/abozec/hycom/HYCOM_CESM/INPUT_h98/* .
else
  echo "error copying hycom input data file, abort"
  exit 2
endif

## determine if we need precip_fact
setenv cpl_epbal `awk -F '[""]' '/CPL_EPBAL/ {print $4}' ${CASEROOT}/env_run.xml`
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

echo "HYCOM configure done."
