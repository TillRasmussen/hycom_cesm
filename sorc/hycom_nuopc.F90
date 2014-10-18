#ifdef HYCOM_IN_CESM
module ocn_comp_nuopc_mod
#else
module hycom
#endif

  !-----------------------------------------------------------------------------
  ! OCN Component.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Model, only: &
    model_routine_SS      => routine_SetServices, &
    model_label_Advance   => label_Advance
  
  use MOD_HYCOM, only : HYCOM_Init, HYCOM_Run, HYCOM_Final, &
    end_of_run, end_of_run_cpl
    
  use hycom_nuopc_glue
  use ESMF_IOScripMod
#ifdef HYCOM_IN_CESM
  use esmf
  use esmfshr_util_mod, only : esmfshr_util_ArrayGetIndex
  use esmfshr_util_mod, only : esmfshr_util_ArrayGetSize
  use esmf2mct_mod    , only : esmf2mct_init

  use seq_flds_mod
  use mct_mod,          only : mct_gsMap, mct_gGrid, mct_gsMap_gsize
  use shr_string_mod,   only : shr_string_listGetNum
  use seq_flds_mod
  use esmfshr_nuopc_mod
#endif

  implicit none
  
  private
  
  ! private internal state to keep instance data
  type InternalStateStruct
    type(hycom_nuopc_glue_type)   :: glue
    integer                       :: slice
  end type

  type InternalState
    type(InternalStateStruct), pointer :: wrap
  end type
  
  public SetServices

#ifdef HYCOM_IN_CESM
  type(mct_gsMap), public, pointer  :: gsmap_o
  type(mct_gGrid), public, pointer  :: dom_o
  integer                           :: OCNID      
#endif
  
  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  subroutine SetServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS
    
    ! the NUOPC model component will register the generic methods
    call NUOPC_CompDerive(gcomp, model_routine_SS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! set entry point for methods that require specific implementation
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv00p1"/), userRoutine=InitializeAdvertise, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv00p2"/), userRoutine=InitializeRealize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! overwrite Finalize
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_FINALIZE, &
      userRoutine=Finalize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! attach specializing method(s)
    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_Advance, &
      specRoutine=ModelAdvance, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#ifdef HYCOM_IN_CESM
    ! Run routine to execute run functionality for tight coupling
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=routine_Run2, phase=2, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Run routine to execute run functionality for loose coupling
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=routine_Run3, phase=3, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif
    
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine InitializeAdvertise(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS

    ! importable fields:
    !call NUOPC_StateAdvertiseFields(importState, &
    !  StandardNames=(/ &
    !  "surface_downward_eastward_stress       ",    & ! from ATM
    !  "surface_downward_northward_stress      ",    & ! from ATM
    !  "wind_speed_height10m                   ",    & ! from ATM
    !  "friction_speed                         ",    & ! from ATM
    !  "mean_down_sw_flx                       ",    & ! from ATM
    !  "mean_net_sw_flx                        ",    & ! from ATM
    !  "mean_net_lw_flx                        ",    & ! from ATM
    !  "inst_temp_height2m                     ",    & ! from ATM
    !  "mean_prec_rate                         ",    & ! from ATM
    !  "inst_spec_humid_height2m               ",    & ! from ATM
    !  "sea_surface_temperature                ",    & ! from ATM
    !  "sea_ice_area_fraction                  ",    & ! from SEA-ICE
    !  "downward_x_stress_at_sea_ice_base      ",    & ! from SEA-ICE
    !  "downward_y_stress_at_sea_ice_base      ",    & ! from SEA-ICE
    !  "downward_sea_ice_basal_solar_heat_flux ",    & ! from SEA-ICE
    !  "upward_sea_ice_basal_heat_flux         ",    & ! from SEA-ICE
    !  "downward_sea_ice_basal_salt_flux       ",    & ! from SEA-ICE
    !  "downward_sea_ice_basal_water_flux      ",    & ! from SEA-ICE
    !  "sea_ice_temperature                    ",    & ! from SEA-ICE
    !  "sea_ice_thickness                      ",    & ! from SEA-ICE
    !  "sea_ice_x_velocity                     ",    & ! from SEA-ICE
    !  "sea_ice_y_velocity                     "/),  & ! from SEA-ICE
    !  rc=rc)
    !if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    !  line=__LINE__, &
    !  file=__FILE__)) &
    !  return  ! bail out
    !
    !! exportable fields:
    !call NUOPC_StateAdvertiseFields(exportState, &
    !  StandardNames=(/ &
    !  "sea_surface_temperature                  ",    &
    !  "upward_sea_ice_basal_available_heat_flux ",    &
    !  "sea_lev                                  ",    &
    !  "mixed_layer_depth                        ",    &
    !  "s_surf                                   ",    &
    !  "ocn_current_zonal                        ",    &
    !  "ocn_current_merid                        "/),  &
    !  rc=rc)
    !if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    !  line=__LINE__, &
    !  file=__FILE__)) &
    !  return  ! bail out

    ! set Component name so it becomes identifiable
    call ESMF_GridCompSet(gcomp, name="HYCOM", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#ifdef HYCOM_IN_CESM
    !! Setup import-able fields
    call esmfshr_nuopc_advertise_fields( &
      ocn_import_fields, importState, tag='HYCOM import', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
    return ! bail out

    !! Setup export-able fields
    call esmfshr_nuopc_advertise_fields( &
      ocn_export_fields, exportState, tag='HYCOM export', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
    return ! bail out
#endif
      
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine InitializeRealize(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! This method assumes that the incoming clock indicates the full simulation
    ! interval between startTime and stopTime.
    
    ! local variables    
    type(ESMF_Field)            :: field
    type(ESMF_Grid)             :: gridIn
    type(ESMF_Grid)             :: gridOut
    type(ESMF_array)            :: array
    type(ESMF_VM)               :: vm
    integer                     :: mpiComm
    type(ESMF_Time)             :: startTime, stopTime, hycomRefTime
    type(ESMF_TimeInterval)     :: interval
    real(ESMF_KIND_R8)          :: startTime_r8, stopTime_r8
    type(InternalState)         :: is
    integer                     :: stat
    type(ESMF_CALKIND_FLAG)     :: calkind
#ifdef HYCOM_IN_CESM            
    type(ESMF_State)            :: import_state, export_state
    type(ESMF_Mesh)             :: mesh
    type(ESMF_DistGrid)         :: distgrid
    type(ESMF_Array)            :: o2x, x2o, dom
    type(ESMF_ArraySpec)        :: arrayspec
    type(ESMF_Delayout)         :: delayout
    integer                     :: ldeCount, eleCount, lsize, mpicom_ocn, nfields, lde
#endif
    
    rc = ESMF_SUCCESS
    
    ! Allocate memory for the internal state and set it in the Component.
    allocate(is%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of the internal state memory failed.", &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridCompSetInternalState(gcomp, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Prepare to call into HYCOM_Init
    call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_VMGet(vm, mpiCommunicator=mpiComm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! Translate startTime and stopTime into HYCOM format
    call ESMF_ClockGet(clock, startTime=startTime, stopTime=stopTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_TimeSet(hycomRefTime, yy=1900, mm=12, dd=31, calkindflag=ESMF_CALKIND_GREGORIAN, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    interval = startTime - hycomRefTime
    call ESMF_TimeIntervalGet(interval, d_r8=startTime_r8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    interval = stopTime - hycomRefTime
    call ESMF_TimeIntervalGet(interval, d_r8=stopTime_r8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    print *, " HYCOM_INIT -->> startTime_r8=", startTime_r8, "stopTime_r8=", stopTime_r8

    call ESMF_LOGWRITE("BEFORE HYCOM_INIT", ESMF_LOGMSG_INFO, rc=rc)
    
    ! Call into the HYCOM initialization  
    call HYCOM_Init(mpiComm, & ! -->> call into HYCOM <<--
!      hycom_start_dtg=-0.d0, hycom_end_dtg=stopTime_r8)
      hycom_start_dtg=startTime_r8, hycom_end_dtg=stopTime_r8)

    call ESMF_LOGWRITE("AFTER HYCOM_INIT", ESMF_LOGMSG_INFO, rc=rc)
    
    ! Write some HYCOM distribution info into the Log.
    call HYCOM_TileInfo(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! Fill in the glue structure.
    call HYCOM_GlueInitialize(is%wrap%glue, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! use the HYCOM Grid that was setup inside of the glue structure
    gridIn  = is%wrap%glue%grid ! for imported Fields
    gridOut = is%wrap%glue%grid ! for exported Fields

#if 0
    ! dump the HYCOM Grid coordinate and mask arrays for reference      
    call ESMF_GridGetCoord(gridIn, coordDim=1, array=array, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ArrayWrite(array, file="array_hycom_grid_coord1.nc", overwrite=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridGetCoord(gridIn, coordDim=2, array=array, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ArrayWrite(array, file="array_hycom_grid_coord2.nc", overwrite=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridGetItem(gridIn, staggerLoc=ESMF_STAGGERLOC_CENTER, &
      itemflag=ESMF_GRIDITEM_MASK, array=array, rc=rc)    
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ArrayWrite(array, file="array_hycom_grid_mask.nc", overwrite=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridGetItem(gridIn, staggerLoc=ESMF_STAGGERLOC_CENTER, &
      itemflag=ESMF_GRIDITEM_AREA, array=array, rc=rc)    
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ArrayWrite(array, file="array_hycom_grid_area.nc", overwrite=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_OutputScripGridFile("hycom_1xv6_grid.nc", gridIn, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
#endif

    
    ! conditionally realize or remove Fields in import and export States
    ! also keep track of these Fields on the glue layer
    
    ! importable fields:
    !call HYCOM_GlueFieldsRealize(is%wrap%glue, importState, &
    !  StandardNames=(/ &
    !  "surface_downward_eastward_stress       ",    & ! from ATM
    !  "surface_downward_northward_stress      ",    & ! from ATM
    !  "wind_speed_height10m                   ",    & ! from ATM
    !  "friction_speed                         ",    & ! from ATM
    !  "mean_down_sw_flx                       ",    & ! from ATM
    !  "mean_net_sw_flx                        ",    & ! from ATM
    !  "mean_net_lw_flx                        ",    & ! from ATM
    !  "inst_temp_height2m                     ",    & ! from ATM
    !  "mean_prec_rate                         ",    & ! from ATM
    !  "inst_spec_humid_height2m               ",    & ! from ATM
    !  "sea_surface_temperature                ",    & ! from ATM
    !  "sea_ice_area_fraction                  ",    & ! from SEA-ICE
    !  "downward_x_stress_at_sea_ice_base      ",    & ! from SEA-ICE
    !  "downward_y_stress_at_sea_ice_base      ",    & ! from SEA-ICE
    !  "downward_sea_ice_basal_solar_heat_flux ",    & ! from SEA-ICE
    !  "upward_sea_ice_basal_heat_flux         ",    & ! from SEA-ICE
    !  "downward_sea_ice_basal_salt_flux       ",    & ! from SEA-ICE
    !  "downward_sea_ice_basal_water_flux      ",    & ! from SEA-ICE
    !  "sea_ice_temperature                    ",    & ! from SEA-ICE
    !  "sea_ice_thickness                      ",    & ! from SEA-ICE
    !  "sea_ice_x_velocity                     ",    & ! from SEA-ICE
    !  "sea_ice_y_velocity                     "/),  & ! from SEA-ICE
    !  rc=rc)
    !if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    !  line=__LINE__, &
    !  file=__FILE__)) &
    !  return  ! bail out

    !! exportable fields:
    !call HYCOM_GlueFieldsRealize(is%wrap%glue, exportState, &
    !  StandardNames=(/ &
    !  "sea_surface_temperature                  ",    &
    !  "upward_sea_ice_basal_available_heat_flux ",    &
    !  "sea_lev                                  ",    &
    !  "mixed_layer_depth                        ",    &
    !  "s_surf                                   ",    &
    !  "ocn_current_zonal                        ",    &
    !  "ocn_current_merid                        "/),  &
    !  rc=rc)
    !if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    !  line=__LINE__, &
    !  file=__FILE__)) &
    !  return  ! bail out

    ! Import data to HYCOM native structures through glue fields.
    call HYCOM_GlueFieldsDataImport(is%wrap%glue, .true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Export HYCOM native data into the glue fields.
    call HYCOM_GlueFieldsDataExport(is%wrap%glue, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Reset the slice counter
    is%wrap%slice = 1

#if 0
    ! Compute regridding weights ATM->OCN/ICE
    ! ATM2OCN CONSV, FIX THIS AFTER CORNER STAGGER COORDINATES ARE ADDED
    call HYCOM_CESM_REGRID_TOOCN("/glade/p/cesm/cseg/mapping/grids/T62_040121.nc", &
      !is%wrap%glue%grid, "map_T62_TO_gx1v6_aave.130322.nc", ESMF_REGRIDMETHOD_CONSERVE, &
      is%wrap%glue%grid, "map_T62_TO_gx1v6_aave.130322.nc", ESMF_REGRIDMETHOD_BILINEAR, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! ATM2OCN BILINEAR
    call HYCOM_CESM_REGRID_TOOCN("/glade/p/cesm/cseg/mapping/grids/T62_040121.nc", &
      is%wrap%glue%grid, "map_T62_TO_gx1v6_blin.130322.nc", ESMF_REGRIDMETHOD_BILINEAR, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! ATM2OCN PATCH
    call HYCOM_CESM_REGRID_TOOCN("/glade/p/cesm/cseg/mapping/grids/T62_040121.nc", &
      is%wrap%glue%grid, "map_T62_TO_gx1v6_patc.130322.nc", ESMF_REGRIDMETHOD_BILINEAR, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! OCN/ICE->ATM CONS
    call HYCOM_CESM_REGRID_FROMOCN(is%wrap%glue%grid, "/glade/p/cesm/cseg/mapping/grids/T62_040121.nc", &
      "map_gx1v6_TO_T62_aave.130322.nc", ESMF_REGRIDMETHOD_BILINEAR, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif

#ifdef HYCOM_IN_CESM
    ! Prepare for CESM SPECIFIC DATA STRUCTURES
    import_state = importState
    export_state = exportState

    ! duplicate the mpi communicator from the current VM 
    call MPI_Comm_dup(mpicomm, mpicom_ocn, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! satisfy some external needs
    call ESMF_AttributeGet(export_state, name="ID", value=OCNID, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Fold DistGrid from HYCOM GRID into 1D arb DistGrid on Mesh
    mesh = ESMF_GridToMesh(is%wrap%glue%grid, ESMF_STAGGERLOC_CENTER, 1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_MeshGet(Mesh, nodalDistgrid=distgrid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    !-----------------------------------------
    !  Set arrayspec for dom, o2x and x2o
    !-----------------------------------------
    
    call ESMF_ArraySpecSet(arrayspec, rank=2, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    call ESMF_DistGridGet(distgrid, delayout=delayout, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_DelayoutGet(delayout, localDeCount=ldeCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    print *, 'HYCOM DG DELAYOUT localDECount: ', ldeCount

    lsize = 0
    do lde = 0, ldeCount-1
      call ESMF_DistGridGet(distgrid, lde, elementCount=eleCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      lsize = lsize + eleCount
    enddo

    print *, 'HYCOM DG DELAYOUT lsize: ', lsize

    !-----------------------------------------
    ! Create dom 
    !-----------------------------------------

    nfields = shr_string_listGetNum(trim(seq_flds_dom_fields))

    dom = ESMF_ArrayCreate(distgrid=distgrid, arrayspec=arrayspec, distgridToArrayMap=(/2/), &
         undistLBound=(/1/), undistUBound=(/nfields/), name="domain", rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(dom, name="mct_names", value=trim(seq_flds_dom_fields), rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    ! Set values of dom (needs ocn initialization info)

    call ocn_domain_esmf(dom, is%wrap%glue%grid)
   
    !----------------------------------------- 
    !  Create o2x 
    !-----------------------------------------

    ! 1d undistributed index of fields, 2d is packed data

    nfields = shr_string_listGetNum(trim(seq_flds_o2x_fields))

    o2x = ESMF_ArrayCreate(distgrid=distgrid, arrayspec=arrayspec, distgridToArrayMap=(/2/), &
         undistLBound=(/1/), undistUBound=(/nfields/), name="d2x", rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(o2x, name="mct_names", value=trim(seq_flds_o2x_fields), rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    !----------------------------------------- 
    !  Create x2o 
    !-----------------------------------------

    nfields = shr_string_listGetNum(trim(seq_flds_x2o_fields))

    x2o = ESMF_ArrayCreate(distgrid=distgrid, arrayspec=arrayspec, distgridToArrayMap=(/2/), &
         undistLBound=(/1/), undistUBound=(/nfields/), name="x2d", rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(x2o, name="mct_names", value=trim(seq_flds_x2o_fields), rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    !----------------------------------------- 
    ! Add esmf arrays to import and export state 
    !-----------------------------------------

    call ESMF_StateAdd(export_state, (/dom/), rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    call ESMF_StateAdd(export_state, (/o2x/), rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
 
    call ESMF_StateAdd(import_state, (/x2o/), rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    allocate(gsmap_o)
    allocate(dom_o)
   
    call esmf2mct_init(distgrid, OCNID, gsmap_o, mpicom_ocn, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    call esmf2mct_init(dom, dom_o, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    print *, 'HYCOM GSMAP gsize: ', mct_gsMap_gsize(gsmap_o)

    call ESMF_AttributeSet(export_state, name="gsize", value=mct_gsMap_gsize(gsmap_o), rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    !! Create and Realize Importable fields
    call esmfshr_nuopc_create_fields( &
      ocn_import_fields, mesh, importState, tag='HYCOM import', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
    return ! bail out

    !! Create and Realize Exportable fields
    call esmfshr_nuopc_create_fields( &
      ocn_export_fields, mesh, exportState, tag='HYCOM export', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
    return ! bail out

    call ocn_forcing(exportState, o2x, '/glade/u/home/feiliu/work/raw_forcing_data/pop2_export_all_fields_init.raw', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
    return ! bail out

    call esmfshr_nuopc_copy(ocn_export_fields, 'd2x', exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
    return ! bail out

#endif
    
  end subroutine

#ifdef HYCOM_IN_CESM
!***********************************************************************
!BOP
! !IROUTINE: ocn_domain_esmf
! !INTERFACE:

 subroutine ocn_domain_esmf( dom, grid )

! !DESCRIPTION:
!  This routine creates the ocean domain
!
! !REVISION HISTORY:
!  same as module
!
! !INPUT/OUTPUT PARAMETERS:

    implicit none
    type(ESMF_Array), intent(inout)     :: dom
    type(ESMF_Grid),  intent(in)        :: grid

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

    integer :: rc

    integer ::   &
      i,j, n, iblock

    integer ::   &
      klon,klat,karea,kmask,kfrac ! domain fields

    real(ESMF_KIND_R8),    pointer ::  &
      fptr (:,:)          ! data pointer into ESMF array

    real(ESMF_KIND_R8)  :: &
      frac                ! temporary var to compute frac/mask from KMT

    type(ESMF_DistGrid)  :: distgrid
    type(ESMF_VM)        :: vm
    character(len=256)   :: msg
    integer              :: n_elem, n_pet, lpet
    integer, pointer     :: indexlist(:)
    logical              :: arbIndexFlag
    type(ESMF_Array)     :: lon1d, lat1d, area1d, mask1d
    type(ESMF_Array)     :: plon, plat, area, mask
    type(ESMF_Routehandle) :: rh, rh1
    integer              :: elb(2,1), eub(2,1), elb1(1,1), eub1(1,1)
    real(ESMF_KIND_R8), pointer  :: tlon(:), tlat(:), tarea(:)
    integer(ESMF_KIND_I4), pointer :: tmask(:)
    real(ESMF_KIND_R8)   :: radian, radius, pi
    type(ESMF_TYPEKIND_FLAG)  :: tkf

!-----------------------------------------------------------------------

    ! Retrieve dom data pointer
    call ESMF_ArrayGet(dom, localDe=0, farrayPtr=fptr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Retrieve the HYCOM Grid coordinate and mask arrays for reference      
    call ESMF_GridGetCoord(grid, coordDim=1, array=plon, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridGetCoord(grid, coordDim=2, array=plat, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridGetItem(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, &
      itemflag=ESMF_GRIDITEM_MASK, array=mask, rc=rc)    
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ArrayGet(mask, typekind=tkf, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridGetItem(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, &
      itemflag=ESMF_GRIDITEM_AREA, array=area, rc=rc)    
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_VMGetCurrent(vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_VMGet(vm, petCount=n_pet, localPet=lpet, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Use the mesh based 1D distgrid to create DOM elements
    call ESMF_ArrayGet(dom, distgrid=distgrid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    lon1D = ESMF_ArrayCreate(distgrid, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ArrayGet(lon1D, farrayPtr = tlon, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    lat1D = ESMF_ArrayCreate(distgrid, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ArrayGet(lat1D, farrayPtr = tlat, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    area1D = ESMF_ArrayCreate(distgrid, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ArrayGet(area1D, farrayPtr = tarea, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    mask1D = ESMF_ArrayCreate(distgrid, typekind=ESMF_TYPEKIND_I4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ArrayGet(mask1D, farrayPtr = tmask, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! CESM uses 1 DE per PET
    call ESMF_DistGridGet(distgrid, 0, arbSeqIndexFlag=arbIndexFlag, elementCount=n_elem, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    allocate(indexList(n_elem))
    call ESMF_DistGridGet(distgrid, 0, seqIndexList=indexlist, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

!-------------------------------------------------------------------
!
!  initialize domain type, lat/lon in degrees,
!  area in radians^2, mask is 1 (ocean), 0 (non-ocean)
!  Fill in correct values for domain components
!
!-------------------------------------------------------------------

    klon  = esmfshr_util_ArrayGetIndex(dom,'lon ',rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    klat  = esmfshr_util_ArrayGetIndex(dom,'lat ',rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    karea = esmfshr_util_ArrayGetIndex(dom,'area',rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    kmask = esmfshr_util_ArrayGetIndex(dom,'mask',rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    kfrac = esmfshr_util_ArrayGetIndex(dom,'frac',rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    fptr(:,:) = -9999.0_ESMF_KIND_R8
    n=0

    write(msg, *) 'DUMPING HYCOM INDICES BEGINS:'
    call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    write(msg, *) 'total number of ocean pet', n_pet, ' local pet number', lpet
    call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    write(msg, *) 'number of elements on this pet:', n_elem, ' arbflag', arbIndexFlag
    call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    write(msg, *) 'lpet ', 'n ', 'Index ', 'lon ', 'lat ', &
      'area ', 'frac ', 'mask'
    call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    call ESMF_ArrayGet(plon, exclusiveLBound=elb, exclusiveUBound=eub, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    write(msg, *) 'src shape: ', elb, eub, ' dst shape: ', lbound(fptr), ubound(fptr)
    call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    write(msg, *) 'plon: ', elb, eub
    call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    call ESMF_ArrayGet(lon1d, exclusiveLBound=elb1, exclusiveUBound=eub1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    write(msg, *) 'lon1d: ', elb1, eub1
    call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    call ESMF_ArrayRedistStore(plon, lon1d, routehandle=rh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ArrayRedistStore(mask, mask1d, routehandle=rh1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ArrayRedist(plon, lon1d, routehandle=rh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ArrayRedist(plat, lat1d, routehandle=rh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ArrayRedist(mask, mask1d, routehandle=rh1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ArrayRedist(area, area1d, routehandle=rh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_RouteHandleRelease(rh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_RouteHandleRelease(rh1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    !do iblock = 1, nblocks_clinic
    !   this_block = get_block(blocks_clinic(iblock),iblock)
    !   do j=this_block%jb,this_block%je
    !   do i=this_block%ib,this_block%ie
    !      n=n+1
    !      fptr(klon , n)          = TLON(i,j,iblock)*radian
    !      fptr(klat , n)          = TLAT(i,j,iblock)*radian 
    !      fptr(karea, n)          = TAREA(i,j,iblock)/(radius*radius)
    !      frac                    = float(KMT(i,j,iblock)) 
    !      if (frac > 1.0_r8) frac = 1.0_r8
    !      fptr(kfrac, n)          = frac
    !      fptr(kmask, n)          = frac
    !      write(msg, '(I4,A1,I8,2I5,I4, 5F10.3)') lpet, ' ', indexlist(n), i, j, iblock, fptr(klon, n), &
    !        fptr(klat , n), fptr(karea, n), fptr(kfrac, n), fptr(kmask, n)
    !      call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO, rc=rc)
    !      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
    !   enddo
    !   enddo
    !enddo
    !print *, 'lpet: ', lpet, ' indexlist: ', indexlist
    pi = 3.14159265358
    radian = 180.0_ESMF_KIND_R8/pi
    radius    = 6370.0e5_ESMF_KIND_R8
    do n = elb1(1,1), eub1(1,1)
      fptr(klon , n)          = TLON(n)
      fptr(klat , n)          = TLAT(n)
      fptr(karea, n)          = TAREA(n)/radius/radius
      frac                    = TMASK(n)
      if (frac > 1.0_ESMF_KIND_R8) frac = 1.0_ESMF_KIND_R8
      fptr(kfrac, n)          = frac
      fptr(kmask, n)          = frac
      write(msg, '(I4,A1,I8,A7,I8,2F10.3,E15.7,2F10.3)') lpet, ' ', n, ' INDEX=',indexlist(n), fptr(klon, n), &
        fptr(klat , n), fptr(karea, n), fptr(kfrac, n), fptr(kmask, n)
      call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
    enddo

    deallocate(indexlist)

    write(msg, *) 'DUMPING HYCOM INDICES ENDS:'
    call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

!-----------------------------------------------------------------------
!EOC

  end subroutine ocn_domain_esmf
#endif

  ! compute regridding weights for all regridding pairs
  subroutine HYCOM_CESM_REGRID_TOOCN(srcGridFile, dstGrid, weightFile, regridMethod, rc)
    character(len=*), intent(in)             :: srcGridFile
    type(ESMF_Grid), intent(in)              :: dstGrid
    character(len=*), intent(in)             :: weightFile
    type(ESMF_REGRIDMETHOD_FLAG), intent(in) :: regridMethod
    integer, intent(out)                     :: rc

    ! local
    character(len=125)                       :: path="/glade/p/work/feiliu/weights/T62/"

    type(ESMF_Grid)                          :: srcGrid
    type(ESMF_Field)                         :: srcField, dstField
    type(ESMF_Field)                         :: srcFracField, dstFracField
    integer, pointer                         :: factorIndexList(:,:)
    real(ESMF_KIND_R8), pointer              :: factorList(:)

    rc = ESMF_SUCCESS

    srcGrid = ESMF_GridCreate(srcGridFile, ESMF_FILEFORMAT_SCRIP, (/5,8/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    srcField = ESMF_FieldCreate(srcGrid, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    dstField = ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    srcFracField = ESMF_FieldCreate(srcGrid, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    dstFracField = ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_FieldRegridStore(srcField, dstField, regridmethod=regridMethod, &
      !srcFracField=srcFracField, dstFracField=dstFracField, &
      unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
      factorList=factorList, factorIndexList=factorIndexList, rc=rc) 
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_OutputScripWeightFile(trim(path)//trim(weightFile), factorList, factorIndexList, &
      method=ESMF_REGRIDMETHOD_BILINEAR, &
      srcFile=srcGridFile, dstFile="/glade/p/cesm/cseg/mapping/grids/gx1v6_090205.nc", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

  ! compute regridding weights for all regridding pairs
  subroutine HYCOM_CESM_REGRID_FROMOCN(srcGrid, dstGridFile, weightFile, regridMethod, rc)
    type(ESMF_Grid), intent(in)              :: srcGrid
    character(len=*), intent(in)             :: dstGridFile
    character(len=*), intent(in)             :: weightFile
    type(ESMF_REGRIDMETHOD_FLAG), intent(in) :: regridMethod
    integer, intent(out)                     :: rc

    ! local
    character(len=125)                       :: path="/glade/p/work/feiliu/weights/T62/"

    type(ESMF_Grid)                          :: dstGrid
    type(ESMF_Field)                         :: srcField, dstField
    type(ESMF_Field)                         :: srcFracField, dstFracField
    integer, pointer                         :: factorIndexList(:,:)
    real(ESMF_KIND_R8), pointer              :: factorList(:)

    rc = ESMF_SUCCESS

    dstGrid = ESMF_GridCreate(dstGridFile, ESMF_FILEFORMAT_SCRIP, (/5,8/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    srcField = ESMF_FieldCreate(srcGrid, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    dstField = ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    srcFracField = ESMF_FieldCreate(srcGrid, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    dstFracField = ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_FieldRegridStore(srcField, dstField, regridmethod=regridMethod, &
      ! enable for conservative regridding
      !srcFracField=srcFracField, dstFracField=dstFracField, &
      unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
      factorList=factorList, factorIndexList=factorIndexList, rc=rc) 
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_OutputScripWeightFile(trim(path)//trim(weightFile), factorList, factorIndexList, &
      method=ESMF_REGRIDMETHOD_BILINEAR, &
      srcFile="/glade/p/cesm/cseg/mapping/grids/gx1v6_090205.nc", dstFile=dstGridFile, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine ModelAdvance(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_State)            :: importState, exportState
    type(ESMF_Time)             :: currTime
    type(ESMF_TimeInterval)     :: timeStep
    type(ESMF_Time)             :: hycomRefTime
    type(ESMF_TimeInterval)     :: interval
    real(ESMF_KIND_R8)          :: stepTime_r8
    type(InternalState)         :: is
    logical                     :: initFlag
    type(ESMF_CALKIND_FLAG)     :: calkind

    rc = ESMF_SUCCESS
    
    ! query the Component for its clock, importState and exportState
    call ESMF_GridCompGet(gcomp, clock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Get the internal state from Component.
    nullify(is%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! HERE THE MODEL ADVANCES: currTime -> currTime + timeStep
    ! on the internal Clock object. The NUOPC Layer will update the Clock 
    ! automatically each time before entering ModelAdvance(), but the HYCOM
    ! model must be stepped forward within this method.
    
    call NUOPC_ClockPrintCurrTime(clock, &
      "------>Advancing hycom from: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call ESMF_ClockGet(clock, currTime=currTime, timeStep=timeStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call NUOPC_TimePrint(currTime + timeStep, &
      "--------------------------------> to: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
#ifdef HYCOM_IN_CESM
    !call RedistAndWriteField(is%wrap%glue%grid, importState, filePrefix="field_ocn_import_", &
    !  timeslice=is%wrap%slice, relaxedFlag=.true., overwrite=.true., rc=rc)
    !if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    !  line=__LINE__, &
    !  file=__FILE__)) &
    !  return  ! bail out
#else
    ! write out the Fields in the importState
    call NUOPC_StateWrite(importState, filePrefix="field_ocn_import_", &
      timeslice=is%wrap%slice, relaxedFlag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif
    
    !TODO: don't need the additional initialization step once data-dependency
    !TODO: is taken care of during initialize.
    initFlag = .false.
    if (is%wrap%slice==1) initFlag = .true.

    ! Import data to HYCOM native structures through glue fields.
    call HYCOM_GlueFieldsDataImport(is%wrap%glue, initFlag, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! Translate currTime + timeStep into HYCOM format
    call ESMF_TimeSet(hycomRefTime, yy=1900, mm=12, dd=31, calkindflag=ESMF_CALKIND_GREGORIAN, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    interval = (currTime + timeStep) - hycomRefTime
    call ESMF_TimeIntervalGet(interval, d_r8=stepTime_r8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Enter the advancing loop over HYCOM_run...
    do
      ! ...on return the end-of-run flags indicate whether HYCOM has advanced
      ! far enough...
      call HYCOM_Run(endtime=stepTime_r8) ! -->> call into HYCOM <<--
      print *, "HYCOM_Run returned with end_of_run, end_of_run_cpl:", &
        end_of_run, end_of_run_cpl
      if (end_of_run .or. end_of_run_cpl) exit
    enddo

    ! Export HYCOM native data through the glue fields.
    call HYCOM_GlueFieldsDataExport(is%wrap%glue, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
#ifdef HYCOM_IN_CESM
    !call RedistAndWriteField(is%wrap%glue%grid, exportState, filePrefix="field_ocn_export_", &
    !  timeslice=is%wrap%slice, relaxedFlag=.true., rc=rc)
    !if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    !  line=__LINE__, &
    !  file=__FILE__)) &
    !  return  ! bail out
#else
    ! write out the Fields in the exportState
    call NUOPC_StateWrite(exportState, filePrefix="field_ocn_export_", &
      !timeslice=is%wrap%slice, relaxedFlag=.true., rc=rc)
      timeslice=is%wrap%slice, relaxedFlag=.true., overwrite=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif
    
    ! advance the time slice counter
    is%wrap%slice = is%wrap%slice + 1

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine Finalize(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    type(InternalState)  :: is
    integer              :: stat

    rc = ESMF_SUCCESS
  
    ! Get the internal state from Component.
    nullify(is%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! TODO: Destroy objects inside of internal state.

    ! Deallocate the internal state memory.
    deallocate(is%wrap, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of internal state memory failed.", &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
  end subroutine

  !-----------------------------------------------------------------------------
#ifdef HYCOM_IN_CESM
  subroutine routine_Run2(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine routine_Run3(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp

    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS

  end subroutine
  !-----------------------------------------------------------------------------

  subroutine ocn_forcing(state, d2x, filename, rc)
    use shr_string_mod

    type(ESMF_State)                :: state
    type(ESMF_Array)                :: d2x
    character(len=*), intent(in)    :: filename
    integer, intent(out)            :: rc

    real(ESMF_KIND_R8), allocatable :: rawdata(:,:)
    integer                         :: gsize, nfields, elb(2,1), eub(2,1), lpet, rec_len
    type(ESMF_VM)                   :: vm

    rc = ESMF_SUCCESS

    call ESMF_AttributeGet(state, name="gsize", value=gsize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
    return ! bail out

    call ESMF_ArrayGet(d2x, exclusiveLBound=elb, exclusiveUBound=eub, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
    return ! bail out

    nfields = shr_string_listGetNum(trim(seq_flds_o2x_fields))
    print *, 'ocn_forcing nfields: ', nfields, ' gsize: ', gsize

    call ESMF_VMGetCurrent(vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
    return ! bail out

    call ESMF_VMGet(vm, localPet=lpet, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
    return ! bail out

    if(lpet == 0) then
      allocate(rawdata(nfields, gsize))
      inquire (IOLENGTH=rec_len) rawdata
      open(1901,file=filename,status = 'unknown', form='unformatted', access='direct',recl=rec_len)
      read(1901,rec=1) rawdata
      close(1901)
    endif

    call ESMF_ArrayScatter(d2x, rawdata, 0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
    return ! bail out

    if(lpet == 0) deallocate(rawdata)

  end subroutine

  subroutine RedistAndWriteField(grid, state, fieldNameList, filePrefix, overwrite, &
    status, timeslice, relaxedflag, rc)
    type(ESMF_Grid),            intent(in)            :: grid
    type(ESMF_State),           intent(in)            :: state
    character(len=*),           intent(in),  optional :: fieldNameList(:)
    character(len=*),           intent(in),  optional :: filePrefix
    logical,                    intent(in),  optional :: overwrite
    type(ESMF_FileStatus_Flag), intent(in),  optional :: status
    integer,                    intent(in),  optional :: timeslice
    logical,                    intent(in),  optional :: relaxedflag
    integer,                    intent(out), optional :: rc
  !-----------------------------------------------------------------------------
    ! local variables
    integer                         :: i, itemCount
    type(ESMF_Field)                :: field
    type(ESMF_StateItem_Flag)       :: itemType
    character(len=80)               :: fileName
    character(len=80), allocatable  :: fieldNameList_loc(:)
    type(ESMF_Mesh)                 :: mesh
    type(ESMF_RouteHandle)          :: rh
    type(ESMF_Field)                :: src1DField, dst2DField

    if (present(rc)) rc = ESMF_SUCCESS

    mesh = ESMF_GridToMesh(grid, ESMF_STAGGERLOC_CENTER, 1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
    return ! bail out
    src1DField = ESMF_FieldCreate(mesh, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
    return ! bail out
    dst2DField = ESMF_FieldCreate(grid, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
    return ! bail out
    call ESMF_FieldRedistStore(src1DField, dst2DField, routehandle=rh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
    return ! bail out

    if (present(fieldNameList)) then
      allocate(fieldNameList_loc(size(fieldNameList)))
      do i=1, size(fieldNameList)
        fieldNameList_loc(i) = trim(fieldNameList(i))
      enddo
    else
      call ESMF_StateGet(state, itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      allocate(fieldNameList_loc(itemCount))
      call ESMF_StateGet(state, itemNameList=fieldNameList_loc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    do i=1, size(fieldNameList_loc)
      call ESMF_StateGet(state, itemName=fieldNameList_loc(i), &
        itemType=itemType, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
      if (itemType == ESMF_STATEITEM_FIELD) then
        ! field is available in the state
        call ESMF_StateGet(state, itemName=fieldNameList_loc(i), field=field, &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
        ! -> output to file
        if (present(filePrefix)) then
          write (fileName,"(A)") filePrefix//trim(fieldNameList_loc(i))//".nc"
        else
          write (fileName,"(A)") trim(fieldNameList_loc(i))//".nc"
        endif

        call ESMF_FieldRedist(field, dst2DField, routehandle=rh, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
        return ! bail out

        call ESMF_FieldWrite(dst2DField, file=trim(fileName), variableName=trim(fieldNameList_loc(i)), &
          overwrite=overwrite, status=status, timeslice=timeslice, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out

      endif
    enddo

    deallocate(fieldNameList_loc)
    call ESMF_RouteHandleRelease(rh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
    return ! bail out

    call ESMF_FieldDestroy(src1DField, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
    return ! bail out
    call ESMF_FieldDestroy(dst2DField, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
    return ! bail out

  end subroutine

    
#endif

end module
