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
    call NUOPC_StateAdvertiseFields(importState, &
      StandardNames=(/ &
      "surface_downward_eastward_stress       ",    & ! from ATM
      "surface_downward_northward_stress      ",    & ! from ATM
      "wind_speed_height10m                   ",    & ! from ATM
      "friction_speed                         ",    & ! from ATM
      "mean_down_sw_flx                       ",    & ! from ATM
      "mean_net_sw_flx                        ",    & ! from ATM
      "mean_net_lw_flx                        ",    & ! from ATM
      "inst_temp_height2m                     ",    & ! from ATM
      "mean_prec_rate                         ",    & ! from ATM
      "inst_spec_humid_height2m               ",    & ! from ATM
      "sea_surface_temperature                ",    & ! from ATM
      "sea_ice_area_fraction                  ",    & ! from SEA-ICE
      "downward_x_stress_at_sea_ice_base      ",    & ! from SEA-ICE
      "downward_y_stress_at_sea_ice_base      ",    & ! from SEA-ICE
      "downward_sea_ice_basal_solar_heat_flux ",    & ! from SEA-ICE
      "upward_sea_ice_basal_heat_flux         ",    & ! from SEA-ICE
      "downward_sea_ice_basal_salt_flux       ",    & ! from SEA-ICE
      "downward_sea_ice_basal_water_flux      ",    & ! from SEA-ICE
      "sea_ice_temperature                    ",    & ! from SEA-ICE
      "sea_ice_thickness                      ",    & ! from SEA-ICE
      "sea_ice_x_velocity                     ",    & ! from SEA-ICE
      "sea_ice_y_velocity                     "/),  & ! from SEA-ICE
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! exportable fields:
    call NUOPC_StateAdvertiseFields(exportState, &
      StandardNames=(/ &
      "sea_surface_temperature                  ",    &
      "upward_sea_ice_basal_available_heat_flux ",    &
      "sea_lev                                  ",    &
      "mixed_layer_depth                        ",    &
      "s_surf                                   ",    &
      "ocn_current_zonal                        ",    &
      "ocn_current_merid                        "/),  &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! set Component name so it becomes identifiable
    call ESMF_GridCompSet(gcomp, name="HYCOM", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
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
    call ESMF_TimeSet(hycomRefTime, yy=1900, mm=12, dd=31, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ClockGet(clock, startTime=startTime, stopTime=stopTime, rc=rc)
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
      
    !print *, " -->> startTime_r8=", startTime_r8, "stopTime_r8=", stopTime_r8
    
    ! Call into the HYCOM initialization  
    call HYCOM_Init(mpiComm, & ! -->> call into HYCOM <<--
!      hycom_start_dtg=-0.d0, hycom_end_dtg=stopTime_r8)
      hycom_start_dtg=-startTime_r8, hycom_end_dtg=stopTime_r8)

    
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

#if 1
    ! dump the HYCOM Grid coordinate and mask arrays for reference      
    call ESMF_GridGetCoord(gridIn, coordDim=1, array=array, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ArrayWrite(array, file="array_hycom_grid_coord1.nc", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridGetCoord(gridIn, coordDim=2, array=array, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ArrayWrite(array, file="array_hycom_grid_coord2.nc", rc=rc)
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
    call ESMF_ArrayWrite(array, file="array_hycom_grid_mask.nc", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
#endif
    
    ! conditionally realize or remove Fields in import and export States
    ! also keep track of these Fields on the glue layer
    
    ! importable fields:
    call HYCOM_GlueFieldsRealize(is%wrap%glue, importState, &
      StandardNames=(/ &
      "surface_downward_eastward_stress       ",    & ! from ATM
      "surface_downward_northward_stress      ",    & ! from ATM
      "wind_speed_height10m                   ",    & ! from ATM
      "friction_speed                         ",    & ! from ATM
      "mean_down_sw_flx                       ",    & ! from ATM
      "mean_net_sw_flx                        ",    & ! from ATM
      "mean_net_lw_flx                        ",    & ! from ATM
      "inst_temp_height2m                     ",    & ! from ATM
      "mean_prec_rate                         ",    & ! from ATM
      "inst_spec_humid_height2m               ",    & ! from ATM
      "sea_surface_temperature                ",    & ! from ATM
      "sea_ice_area_fraction                  ",    & ! from SEA-ICE
      "downward_x_stress_at_sea_ice_base      ",    & ! from SEA-ICE
      "downward_y_stress_at_sea_ice_base      ",    & ! from SEA-ICE
      "downward_sea_ice_basal_solar_heat_flux ",    & ! from SEA-ICE
      "upward_sea_ice_basal_heat_flux         ",    & ! from SEA-ICE
      "downward_sea_ice_basal_salt_flux       ",    & ! from SEA-ICE
      "downward_sea_ice_basal_water_flux      ",    & ! from SEA-ICE
      "sea_ice_temperature                    ",    & ! from SEA-ICE
      "sea_ice_thickness                      ",    & ! from SEA-ICE
      "sea_ice_x_velocity                     ",    & ! from SEA-ICE
      "sea_ice_y_velocity                     "/),  & ! from SEA-ICE
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable fields:
    call HYCOM_GlueFieldsRealize(is%wrap%glue, exportState, &
      StandardNames=(/ &
      "sea_surface_temperature                  ",    &
      "upward_sea_ice_basal_available_heat_flux ",    &
      "sea_lev                                  ",    &
      "mixed_layer_depth                        ",    &
      "s_surf                                   ",    &
      "ocn_current_zonal                        ",    &
      "ocn_current_merid                        "/),  &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

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
    
    ! write out the Fields in the importState
    call NUOPC_StateWrite(importState, filePrefix="field_ocn_import_", &
      timeslice=is%wrap%slice, relaxedFlag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
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
    call ESMF_TimeSet(hycomRefTime, yy=1900, mm=12, dd=31, rc=rc)
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
    
    ! write out the Fields in the exportState
    call NUOPC_StateWrite(exportState, filePrefix="field_ocn_export_", &
      timeslice=is%wrap%slice, relaxedFlag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
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
#endif

end module
