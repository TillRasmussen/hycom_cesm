      module mod_hycom

      use mod_xc      ! HYCOM communication interface
      use mod_za      ! HYCOM I/O interface
      use mod_pipe    ! HYCOM debugging interface
      use mod_incupd  ! HYCOM incremental update (for data assimilation)
      use mod_floats  ! HYCOM synthetic floats, drifters and moorings
      use mod_tides   ! HYCOM tides
      use mod_archiv  ! HYCOM archives
      use mod_mean    ! HYCOM mean archives
#if defined(USE_CCSM3)
      use ccsm3, only : ccsm3_setup_coupling_env ! ccsm3 ocean
      use ccsm3_exit                             ! ccsm3 ocean exit
      use ccsm3_io                               ! ccsm3 ocean io
      use ccsm3_forcing                          ! ccsm3 ocean/cpl6 comms
      use ccsm3_time_management                  ! ccsm3 ocean time
      use ccsm3_global_reductions                ! ccsm3 ocean mpi comms
      use shr_timer_mod                          ! ccsm3 shared timing routine
#endif

c
c --- -----------------------------------------
c --- MICOM-based HYbrid Coordinate Ocean Model
c ---               H Y C O M
c ---           v e r s i o n  2.2
c --- -----------------------------------------
c
      implicit none
c
      include "mpif.h"

#if ! defined(USE_CCSM3)
      include 'common_blocks.h'
#endif
c
      public HYCOM_Init, HYCOM_Run, HYCOM_Final
c
      logical, save, public  :: put_export   !set in main program
      logical, save, public  :: get_import   !set in main program
      logical, save, public  :: end_of_run   !set in HYCOM_Run
      integer, save, public  :: nts_day      !set in HYCOM_init, timesteps/day
      integer, save, public  :: nts_ice      !set in HYCOM_init, timesteps/ice
c
      logical, save, public  :: end_of_run_cpl   !set in HYCOM_Run for coupling
c
      integer, save, private ::
     &         m,n
#if defined(USE_CCSM3)
      logical, save, private ::
     &         end_month
      integer, save, private ::
     &         k1m,         !k1n in ccsm3_forcing
     &         mm,nn,
     &         termination, !if 1, stop now!
     &         ix0,ix1,ix2
      character, save, private ::
     &         ccsm3_string*80
#endif
      real*8,  save, private ::
     &        d1,d2,d3,d4,d2a,d3a,d4a,
     &        ddsurf,ddiagf,dproff,dtilef,drstrf,dmeanf,
     &        dske,dskea,dsmr,dsmra,dsms,dsmsa,dsmt,dsmta,dsum,dsuma,
     &        dsumtr(mxtrcr),
     &        dtime,dtime0,dbimon,dmonth,dyear,dyear0,
     &        dsmall,dsmall2
      real,    save, private ::
     &        smr,sms,smt,sum,smin,smax, tsur,
     &        coord,day1,day2,x,x1,time0,timav,cold,utotp,vtotp
      real,    save, private, allocatable ::
     &        sminy(:),smaxy(:)
      integer, save, private ::
     &        nstep0,nod,
     &        lt,ma0,ma1,ma2,ma3,mc0,mc1,mc2,mc3,
     &           mk0,mk1,mk2,mk3,mr0,mr1,mr2,mr3,mnth,iofl,
#if defined(USE_CCSM3)
     &        jday
*     logical, save ::
*    &         linit
      logical, save, private ::
     &               diagsv,hisurf,histry,hiprof,hitile,histmn,
     &         restrt,diag_tide
#else
     &        jday,ihour,iyear
      logical, save, private ::
     &         linit,diagsv,hisurf,histry,hiprof,hitile,histmn,
     &         restrt,diag_tide
#endif
      character, save, private ::
     &         intvl*3,c_ydh*14
c
      real*8     hours1,days1,days6
      private    hours1,days1,days6
      parameter (hours1=1.d0/24.d0,days1=1.d0,days6=6.d0)
c
c --- tfrz_n = nominal ice melting point (degC) for ice mask
      real       tfrz_n
      private    tfrz_n
      parameter (tfrz_n=-1.79)  !slightly above -1.8

c

      contains

      
c===================================================================
      subroutine HYCOM_Init(mpiCommunicator,hycom_start_dtg,
     &   hycom_end_dtg)


c
c --- Initialize (before the 1st time step).
c

      integer, optional   ::   mpiCommunicator
      real, optional      ::   hycom_start_dtg,hycom_end_dtg

      integer      mpi_comm_ocean,istat


      integer      i,j,k,l,nm
      character*80 flnm,flnmra,flnmrb

!!Alex add variable for calculation of dhdx and dhdy
      real            :: ssh_n,ssh_s,ssh_e,ssh_w
c
      include 'stmt_fns.h'

#if defined(USE_CCSM3)
!--------------------------------------------------------
!     intialize memory-statistics code
!--------------------------------------------------------
!     call start()
!--------------------------------------------------------
!     intialize message-passing with ccsm3 coupler cpl6
!--------------------------------------------------------
      call ccsm3_setup_coupling_env()
!--------------------------------------------------------
!     intialize i/o for ccsm3 coupled system
!--------------------------------------------------------
      call ccsm3_io_init
c
c --- initialize SPMD processsing
cjc/gjt      call xcspmd
      if (present(mpiCommunicator)) then
        call MPI_Comm_Dup(mpiCommunicator,mpi_comm_ocean,istat)
      else
        mpi_comm_ocean = MPI_COMM_WORLD
      endif
      call xcspmd(mpi_comm_ocean)

cjc/gjt      call xcspmd(mpiCommunicator)
#else
c
c --- initialize SPMD processsing
cjc/gjt      call xcspmd
      if (present(mpiCommunicator)) then
        call MPI_Comm_Dup(mpiCommunicator,mpi_comm_ocean,istat)
      else
        mpi_comm_ocean = MPI_COMM_WORLD
      endif
      call xcspmd(mpi_comm_ocean)

cjc/gjt      call xcspmd(mpiCommunicator)
#endif /* USE_ESMF:USE_CCSM3:else */
c
c --- initialize timer names.
c
      call xctmrn(40,'cnuity')
      call xctmrn(41,'tsadvc')
      call xctmrn(42,'momtum')
      call xctmrn(43,'barotp')
      call xctmrn(44,'thermf')
      call xctmrn(45,'ic****')
      call xctmrn(46,'mx****')
      call xctmrn(47,'conv**')
      call xctmrn(48,'diapf*')
      call xctmrn(49,'hybgen')
      call xctmrn(50,'trcupd')
      call xctmrn(51,'restrt')
      call xctmrn(52,'overtn')
      call xctmrn(53,'archiv')
      call xctmrn(54,'incupd')
c
c --- machine-specific initialization
      call machine
c
c --- initialize array i/o.
      call zaiost
c
c --- initiate named-pipe comparison utility
      call pipe_init
c
c --- initialize common variables
c
      call blkdat
c
      nts_ice = icefrq                     !no. time steps between ice coupling
      nts_day = nint(86400.0d0/baclin)     !no. time steps per day
      dsmall  = baclin/86400.0d0 * 0.25d0  !1/4 of a time step in days
      dsmall2 = dsmall*2.0d0
      if     (dsurfq.ge.1.0) then
        ddsurf = dsurfq
      elseif (dsurfq.ne.0.0) then
        ddsurf = (baclin/86400.0d0)*
     &           max(1,nint((86400.0d0*dsurfq)/baclin))
      else !no surface archives
        ddsurf = (baclin/86400.0d0)*0.99*huge(i)
      endif
      if     (diagfq.ge.1.0) then
        ddiagf = diagfq
      elseif (diagfq.ne.0.0) then
        ddiagf = (baclin/86400.0d0)*
     &           max(1,nint((86400.0d0*diagfq)/baclin))
      else !no 3-d archives
        ddiagf = (baclin/86400.0d0)*0.99*huge(i)
      endif
      if     (proffq.ge.1.0) then
        dproff = proffq
      elseif (proffq.ne.0.0) then
        dproff = (baclin/86400.0d0)*
     &           max(1,nint((86400.0d0*proffq)/baclin))
      else !no 3-d profiles at selection locations
        dproff = (baclin/86400.0d0)*0.99*huge(i)
      endif
      if     (tilefq.ge.1.0) then
        dtilef = tilefq
      elseif (tilefq.ne.0.0) then
        dtilef = (baclin/86400.0d0)*
     &           max(1,nint((86400.0d0*tilefq)/baclin))
      else !no tiled 3-d archives
        dtilef = (baclin/86400.0d0)*0.99*huge(i)
      endif
#if defined(USE_CCSM3)
      if     (meanfq.eq.0.0) then
        meanfq = 1.0  !always write out mean archives (value is ignored)
      endif
#endif
      if     (meanfq.ge.1.0) then
        dmeanf = meanfq
      elseif (meanfq.ne.0.0) then
        dmeanf = (baclin/86400.0d0)*
     &           max(1,nint((86400.0d0*meanfq)/baclin))
      else !no mean archives
        dmeanf = (baclin/86400.0d0)*0.99*huge(i)
      endif
      if     (rstrfq.eq.0.0) then  ! no restart
        drstrf = rstrfq
      elseif (rstrfq.lt.0.0) then  ! no restart at end of run
        drstrf = -rstrfq
      elseif (rstrfq.ge.1.0) then
        drstrf = rstrfq
      else
        drstrf = (baclin/86400.0d0)*
     &           max(1,nint((86400.0d0*rstrfq)/baclin))
      endif
      if (mnproc.eq.1) then
      write(lp,*)
      write(lp,*) 'ddsurf = ',ddsurf,nint((86400.0d0*ddsurf)/baclin)
      write(lp,*) 'ddiagf = ',ddiagf,nint((86400.0d0*ddiagf)/baclin)
      write(lp,*) 'dproff = ',dproff,nint((86400.0d0*dproff)/baclin)
      write(lp,*) 'dtilef = ',dtilef,nint((86400.0d0*dtilef)/baclin)
      write(lp,*) 'dmeanf = ',dmeanf,nint((86400.0d0*dmeanf)/baclin)
      write(lp,*) 'drstrf = ',drstrf,nint((86400.0d0*drstrf)/baclin)
      write(lp,*)
      write (lp,101) thkdf2,temdf2,
     &               thkdf4,
     &               veldf2,visco2,
     &               veldf4,visco4,
     &               diapyc,vertmx
 101  format (
     &  ' turb. flux parameters:',1p/
     &  ' thkdf2,temdf2 =',2e9.2/
     &  ' thkdf4        =', e9.2/
     &  ' veldf2,visco2 =',2e9.2/
     &  ' veldf4,visco4 =',2e9.2/
     &  ' diapyc,vertmx =',2e9.2/)
      endif !1st tile

c
c --- days in year.
c
      if     (yrflag.eq.0) then
c ---   360 days, starting Jan 16
        dmonth =  30.0d0
        dbimon =  60.0d0
        dyear  = 360.0d0
        dyear0 =   0.0d0
      elseif (yrflag.eq.1) then
c ---   366 days, starting Jan 16
        dmonth =  30.5d0
        dbimon =  61.0d0
        dyear  = 366.0d0
        dyear0 =   0.0d0
      elseif (yrflag.eq.2) then
c ---   366 days, starting Jan 1
c ---   also implies high frequency atmospheric forcing
        dmonth =  30.5d0
        dbimon =  61.0d0
        dyear  = 366.0d0
        dyear0 = -15.0d0+dyear
      elseif (yrflag.eq.3) then
c ---   model day is calendar days since 01/01/1901
c ---   also implies high frequency atmospheric forcing
        dyear  = 365.25d0
        dmonth = dyear/12.d0
        dbimon = dyear/ 6.d0
        dyear0 = -15.0d0+dyear
      else
        if (mnproc.eq.1) then
        write(lp,*)
        write(lp,*) 'error in hycom - unsupported yrflag value'
        write(lp,*)
        call flush(lp)
        endif !1st tile
        call xcstop('(hycom)')
               stop '(hycom)'   !won't get here
      endif
c
c --- 'lstep' = number of barotropic time steps per baroclinic time step.
c --- lstep   m u s t   be even.
c
      lstep=nint(baclin/batrop)
      lstep=2*((lstep+1)/2)
      dlt=baclin/lstep
      if (mnproc.eq.1) then
      write (lp,'(i4,a/)')
     &  lstep,' barotropic steps per baroclinic time step'
      endif !1st tile
c
c --- number of baroclinic time steps per day...
      nsteps_per_day = nint(86400.0/baclin)
c
c --- set up parameters defining the geographic environment
c
      call geopar
      
c
c --- set up forcing functions
c
#if defined(USE_CCSM3)
!----------------------------------------------------------------------
!     calls to forfuna and forfunp have been removed, because
!     in the coupled version, forcing fields are received from the coupler
!     at the beginning of each coupled interval
!----------------------------------------------------------------------
      if     (jerlv0.eq.0) then
        call forfunk  !  annual/monthly kpar
      endif
#else
      if (yrflag.lt.2) then
        call forfuna  ! monthly atmospheric forcing
      endif
      if     (jerlv0.eq.0) then
        call forfunk  !  annual/monthly kpar
      endif
      call forfunp  !    annual/monthly rivers
#endif
      call forfunr  ! bimonthly/monthly climatology
      watcum=0.
      empcum=0.
c
c --- set minimum salinity for each isopycnic layer
      if     (isopyc) then
        do k=2,kk
          cold=-3.0
          salmin(k)=sofsig(sigma(k),cold)
        enddo
      endif
c
c --- layer specific volume is defined as (1-theta)*thref
c --- subtract constant 'thbase' from theta to reduce roundoff errors
c
      if     (vsigma) then
        call forfunv  ! spacially varying isopycnal target densities
      else
        do k=1,kk
          theta(:,:,k)=sigma(k)-thbase
        enddo
      endif
c
c --- minimum depth of isopycnmal layers (pressure units).
c
      if     (isotop.lt.0.0) then
        call forfunt    !spacially varying minimum depths
      else
        topiso(:,:)=onem*isotop  !constant minimum depth
      endif
c
c --- tidal drag roughness (m/s)
c
      if     (drgscl.ne.0.0) then
        call forfund    !tidal drag roughness
      else
        dragrh(:,:)=0.0
      endif
c
c --- veldf2, veldf4 and thkdf4 may be spacially varying
c
      call forfundf
#if defined(USE_CCSM3)
!--------------------------------------------------------------
!     initialize time-related variables
!--------------------------------------------------------------
      call ccsm3_time_init (time0, time, dtime0, dtime)
#else
c
c --- model is to be integrated from time step 'nstep1' to 'nstep2'
      if (.not.present(hycom_start_dtg) .or. 
     & .not.present(hycom_end_dtg)) then
        open( unit=uoff+99,file=trim(flnminp)//'limits')
        read(      uoff+99,*) day1,day2
        close(unit=uoff+99)
      endif
c --- non-positive day1 indicates a new initialization, or
c --- the start of a yrflag==3 case.

c      print *,"mod_hycom, day1,day2=",day1,day2
c
      if (present(hycom_start_dtg)) day1=hycom_start_dtg
      if (present(hycom_end_dtg)) day2=hycom_end_dtg

      linit =day1.le.0.0 
      day1  =abs(day1)
c
      dtime=day1
      nstep1=nint(dtime*(86400.0d0/baclin))
      dtime=nstep1/(86400.0d0/baclin)
      day1 =dtime
c
      dtime=day2
      nstep2=nint(dtime*(86400.0d0/baclin))
      dtime=nstep2/(86400.0d0/baclin)
      day2 =dtime
#endif /* USE_CCSM3:else */
c
      if     (mxlkpp) then
c ---   initialize kpp mixing
        call inikpp
      elseif (mxlmy) then
c ---   initialize m-y 2.5 mixing
        call inimy
      elseif (mxlgiss) then
c ---   initialize nasa giss mixing
        call inigiss
      endif
c
#if defined(USE_CCSM3)
      if (linit) then
        delt1 = baclin
        dhdx  = c0p
        dhdy  = c0p
        QICE  = c0p
        AQICE = c0p
        QFLUX = c0p
c
c ---   set up initial conditions
c
        mnth = imonth
        call inicon(mnth)
        trcrin = .false.
        call initrc(mnth)
c
c ---   setup parameters defining tidal body forces
c
        if     (tidflg.gt.0) then
          time_8=dtime0      !'baroclinic' time for body force tides
          call tides_set(0)
        endif
      else
c
c ---   start from restart file
c
        delt1=baclin+baclin
        if (mnproc == 1) then
        open(1,file=trim(pointer_filename),form='formatted',status='old')
        read(1,'(a)') flnmra
        read(1,'(a)') flnmrb
        read(1,*)time                        ! real,  in days
        read(1,*)dtime                       ! real8, in days
        read(1,*)nstep,iyear,imonth,iday     ! integer
     &,          elapsed_days
        read(1,*)eom, eoy                    ! logical
        close(1)
        write(lp,*) ' flnm  = ', flnmra
        write(lp,*) ' flnmh = ', flnmrb
        write(lp,*) ' pointer_filename = ', pointer_filename
        call flush (lp)
        write(lp,'(2a)')'(hycom) the restart info from: ',trim(pointer_filename)
        write(lp,'(2a)')'     restart file= ',trim(flnmra)
        write(lp,*)     '            time= ',time
        write(lp,*)     '           nstep= ',nstep
        write(lp,*)     '           iyear= ',iyear
        write(lp,*)     '          imonth= ',imonth
        write(lp,*)     '            iday= ',iday
        write(lp,*)     '    elapsed_days= ',elapsed_days
        write(lp,*)     '             eom= ', eom
        write(lp,*)     '             eoy= ', eoy
        call flush(lp)
        endif !mnproc==1
        !broadcast the above info to all processors:
        call broadcast_scalar(flnmra,       master_task)
        call broadcast_scalar(flnmrb,       master_task)
        call broadcast_scalar(time,         master_task)
        call broadcast_scalar(dtime,        master_task)
        call broadcast_scalar(nstep,        master_task)
        call broadcast_scalar(iyear,        master_task)
        call broadcast_scalar(imonth,       master_task)
        call broadcast_scalar(iday,         master_task)
        call broadcast_scalar(elapsed_days, master_task)
        call broadcast_scalar(eom,          master_task)
        call broadcast_scalar(eoy,          master_task)

!-----------------------------------------------------------------------
!     make any other needed adjustments after header file has been read
!     and report restart time in format recognized by ccsm3
!-----------------------------------------------------------------------
        call ccsm3_time_init1

        call restart_in(nstep,dtime, flnmra, flnmrb)

        if (mnproc == 1 ) then
            write(lp,*)'(hycom) restartfile reading successful!'
        endif
        !
         time0 = (iyear0-1)*days_in_year+days_in_prior_months(imonth0)
     &                                  +iday0+0.0001
        !the above makes time0 be the end of last integration (in days)!

        mnth = imonth
        call initrc(mnth)
c
c ---   setup parameters defining tidal body forces
c
        if     (tidflg.gt.0) then
          time_8=dtime0      !'baroclinic' time for body force tides
          call tides_set(0)
        endif
      endif !initial conditions
#else
      if (linit .and. yrflag.lt.3) then
c
c ---   set up initial conditions
c
        nstep0=nstep1
        dtime0=nstep0/(86400.0d0/baclin)
        time0=dtime0
        delt1=baclin
        if     (clmflg.eq.12) then
          mnth=   1.+nint(mod(dtime0+dyear0,dyear)/dmonth)
        elseif (clmflg.eq.6) then
          mnth=2*(1.+nint(mod(dtime0+dyear0,dyear)/dbimon))-1
        endif
        call inicon(mnth)
        trcrin = .false.
        call initrc(mnth)
c
c ---   setup parameters defining tidal body forces
c
        if     (tidflg.gt.0) then
          time_8=dtime0      !'baroclinic' time for body force tides
          call tides_set(0)
        endif
c
c ---   output to archive file
c
        m=mod(nstep0  ,2)+1
        n=mod(nstep0+1,2)+1
        nstep=nstep0
        time=dtime0
        call forday(dtime0,yrflag, iyear,jday,ihour)
c
!$OMP   PARALLEL DO PRIVATE(j,k,l,i)
!$OMP&           SCHEDULE(STATIC,jblk)
        do j=1,jj
          do l=1,isp(j)
            do i=max(1,ifp(j,l)),min(ii,ilp(j,l))
               tmix(i,j)=temp(i,j,1,n)
               smix(i,j)=saln(i,j,1,n)
              thmix(i,j)=th3d(i,j,1,n)
            enddo !i
          enddo !l
          if (isopyc .or. mxlkrt) then
            do l=1,isu(j)
              do i=max(1,ifu(j,l)),min(ii,ilu(j,l))
                umix(i,j)=u(i,j,1,n)
              enddo !i
            enddo !l
            do l=1,isv(j)
              do i=max(1,ifv(j,l)),min(ii,ilv(j,l))
                vmix(i,j)=v(i,j,1,n)
              enddo !i
            enddo !l
          endif !isopyc.or.mxlkrt
        enddo !j
!$OMP   END PARALLEL DO
c
        if (mnproc.eq.1) then
        write (intvl,'(i3.3)') 0
        endif !1st tile
        if     (rstrfq.ne.0.0) then  !don't write if benchmarking (no restart)
          call archiv(n, kk, iyear,jday,ihour, intvl)
        endif
c
      else
c
c ---   start from restart file
c
        flnmra = trim(flnmrsi)//'.a'
        flnmrb = trim(flnmrsi)//'.b'
        call restart_in(nstep0,dtime0, flnmra,flnmrb)
c
        if     (linit) then
c
c ---     start a new calendar-day (yrflag==3) case.
          if (mnproc.eq.1) then
          time0=dtime0
          write (lp,'(9x,a,f8.1,a,f8.1,a,i9)')
     &      'restart file for day',time0,
     &      ', is treated as day',  day1,'  step',nstep1
          endif !1st tile
          nstep0=nstep1
          dtime0=nstep0/(86400.0d0/baclin)
          time0=dtime0
          delt1=baclin+baclin
        else
          nstep0=nint(dtime0*(86400.0d0/baclin))
          dtime0=nstep0/(86400.0d0/baclin)
          time0=dtime0
          delt1=baclin+baclin
          if (mnproc.eq.1) then
          write (lp,'(a,f8.1,a,i9,a, a,f8.1,a,i9,a)')
     &      'restart on day',time0,' (step',nstep0,')',
     &      ', wanted day',   day1,' (step',nstep1,')'
          endif !1st tile
          if (nstep0.ne.nstep1) then
            if (mnproc.eq.1) then
            write(lp,'(/a/a,f8.1/a,f8.1/)')
     &        'error in hycom - wrong restart (or limits) file',
     &        'restart file day is ',time0,
     &        'limits start day is ',day1
            endif !1st tile
            call xcstop('(hycom)')
                   stop '(hycom)'   !won't get here
          endif !nstep0.ne.nstep1
        endif !linit:else
c
        if     (clmflg.eq.12) then
          mnth=   1.+nint(mod(dtime0+dyear0,dyear)/dmonth)
        elseif (clmflg.eq.6) then
          mnth=2*(1.+nint(mod(dtime0+dyear0,dyear)/dbimon))-1
        endif
        call initrc(mnth)
c
c ---   setup parameters defining tidal body forces
c
        if     (tidflg.gt.0) then
          time_8=dtime0      !'baroclinic' time for body force tides
          call tides_set(0)
        endif
c
        if     (trcout .and. .not.trcrin) then
c
c ---     new tracers, so output to archive file
c
          m=mod(nstep0  ,2)+1
          n=mod(nstep0+1,2)+1
          call momtum_hs(n,m)  !calculate srfhgt
          nstep=nstep0
          time=dtime0
          call forday(dtime0,yrflag, iyear,jday,ihour)
          if (mnproc.eq.1) then
          write (intvl,'(i3.3)') 0
          endif !1st tile
          call archiv(n, kk, iyear,jday,ihour, intvl)
        endif !archive output
      endif !initial conditions
#endif /* USE_CCSM3:else */
c
c --- set barotp.pot.vort. and layer thickness (incl.bottom pressure) at
c --- u,v points
c
      call dpthuv
c
      call xctilr(dp(    1-nbdy,1-nbdy,1,1),1,2*kk, nbdy,nbdy, halo_ps)
      call xctilr(dpmixl(1-nbdy,1-nbdy,  1),1,   2, nbdy,nbdy, halo_ps)
      call xctilr(thkk(  1-nbdy,1-nbdy,1  ),1,   2, nbdy,nbdy, halo_ps)
      call xctilr(psikk( 1-nbdy,1-nbdy,1  ),1,   2, nbdy,nbdy, halo_ps)
c
      margin = nbdy
c
      do nm=1,2
c
!$OMP   PARALLEL DO PRIVATE(j,k,l,i)
!$OMP&           SCHEDULE(STATIC,jblk)
        do j=1-margin,jj+margin
          do l=1,isp(j)
            if     (nm.eq.mod(nstep+1,2)+1) then
              do i=max(1-margin,ifp(j,l)),min(ii+margin,ilp(j,l))
                dpbl(i,j)=dpmixl(i,j,nm)
              enddo
            endif
            do i=max(1-margin,ifp(j,l)),min(ii+margin,ilp(j,l))
              p(i,j,1)=0.0
              do k=1,kk
                p(i,j,k+1)=p(i,j,k)+dp(i,j,k,nm)
              enddo
            enddo
          enddo
        enddo
!$OMP   END PARALLEL DO
c
        call dpudpv(dpu(1-nbdy,1-nbdy,1,nm),
     &              dpv(1-nbdy,1-nbdy,1,nm),
     &              p,depthu,depthv, max(0,margin-1))
c
        if (.false.) then
c
c ---     ISOPYC TO HYBRID RESTART ONLY
          nstep=nstep1
          n=nm
          m=mod(n,2)+1
          call hybgen(m,n)
          call xctilr(dp(1-nbdy,1-nbdy,1,n),1,kk, nbdy,nbdy, halo_ps)
          margin = nbdy
c
!$OMP     PARALLEL DO PRIVATE(j,k,l,i)
!$OMP&             SCHEDULE(STATIC,jblk)
          do j=1-margin,jj+margin
            do l=1,isp(j)
!DIR$         PREFERVECTOR
              do i=max(1-margin,ifp(j,l)),min(ii+margin,ilp(j,l))
                p(i,j,1)=0.0
                do k=1,kk
                  p(i,j,k+1)=p(i,j,k)+dp(i,j,k,n)
                enddo
              enddo
            enddo
          enddo
!$OMP     END PARALLEL DO
          call dpudpv(dpu(1-nbdy,1-nbdy,1,n),
     &                dpv(1-nbdy,1-nbdy,1,n),
     &                p,depthu,depthv, max(0,margin-1))
             call pipe_comparall(m,n, 'hybgen, step')
        endif  !isopyc to hybrid restart only
c
      enddo  !nm=1,2
c
c --- surface archive output flags initiialization
c
      call archiv_init
c
c --- multi-location profile initialization.
c
      if     (proffq.ne.0.0) then
        call archiv_prof_init
      endif
c
c --- mean archive initialization.
c
      if     (meanfq.ne.0.0) then
        call mean_allocate
#if defined(USE_CCSM3)
        nstep0=nstep
#endif
        m=mod(nstep0  ,2)+1
        n=mod(nstep0+1,2)+1
        nstep=nstep0
        time =dtime0
        call mean_zero(time)
        call momtum_hs(n,m)  !calculate srfhgt
        call mean_add(n, 0.5)
      endif
c
#if ! defined(USE_CCSM3)
      nod=14 
      nstep=nstep1
      if (mnproc.eq.1) then
      write (lp,'(/2(a,f8.1),2(a,i9),a/)') 'model starts at day',
     &   time0,', goes to day',time0+day2-day1,'   (steps',nstep1,
     &   ' --',nstep2,')'
      open (unit=nod,file='summary_out',status='unknown')
      write(nod,'(/2(a,f8.1),2(a,i9),a/)') 'model starts at day',
     &   time0,', goes to day',time0+day2-day1,'   (steps',nstep1,
     &   ' --',nstep2,')'
      endif !1st tile
#endif
c
      timav=time0
      m=mod(nstep  ,2)+1
      n=mod(nstep+1,2)+1
c
           call pipe_comparall(m,n, 'restrt, step')
c
      if (synflt) then
c ---   initialize synthetic floats/moorings
        call floats_init(m,n,time0)
        margin = nbdy
      endif
#if defined(USE_CCSM3)
!----------------------------------------------------------
!     in the coupled model, forcing is received from the
!     coupler; therefore code for reading stand-alone version
!     forcing fields has been removed
!----------------------------------------------------------

      nstep0=nstep
      m=mod(nstep  ,2)+1
      n=mod(nstep+1,2)+1

      k1m=m                !NOT k1m=1+mm
      k1n=n                !NOT k1n=1+nn


      !-------------------------------------------------------------
      ! initialize namelists, grid information, and initial message-
      ! passing for cpl6
      !-------------------------------------------------------------
      call ccsm3_init_coupled(flnmgrdd)

      if (mnproc == 1) then
        write(lp,*)'(hycom) ccsm3_init_coupled completed'
        call flush(lp)
      endif


      if (mnproc == 1) then
        write (*,*) '(hycom) model starts from day, nstep=', iday,nstep
      endif

      l0=1
      l1=2
      l2=3
      l3=4
      w0=0.0
      w1=1.0
      w2=0.0
      w3=0.0
c
      if (jerlv0.eq.0) then
c ---   read in kpar field for 4 consecutive months
        mk1=imonth
        mk0=mod(mk1+10,12)+1
        mk2=mod(mk1,   12)+1
        mk3=mod(mk2,   12)+1
        lk0=1
        lk1=2
        lk2=3
        lk3=4
        call rdkpar(mk0,lk0)
        call rdkpar(mk1,lk1)
        call rdkpar(mk2,lk2)
        call rdkpar(mk3,lk3)
      endif
c
      if     (clmflg.eq.12) then
c ---   read in relaxation climatology fields for 4 consecutive months
        mc1=imonth
        mc0=mod(mc1+10,12)+1
        mc2=mod(mc1,   12)+1
        mc3=mod(mc2,   12)+1
        lc0=1
        lc1=2
        lc2=3
        lc3=4
        call rdrlax(mc0,lc0)
        call rdrlax(mc1,lc1)
        call rdrlax(mc2,lc2)
        call rdrlax(mc3,lc3)
      elseif (clmflg.eq.6) then
c ---   read in relaxation fields for 4 consecutive bi-months
        mc1=imonth
        mc0=mod(mc1+4,6)+1
        mc2=mod(mc1,  6)+1
        mc3=mod(mc2,  6)+1
        lc0=1
        lc1=2
        lc2=3
        lc3=4
        call rdrlax(2*mc0-1,lc0)
        call rdrlax(2*mc1-1,lc1)
        call rdrlax(2*mc2-1,lc2)
        call rdrlax(2*mc3-1,lc3)
      else
        if (mnproc.eq.1) then
        write(lp,'(/ a /)') 'error in hycom - unsupported clmflg value'
        call flush(lp)
        endif !1st tile
        call xcstop('(hycom)')
               stop '(hycom)'   !won't get here
      endif

      nod = 14
      write(cyear,'(i4.4)') iyear
Cpg - Write out summary files for each month
      write(flnm,'(a,a,i4.4,a,i2.2)')
     &  trim(flnmsumd),'summary_out.',iyear,'-',imonth
      if (mnproc==1) then
        open(unit=nod,file=flnm,status='unknown')
        write(nod,'(3a,i2.2)')
     &    'Summary of diagnostic outputs for year ',cyear
     &  , '  month ', imonth
Cpg
        write(nod,*)
        call flush(nod)
      endif
#else
c
      if (yrflag.lt.2) then
c
c ---   read in forcing fields for 4 consecutive months
        ma1=1.+mod(dtime0+dyear0,dyear)/dmonth
        ma0=mod(ma1+10,12)+1
        ma2=mod(ma1,   12)+1
        ma3=mod(ma2,   12)+1
        l0=1
        l1=2
        l2=3
        l3=4
        call rdforf(ma0,l0)
        call rdforf(ma1,l1)
        call rdforf(ma2,l2)
        call rdforf(ma3,l3)
      else
c
c ---   initial day of high frequency atmospheric forcing.
c ---   only two fields are used (linear interpolation in time).
        l0=1
        l1=2
        l2=3
        l3=4
        if (windf) then
          w0=-99.9
          w1=-99.0
          w2=0.0
          w3=0.0
          call forfunh(dtime0)
        else
          w0=0.0
          w1=0.0
          w2=0.0
          w3=0.0
        endif
      endif
c
      if (jerlv0.eq.0) then
c ---   read in kpar field for 4 consecutive months
        mk1=1.+mod(dtime0+dyear0,dyear)/dmonth
        mk0=mod(mk1+10,12)+1
        mk2=mod(mk1,   12)+1
        mk3=mod(mk2,   12)+1
        lk0=1
        lk1=2
        lk2=3
        lk3=4
        call rdkpar(mk0,lk0)
        call rdkpar(mk1,lk1)
        call rdkpar(mk2,lk2)
        call rdkpar(mk3,lk3)
      endif
c
      if (priver) then
c ---   read in rivers field for 4 consecutive months
        mr1=1.+mod(dtime0+dyear0,dyear)/dmonth
        mr0=mod(mr1+10,12)+1
        mr2=mod(mr1,   12)+1
        mr3=mod(mr2,   12)+1
        lr0=1
        lr1=2
        lr2=3
        lr3=4
        call rdrivr(mr0,lr0)
        call rdrivr(mr1,lr1)
        call rdrivr(mr2,lr2)
        call rdrivr(mr3,lr3)
      endif
c
      if     (clmflg.eq.12) then
c ---   read in relaxation climatology fields for 4 consecutive months
        mc1=1.+mod(dtime0+dyear0,dyear)/dmonth
        mc0=mod(mc1+10,12)+1
        mc2=mod(mc1,   12)+1
        mc3=mod(mc2,   12)+1
        lc0=1
        lc1=2
        lc2=3
        lc3=4
        call rdrlax(mc0,lc0)
        call rdrlax(mc1,lc1)
        call rdrlax(mc2,lc2)
        call rdrlax(mc3,lc3)
      elseif (clmflg.eq.6) then
c ---   read in relaxation fields for 4 consecutive bi-months
        mc1=1.+mod(dtime0+dyear0,dyear)/dbimon
        mc0=mod(mc1+4,6)+1
        mc2=mod(mc1,  6)+1
        mc3=mod(mc2,  6)+1
        lc0=1
        lc1=2
        lc2=3
        lc3=4
        call rdrlax(2*mc0-1,lc0)
        call rdrlax(2*mc1-1,lc1)
        call rdrlax(2*mc2-1,lc2)
        call rdrlax(2*mc3-1,lc3)
      else
        if (mnproc.eq.1) then
        write(lp,'(/ a /)') 'error in hycom - unsupported clmflg value'
        call flush(lp)
        endif !1st tile
        call xcstop('(hycom)')
               stop '(hycom)'   !won't get here
      endif
c
      if     (bnstfq.ne.0.0) then  ! initialize barotropic boundary input
        wb0=-99.0
        wb1=-99.0
        call rdbaro(dtime0)
      endif
c
      if     (nestfq.ne.0.0) then  ! initialise 3-d nesting input
        wn0=-99.0
        wn1=-99.0
        call rdnest(dtime0)
      endif
#endif /* USE_CCSM3:else */
c
c --- initialize incremental update.
c
      if (incflg.ne.0) then
        call incupd_init(dtime0)
c
        if (incstp.eq.1) then
              call xctmr0(54)
           call incupd(1)
           call incupd(2)
              call xctmr1(54)
        endif ! full insertion of update
      endif

c
c --- Only here for compatibility with coupled runs.
c
      m=mod(nstep0  ,2)+1
      n=mod(nstep0+1,2)+1
      call momtum_hs(n,m)

!!Alex add calculation of seas surface slope for CICE (NUOPC)
      call xctilr(srfhgt( 1-nbdy,1-nbdy),1,   3, 6,6, halo_ps)

      do j=1,jj
        do i=1,ii
          if     (ip(i,j).eq.1) then
            if     (ip(i+1,j)  .eq.1) then
              ssh_e = srfhgt(i+1,j)
            else
              ssh_e = srfhgt(i,  j)
            endif
            if     (ip(i-1,j)  .eq.1) then
              ssh_w = srfhgt(i-1,j)
            else
              ssh_w = srfhgt(i,  j)
            endif
            if     (ip(i,  j+1).eq.1) then
              ssh_n = srfhgt(i,  j+1)
            else
              ssh_n = srfhgt(i,  j)
            endif
            if     (ip(i,  j-1).eq.1) then
              ssh_s = srfhgt(i,  j-1)
            else
              ssh_s = srfhgt(i,  j)
            endif
            dhdx(i,j)=(ssh_e-ssh_w)/(scux(i+1,j)+scux(i,j))
            dhdy(i,j)=(ssh_n-ssh_s)/(scvy(i,j+1)+scvy(i,j))
          else
            dhdx(i,j)=0.0
            dhdy(i,j)=0.0
          endif
        enddo !i
      enddo !j
c
c --- report initialization time.
c
      call xctmrp
c
      end subroutine HYCOM_Init

c============================================================
      subroutine HYCOM_Run(endtime)
c
c --- -------------------------
c --- execute a single timestep
c --- -------------------------
c
      real, optional :: endtime
c
      real         nstep2_cpl
      logical      lfatal
      integer      i,j,jja,k,ktr,l,nm
      character*80 flnmra,flnmrb

!!Alex add variable for calculation of dhdx and dhdy
      real            :: ssh_n,ssh_s,ssh_e,ssh_w
c
      logical hycom_isnaninf  !function to detect NaN and Inf
c
      include 'stmt_fns.h'
c
c --- letter 'm' refers to mid-time level (example: dp(i,j,k,m) )
c --- letter 'n' refers to old and new time level
c
c      print *,"HYCOM_Run, endtime=",endtime
c
      end_of_run      = .false.   ! initialize on every entry
      end_of_run_cpl  = .false.   ! initialize on every entry
C
      m=mod(nstep  ,2)+1
      n=mod(nstep+1,2)+1
c
#if defined(USE_CCSM3)
      k1m=m              !NOT k1m=1+mm (DBI)
      k1n=n              !NOT k1n=1+nn (DBI)

!-------------------------------------------------------------------------
!     exchange fields and fluxes with the ccsm3 flux coupler
!-------------------------------------------------------------------------
      call ccsm3_set_coupled_forcing

!--------------------------------------------------------
!     stop if cpl has sent the stop signal
!--------------------------------------------------------
      end_of_run = stop_now == 1
      if (end_of_run) then
c ---   output float restart file 
        if     (synflt) then
          call floats_restart
        endif !synflt
        return
      endif

!-------------------------------------------------------------------------
!     ccsm3-related time-keeping
!-------------------------------------------------------------------------
      call ccsm3_time_advance (time, dtime)
c
      hisurf=.false.
      histry=.false.
      hiprof=.false.
      hitile=.false.
      histmn=dohist
*     restrt=dorestart .or. (cpl_write_restart .and. eod)
      restrt=dorestart .and. mod(imonth-1, 3).eq.0  ! every 3 months
      diagno=mod(dtime+dsmall,ddiagf).lt.dsmall2 .or.  restrt
c
c --- set weights for quasi-hermite time interpolation for kpar.
      if     (jerlv0.eq.0) then
c ---   monthly fields.
        if (imonth.ne.mk1) then
          mk1=imonth
          mk0=mod(mk1+10,12)+1
          mk2=mod(mk1,   12)+1
          mk3=mod(mk2,   12)+1
          lt =lk0
          lk0=lk1
          lk1=lk2
          lk2=lk3
          lk3=lt
          call rdkpar(mk3,lk3)
        endif
        wk1=1.0  !constant for the month
        wk2=0.0
        wk0=0.0
        wk3=0.0
      endif
c
c --- set weights for quasi-hermite time interpolation for temperature,
c --- salinity and pressure relaxation fields.
      if     (clmflg.eq.12) then
c ---   monthly fields.
        if (imonth.ne.mc1) then
          mc1=imonth
          mc0=mod(mc1+10,12)+1
          mc2=mod(mc1,   12)+1
          mc3=mod(mc2,   12)+1
          lt =lc0
          lc0=lc1
          lc1=lc2
          lc2=lc3
          lc3=lt
          call rdrlax(mc3,lc3)
        endif
        wc1=1.0  !constant for the month
        wc2=0.0
        wc0=0.0
        wc3=0.0
      endif
cdiag if (mnproc.eq.1) then
cdiag write (lp,'(i9,'' relax time interpolation: months'',4i3,
cdiag.  '',  weights '',4f6.3)') nstep,lc0,lc1,lc2,lc3,wc0,wc1,wc2,wc3
cdiag endif !1st tile
#else
      nstep=nstep+1
      dtime=dtime0+(nstep-nstep0)/(86400.0d0/baclin)
c
      time  =dtime
      time_8=dtime  !'baroclinic' time for body force tides
      if     (tidflg.gt.0 .and.
     &        mod(dtime+dsmall,hours1).lt.dsmall2) then
        call tides_detide(n, .true.)  !update 25-hour average
      endif
      hisurf=mod(dtime+dsmall,ddsurf).lt.dsmall2
      histry=mod(dtime+dsmall,ddiagf).lt.dsmall2 .or.
     &        (nstep.ge.nstep2 .and. arcend)
      hiprof=mod(dtime+dsmall,dproff).lt.dsmall2
      hitile=mod(dtime+dsmall,dtilef).lt.dsmall2
      histmn=mod(dtime+dsmall,dmeanf).lt.dsmall2 .or.
     &         nstep.ge.nstep2
      if     (rstrfq.eq.0.0) then  ! no restart
        restrt=.false.  ! for benchmark cases only
      elseif (drstrf.gt.dtime0) then  !at most one restart within the run
        if (rstrfq.lt.0.0) then  ! no restart at end of run
          restrt=mod(dtime+dsmall,drstrf).lt.dsmall2
        else
          restrt=mod(dtime+dsmall,drstrf).lt.dsmall2 .or.
     &           nstep.ge.nstep2
        endif
      else
        if (rstrfq.lt.0.0) then  ! no restart at end of run
          restrt=mod(dtime-dtime0+dsmall,drstrf).lt.dsmall2
        else
          restrt=mod(dtime-dtime0+dsmall,drstrf).lt.dsmall2 .or.
     &           nstep.ge.nstep2
        endif
      endif
      diagno=mod(dtime+dsmall,ddiagf).lt.dsmall2 .or.
     &       restrt .or. nstep.ge.nstep2
      if (yrflag.lt.2) then
c
c ---   set weights for quasi-hermite time interpolation for
c ---   monthly atmospheric forcing fields
        x=1.+mod(dtime+dyear0,dyear)/dmonth
c ---   keep quadruplet of forcing functions centered on model time
        if (int(x).ne.ma1) then
          ma1=x
          ma0=mod(ma1+10,12)+1
          ma2=mod(ma1,   12)+1
          ma3=mod(ma2,   12)+1
          lt=l0
          l0=l1
          l1=l2
          l2=l3
          l3=lt
c ---     newest set of forcing functions overwrites set no longer needed
          call rdforf(ma3,l3)
        endif
        x=mod(x,1.)
        x1=1.-x
        w1=x1*(1.+x *(1.-1.5*x ))
        w2=x *(1.+x1*(1.-1.5*x1))
        w0=-.5*x *x1*x1
        w3=-.5*x1*x *x
cdiag   if (mnproc.eq.1) then
cdiag   write (lp,'(i9,'' atmos time interpolation: months'',4i3,
cdiag.    '',  weights '',4f6.3)') nstep,l0,l1,l2,l3,w0,w1,w2,w3
cdiag   endif !1st tile
      elseif (windf) then
c
c ---   set weights and fields for high frequency atmospheric forcing.
c ---   only two fields are used (linear interpolation in time).
        call forfunh(dtime)

c     !Debugging
      if(mnproc.eq.1) then
c        print *,"mod_hycom,time,nstep=",
c     &     time,nstep
c        print *,"taux=",taux(2,2,l0),taux(2,2,l1),
c     &     taux(2,2,l2),taux(2,2,l3)
c        print *," "
      endif

      endif
c
c --- set weights for quasi-hermite time interpolation for kpar.
      if     (jerlv0.eq.0) then
c ---   monthly fields.
        x=1.+mod(dtime+dyear0,dyear)/dmonth
        if (int(x).ne.mk1) then
          mk1=x
          mk0=mod(mk1+10,12)+1
          mk2=mod(mk1,   12)+1
          mk3=mod(mk2,   12)+1
          lt =lk0
          lk0=lk1
          lk1=lk2
          lk2=lk3
          lk3=lt
          call rdkpar(mk3,lk3)
        endif
        x=mod(x,1.)
        x1=1.-x
        wk1=x1*(1.+x *(1.-1.5*x ))
        wk2=x *(1.+x1*(1.-1.5*x1))
        wk0=-.5*x *x1*x1
        wk3=-.5*x1*x *x
      endif
c
c --- set weights for quasi-hermite time interpolation for rivers.
      if     (priver) then
c ---   monthly fields.
        x=1.+mod(dtime+dyear0,dyear)/dmonth
        if (int(x).ne.mr1) then
          mr1=x
          mr0=mod(mr1+10,12)+1
          mr2=mod(mr1,   12)+1
          mr3=mod(mr2,   12)+1
          lt =lr0
          lr0=lr1
          lr1=lr2
          lr2=lr3
          lr3=lt
          call rdrivr(mr3,lr3)
        endif
        x=mod(x,1.)
        x1=1.-x
        wr1=x1*(1.+x *(1.-1.5*x ))
        wr2=x *(1.+x1*(1.-1.5*x1))
        wr0=-.5*x *x1*x1
        wr3=-.5*x1*x *x
      endif
c
c --- set weights for quasi-hermite time interpolation for temperature,
c --- salinity and pressure relaxation fields.
      if     (clmflg.eq.12) then
c ---   monthly fields.
        x=1.+mod(dtime+dyear0,dyear)/dmonth
        if (int(x).ne.mc1) then
          mc1=x
          mc0=mod(mc1+10,12)+1
          mc2=mod(mc1,   12)+1
          mc3=mod(mc2,   12)+1
          lt =lc0
          lc0=lc1
          lc1=lc2
          lc2=lc3
          lc3=lt
          call rdrlax(mc3,lc3)
        endif
        x=mod(x,1.)
        x1=1.-x
        wc1=x1*(1.+x *(1.-1.5*x ))
        wc2=x *(1.+x1*(1.-1.5*x1))
        wc0=-.5*x *x1*x1
        wc3=-.5*x1*x *x
      elseif (clmflg.eq.6) then
c ---   bi-monthly fields.
        x=1.+mod(dtime+dyear0,dyear)/dbimon
        if (int(x).ne.mc1) then
          mc1=x
          mc0=mod(mc1+4,6)+1
          mc2=mod(mc1,  6)+1
          mc3=mod(mc2,  6)+1
          lt =lc0
          lc0=lc1
          lc1=lc2
          lc2=lc3
          lc3=lt
          call rdrlax(2*mc3-1,lc3)
        endif
        x=mod(x,1.)
        x1=1.-x
        wc1=x1*(1.+x *(1.-1.5*x ))
        wc2=x *(1.+x1*(1.-1.5*x1))
        wc0=-.5*x *x1*x1
        wc3=-.5*x1*x *x
      endif
cdiag if (mnproc.eq.1) then
cdiag write (lp,'(i9,'' relax time interpolation: months'',4i3,
cdiag.  '',  weights '',4f6.3)') nstep,lc0,lc1,lc2,lc3,wc0,wc1,wc2,wc3
cdiag endif !1st tile
#endif /* USE_CCSM3:else */
c
c
      if     (bnstfq.ne.0.0) then  ! new fields for baro nesting
        call rdbaro(dtime)
      endif
c
      if     (nestfq.ne.0.0) then  ! new fields for 3-d  nesting
        call rdnest(dtime)
      endif
c
         call pipe_comparall(m,n, 'ENTER , step')
         call xctmr0(40)
      call cnuity(m,n)
         call xctmr1(40)
         call pipe_comparall(m,n, 'cnuity, step')
         call xctmr0(41)
      call tsadvc(m,n)
         call xctmr1(41)
         call pipe_comparall(m,n, 'tsadvc, step')
         call xctmr0(42)
      if     (momtyp.eq.2) then
        call momtum(m,n)
      else   !momtyp.eq.4
        call momtum4(m,n)
      endif
         call xctmr1(42)
         call pipe_comparall(m,n, 'momtum, step')
         call xctmr0(43)
      call barotp(m,n)
         call xctmr1(43)
         call pipe_comparall(m,n, 'barotp, step')
         call xctmr0(44)
#if defined(USE_CCSM3)
      call thermf_c(m,n)   !,mm,nn,k1m,k1n)
         call xctmr1(44)
         call pipe_comparall(m,n, 'thermf, step')
      !get potential freezing/melting heat flux needed for ice model.
      !mixed layer T and S are adjusted only TWICE in each coupling interval.
      !
      call ice_formation(k1n, ice_ts)
      !===================================================================
      !if (.false. .and. ice_ts) then   !DBI: switched this call off!
      !  call ice_flx_to_coupler        ! work done in ice_formation now!
      !endif
#else
      call thermf(m,n, dtime)
         call xctmr1(44)
         call pipe_comparall(m,n, 'thermf, step')
      if (icegln) then
            call xctmr0(45)
         call icloan(m,n)
         call thermf_oi(m,n)
            call xctmr1(45)
            call pipe_comparall(m,n, 'icloan, step')
      elseif (iceflg.ge.2) then
            call xctmr0(45)
         call thermf_oi(m,n)
            call xctmr1(45)
            call pipe_comparall(m,n, 'icecpl, step')
      else
            call xctmr0(45)
         call thermf_oi(m,n)
            call xctmr1(45)
            call pipe_comparall(m,n, 'thermf, step') !thermf_oi
      endif !icegln:icecpl:else
#endif /* USE_CCSM3:else */
      if (trcout) then
           call xctmr0(50)
        call trcupd(m,n)
           call xctmr1(50)
           call pipe_comparall(m,n, 'trcupd, step')
      endif !trcout
      if (hybrid) then
         diagsv = diagno
         diagno = diagno .or. nstep.eq.nstep0+1 .or.
     &            histry .or. hisurf .or. hiprof .or. hitile .or.
     &            mod(dtime+dsmall,days6).lt.dsmall2
         if (mxlkpp .or. mxlmy .or. mxlgiss) then
               call xctmr0(46)
            call mxkprf(m,n)
               call xctmr1(46)
               call pipe_comparall(m,n, 'mxkprf, step')
         elseif (mxlpwp) then
               call xctmr0(46)
            call mxpwp(m,n)
               call xctmr1(46)
               call pipe_comparall(m,n, 'mxpwp,  step')
         elseif (mxlkta) then
               call xctmr0(46)
            call mxkrta(m,n)
               call xctmr1(46)
               call pipe_comparall(m,n, 'mxkrta, step')
         elseif (mxlktb) then
               call xctmr0(46)
            call mxkrtb(m,n)
               call xctmr1(46)
               call pipe_comparall(m,n, 'mxkrtb, step')
         else
               call xctmr0(46)
               call xctmr1(46)
         endif !mixed layer
         diagno = diagsv
         if (mxlkpp .or. mxlmy .or. mxlgiss) then  !tsoff in mxkprf
               call xctmr0(47)
               call xctmr1(47)
               call xctmr0(48)
               call xctmr1(48)
         else  ! mxlpwp has dypflg=2
               call xctmr0(47)
            call convch(m,n)
               call xctmr1(47)
               call pipe_comparall(m,n, 'convch, step')
           if     (dypflg.eq.1) then  ! KPP-like, tsoff in diapf1
                 call xctmr0(48)
              call diapf1(m,n)
                 call xctmr1(48)
                 call pipe_comparall(m,n, 'diapf1, step')
           elseif (dypflg.eq.2) then  ! explicit, tsoff in diapf2
                 call xctmr0(48)
              call diapf2(m,n)
                 call xctmr1(48)
                 call pipe_comparall(m,n, 'diapf2, step')
           else
                 call xctmr0(48)
                 call xctmr1(48)
           endif
        endif !diapycnal mixing
        if     (incflg.ne.0 .and. incstp.gt.1) then
             call xctmr0(54)
          call incupd(n)
             call xctmr1(54)
             call pipe_comparall(m,n, 'incupd, step')
        else
             call xctmr0(54)
             call xctmr1(54)
        endif ! incremental update
           call xctmr0(49)
        call hybgen(m,n)
           call xctmr1(49)
           call pipe_comparall(m,n, 'hybgen, step')
      else  ! isopyc
            call xctmr0(46)
         call mxkrtm(m,n)
            call xctmr1(46)
            call pipe_comparall(m,n, 'mxkrtm, step')
            call xctmr0(47)
         call convcm(m,n)
            call xctmr1(47)
            call pipe_comparall(m,n, 'convcm, step')
            call xctmr0(48)
         call diapf3(m,n)
            call xctmr1(48)
            call pipe_comparall(m,n, 'diapf3, step')
            call xctmr0(54)
            call xctmr1(54)
            call xctmr0(49)
            call xctmr1(49)
      endif !hybrid:isopyc

!!Alex add calculation of seas surface slope for CICE (NUOPC)
      call xctilr(srfhgt( 1-nbdy,1-nbdy),1,   3, 6,6, halo_ps)

      do j=1,jj
        do i=1,ii
          if     (ip(i,j).eq.1) then
            if     (ip(i+1,j)  .eq.1) then
              ssh_e = srfhgt(i+1,j)
            else
              ssh_e = srfhgt(i,  j)
            endif
            if     (ip(i-1,j)  .eq.1) then
              ssh_w = srfhgt(i-1,j)
            else
              ssh_w = srfhgt(i,  j)
            endif
            if     (ip(i,  j+1).eq.1) then
              ssh_n = srfhgt(i,  j+1)
            else
              ssh_n = srfhgt(i,  j)
            endif
            if     (ip(i,  j-1).eq.1) then
              ssh_s = srfhgt(i,  j-1)
            else
              ssh_s = srfhgt(i,  j)
            endif
            dhdx(i,j)=(ssh_e-ssh_w)/(scux(i+1,j)+scux(i,j))
            dhdy(i,j)=(ssh_n-ssh_s)/(scvy(i,j+1)+scvy(i,j))
          else
            dhdx(i,j)=0.0
            dhdy(i,j)=0.0
          endif
        enddo !i
      enddo !j
c
c --- update floats/moorings
      if     (synflt) then
        nstepfl=nstepfl+1
        if (nstepfl.eq.1) then
          iofl=1
          call floats(m,n,0.0,iofl)
        endif
        if (mod(nstepfl,nfldta).eq.0) then
          iofl=0
          if (mod(nstepfl,nflsam).eq.0) then
            iofl=1
          endif
          call floats(m,n,nstepfl*baclin/86400.0,iofl)
        endif
      endif !synflt
c
c ---------------------------------------------------------------------------
c
c --- output and diagnostic calculations
c
c ---------------------------------------------------------------------------
c
      lfatal    = .false.
      diag_tide = tidflg.gt.0 .and.
     &            mod(dtime+dsmall,hours1).lt.dsmall2  !at least hourly
      if (diagno .or.
     &    histry .or. hiprof .or. hitile .or. hisurf .or. histmn .or.
     &    nstep.eq.nstep0+1 .or.
     &    diag_tide .or.
     &    mod(dtime+dsmall,ddsurf).lt.dsmall2 .or.
     &    mod(dtime+dsmall, days1).lt.dsmall2     ) then     ! at least daily
c
#if defined(ARCTIC)
c ---   Arctic (tripole) domain, top row is replicated (ignore it)
        jja = min( jj, jtdm-1-j0 )
        if     (jja.ne.jj) then
          do i=1,ii
            util1(i,jj)=0.0
            util2(i,jj)=0.0
            util3(i,jj)=0.0
            util4(i,jj)=0.0
            util5(i,jj)=0.0
          enddo
        endif
#else
        jja = jj
#endif
c
#if defined(USE_CCSM3)
        jday = days_in_prior_months(imonth)+iday
#else
        call forday(dtime,yrflag, iyear,jday,ihour)
#endif
        write(c_ydh,'('' ('',i4.4,''/'',i3.3,1x,i2.2,'')'')')
     &    iyear,jday,ihour
c
c ---   diagnose mean sea surface height

        if     (.not.allocated(sminy)) then
          allocate( sminy(1:jj), smaxy(1:jj) )
        endif
!$OMP   PARALLEL DO PRIVATE(j,l,i)
!$OMP&           SCHEDULE(STATIC,jblk)
        do j=1,jja
          sminy(j)= huge(sminy(j))
          smaxy(j)=-huge(smaxy(j))
          do l=1,isp(j)
            do i=max(1,ifp(j,l)),min(ii,ilp(j,l))
              if     (tidflg.gt.0) then
                util2(i,j)=(srfhgt(i,j)/g)**2*scp2(i,j)
              endif
              util3(i,j)=srfhgt(i,j)*scp2(i,j)
              util4(i,j)=montg1(i,j)*scp2(i,j)
              sminy(j)=min(sminy(j),srfhgt(i,j))
              smaxy(j)=max(smaxy(j),srfhgt(i,j))
            enddo
          enddo
        enddo
!$OMP   END PARALLEL DO
        smin=minval(sminy(1:jja))
        smax=maxval(smaxy(1:jja))
        call xcminr(smin)
        call xcmaxr(smax)
        call xcsum( dsum, util3,ip)
        call xcsum( dsmt, util4,ip)
        sum=dsum
        smt=dsmt
        if (mnproc.eq.1) then
        write (lp,'(i9,a,
     &              '' mean      SSH (mm):'',f8.2,
     &              ''  ('',1pe8.1,'' to '',e8.1,'')'')')
     &  nstep,c_ydh,
     &  sum/(area*thref*onemm),smin/(thref*onemm),smax/(thref*onemm)
*       write (lp,'(i9,a,
*    .              '' mean MontgPot (mm):'',f8.2)')
*    .  nstep,c_ydh,
*    .  smt/(area*thref*onemm)
        call flush(lp)
        write(nod,'(i9,a,
     &              '' mean      SSH (mm):'',f8.2,
     &              ''  ('',1pe8.1,'' to '',e8.1,'')'')')
     &  nstep,c_ydh,
     &  sum/(area*thref*onemm),smin/(thref*onemm),smax/(thref*onemm)
*       write(nod,'(i9,a,
*    .              '' mean MontgPot (mm):'',f8.2)')
*    .  nstep,c_ydh,
*    .  smt/(area*thref*onemm)
        call flush(nod)
        endif !1st tile
c ---   NaN detection.
        if     (hycom_isnaninf(smin) .or.
     &          hycom_isnaninf(sum)  .or.
     &          hycom_isnaninf(smax) .or.
     &          hycom_isnaninf(smt)      ) then
          if (mnproc.eq.1) then
          write(lp,*)
          write(lp,*) 'error - NaN or Inf detected'
          write(lp,*)
          call flush(lp)
          endif !1st tile
          lfatal = .true.  !delay exit to allow archive output
        endif !NaN
c                
        if     (tidflg.gt.0) then
          call xcsum( dsms, util2,ip)
          sms=dsms/area
c         
          call xctilr(u(    1-nbdy,1-nbdy,1,n),1,kk, 1,1, halo_uv)
          call xctilr(v(    1-nbdy,1-nbdy,1,n),1,kk, 1,1, halo_vv)
          call xctilr(ubavg(1-nbdy,1-nbdy,  n),1, 1, 1,1, halo_uv)
          call xctilr(vbavg(1-nbdy,1-nbdy,  n),1, 1, 1,1, halo_vv)
c         
          dskea=0.0d0
          do k=1,kk
!$OMP       PARALLEL DO PRIVATE(j,l,i,utotp,vtotp)
!$OMP&               SCHEDULE(STATIC,jblk)
            do j=1,jja
              do l=1,isp(j)
                do i=max(1,ifp(j,l)),min(ii,ilp(j,l))
                  utotp=0.5*( u(i,  j,k,n)+ubavg(i,  j,n) +
     &                        u(i+1,j,k,n)+ubavg(i+1,j,n)  )
                  vtotp=0.5*( v(i,j,  k,n)+vbavg(i,j,  n) +
     &                        v(i,j+1,k,n)+vbavg(i,j+1,n)  )
                  util4(i,j)=dp(i,j,k,n)*scp2(i,j)*
     &                      0.5*(1000.0+th3d(i,j,k,n)+thbase)*
     &                          (utotp**2+vtotp**2)
                enddo
              enddo
            enddo
!$OMP       END PARALLEL DO
            call xcsum(dske, util4,ip)
            dskea=dskea+dske
          enddo !k
          sum=dskea/(area*onem)
          if (mnproc.eq.1) then
          write (lp,'(i9,a,
     &                  '' region-wide mean KE: '',f20.10)')
     &        nstep,c_ydh,
     &          sum
          write (lp,'(i9,a,
     &                  '' region-wide mean APE:'',f20.10)')
     &        nstep,c_ydh,
     &          sms*0.5*g*(1000.0+thbase)
          endif
        endif !tidflg
*     else
*       if (mnproc.eq.1) then
*       write (lp,'('' time step ='',i9)') nstep
*       call flush(lp)
*       endif !1st tile
      endif !daily or hourly
c
c --- diagnose heat/salt flux, ice, layer thickness and temperature,
c --- mean temperature and mean kinetic energy
c --- note that mixed-layer fields must be switched on in mxkprf
      if (diagno .or.
     &    nstep.eq.nstep0+1 .or.
     &    mod(dtime+dsmall,days6).lt.dsmall2) then  ! at least every 6 days
c
#if defined(ARCTIC)
c ---   Arctic (tripole) domain, top row is replicated (ignore it)
        jja = min( jj, jtdm-1-j0 )
        if     (jja.ne.jj) then
          do i=1,ii
            util1(i,jj)=0.0
            util2(i,jj)=0.0
            util3(i,jj)=0.0
            util4(i,jj)=0.0
            util5(i,jj)=0.0
          enddo
        endif
#else
        jja = jj
#endif
c
#if defined(USE_CCSM3)
        jday = days_in_prior_months(imonth)+iday
#else
        call forday(dtime,yrflag, iyear,jday,ihour)
#endif
        write(c_ydh,'('' ('',i4.4,''/'',i3.3,1x,i2.2,'')'')')
     &    iyear,jday,ihour
c
        if (thermo .or. sstflg.gt.0 .or. srelax) then
!$OMP     PARALLEL DO PRIVATE(j,l,i)
!$OMP&             SCHEDULE(STATIC,jblk)
          do j=1,jja
            do l=1,isp(j)
              do i=max(1,ifp(j,l)),min(ii,ilp(j,l))
                util1(i,j)=buoflx(i,j)*scp2(i,j)
                util2(i,j)=bhtflx(i,j)*scp2(i,j)
              enddo
            enddo
          enddo
!$OMP     END PARALLEL DO
          call xcsum(dsum, util1,ip)
          call xcsum(dsmt, util2,ip)
          sum= (dsum*1.00D9)/area  ! 1.e9*m**2/sec**3
          smt= (dsmt*1.00D9)/area  ! 1.e9*m**2/sec**3
          if (mnproc.eq.1) then
          write (lp, '(i9,a,
     &        '' mean BFL (m^2/s^3):'',f8.2,
     &                       '' hfl:'',f8.2)')
     &      nstep,c_ydh,
     &      sum,smt
          call flush(lp)
          write (nod,'(i9,a,
     &        '' mean BFL (m^2/s^3):'',f8.2,
     &                       '' hfl:'',f8.2)')
     &      nstep,c_ydh,
     &      sum,smt
          call flush(nod)
          endif !1st tile
c
!$OMP     PARALLEL DO PRIVATE(j,l,i)
!$OMP&             SCHEDULE(STATIC,jblk)
          do j=1,jja
            do l=1,isp(j)
              do i=max(1,ifp(j,l)),min(ii,ilp(j,l))
                util1(i,j)=surflx(i,j)*scp2(i,j)
                util2(i,j)=mixflx(i,j)*scp2(i,j)
                util3(i,j)=sstflx(i,j)*scp2(i,j)
              enddo
            enddo
          enddo
!$OMP     END PARALLEL DO
          call xcsum(dsum, util1,ip)
          call xcsum(dsmt, util2,ip)
          call xcsum(d3,   util3,ip)
          sum= dsum/area
          smt= dsmt/area
          smr= d3  /area
          if (mnproc.eq.1) then
          write (lp, '(i9,a,
     &        '' mean HFLUX (w/m^2):'',f8.2,
     &                       '' sst:'',f8.2,
     &                       ''  ml:'',f8.2)')
     &      nstep,c_ydh,
     &      sum,smr,smt
          call flush(lp)
          write (nod,'(i9,a,
     &        '' mean HFLUX (w/m^2):'',f8.2,
     &                       '' sst:'',f8.2,
     &                       ''  ml:'',f8.2)')
     &      nstep,c_ydh,
     &      sum,smr,smt
          call flush(nod)
          endif !1st tile
c
!$OMP     PARALLEL DO PRIVATE(j,l,i)
!$OMP&             SCHEDULE(STATIC,jblk)
          do j=1,jja
            do l=1,isp(j)
              do i=max(1,ifp(j,l)),min(ii,ilp(j,l))
                util1(i,j)=salflx(i,j)*scp2(i,j)/saln(i,j,1,n)
                util2(i,j)=sssflx(i,j)*scp2(i,j)/saln(i,j,1,n)
              enddo
            enddo
          enddo
!$OMP     END PARALLEL DO
          call xcsum(dsms, util1,ip)
          call xcsum(dsum, util2,ip)
          sms=-(dsms*thref*7.0D0*8.64D7)/area  ! P-E in mm/week
          smr=-(dsum*thref*7.0D0*8.64D7)/area  ! P-E in mm/week
          if (mnproc.eq.1) then
          write (lp, '(i9,a,
     &        '' mean WFLUX (mm/wk):'',f8.2,
     &                       '' sss:'',f8.2)')
     &      nstep,c_ydh,
     &      sms,smr
          call flush(lp)
          write (nod,'(i9,a,
     &        '' mean WFLUX (mm/wk):'',f8.2,
     &                       '' sss:'',f8.2)')
     &      nstep,c_ydh,
     &      sms,smr
          call flush(nod)
          endif !1st tile
        endif
c
        if (icegln) then  ! basin-wide ice
!$OMP     PARALLEL DO PRIVATE(j,l,i)
!$OMP&             SCHEDULE(STATIC,jblk)
          do j=1,jja
            do l=1,isp(j)
              do i=max(1,ifp(j,l)),min(ii,ilp(j,l))
                if     (covice(i,j).ne.0.0) then
                  util2(i,j)=covice(i,j)*            scp2(i,j)
                  util3(i,j)=            thkice(i,j)*scp2(i,j)
                  util4(i,j)=covice(i,j)*temice(i,j)*scp2(i,j)
                else
                  util2(i,j)=0.0
                  util3(i,j)=0.0
                  util4(i,j)=0.0
                endif
              enddo
            enddo
          enddo
!$OMP     END PARALLEL DO
          call xcsum(d2, util2,ip)
          call xcsum(d3, util3,ip)
          call xcsum(d4, util4,ip)
          if     (d2.gt.0.0d0) then
            sum=d3/d2   !average ice thickness,   where there is ice
            smt=d4/d2   !average ice temperature, where there is ice
            sms=d2/area * 100.0  !ice coverage, percent of total area
          else
            sum=0.0 
            smt=0.0 
            sms=0.0 
          endif
          if (mnproc.eq.1) then
          write (lp,'(i9,a,
     &              '' mean  ice thk. (m):'',f8.2,
     &                           ''  temp:'',f7.3,
     &                            '' pcen:'',f7.3)')
     &      nstep,c_ydh,
     &      sum,smt,sms
          call flush(lp)
          write(nod,'(i9,a,
     &              '' mean  ice thk. (m):'',f8.2,
     &                           ''  temp:'',f7.3,
     &                            '' pcen:'',f7.3)')
     &      nstep,c_ydh,
     &      sum,smt,sms
          call flush(nod)
          endif !1st tile
        endif  ! icegln
c
        if (nreg.ne.0 .and. icegln) then  ! southern hemisphere ice
          d2a = d2
          d3a = d3
          d4a = d4
!$OMP     PARALLEL DO PRIVATE(j,l,i)
!$OMP&             SCHEDULE(STATIC,jblk)
          do j=1,jja
            do l=1,isp(j)
              do i=max(1,ifp(j,l)),min(ii,ilp(j,l))
                if     (covice(i,j).ne.0.0 .and.
     &                    plat(i,j).lt.0.0      ) then
                  util2(i,j)=covice(i,j)*            scp2(i,j)
                  util3(i,j)=            thkice(i,j)*scp2(i,j)
                  util4(i,j)=covice(i,j)*temice(i,j)*scp2(i,j)
                else
                  util2(i,j)=0.0
                  util3(i,j)=0.0
                  util4(i,j)=0.0
                endif
              enddo
            enddo
          enddo
!$OMP     END PARALLEL DO
          call xcsum(d2, util2,ip)
          call xcsum(d3, util3,ip)
          call xcsum(d4, util4,ip)
          if     (d2.gt.0.0d0) then
            sum=d3/d2   !average ice thickness,   where there is ice in S.H.
            smt=d4/d2   !average ice temperature, where there is ice in S.H.
            sms=d2/area * 100.0  !S.H. ice coverage, percent of total area
          else
            sum=0.0 
            smt=0.0 
            sms=0.0 
          endif
          if (mnproc.eq.1) then
          write (lp,'(i9,a,
     &              '' mean SH I thk. (m):'',f8.2,
     &                           ''  temp:'',f7.3,
     &                            '' pcen:'',f7.3)')
     &      nstep,c_ydh,
     &      sum,smt,sms
          call flush(lp)
          write(nod,'(i9,a,
     &              '' mean SH I thk. (m):'',f8.2,
     &                           ''  temp:'',f7.3,
     &                            '' pcen:'',f7.3)')
     &      nstep,c_ydh,
     &      sum,smt,sms
          call flush(nod)
          endif !1st tile
c
          d2 = d2a - d2
          d3 = d3a - d3
          d4 = d4a - d4
          if     (d2.gt.0.0d0) then
            sum=d3/d2   !average ice thickness,   where there is ice in N.H.
            smt=d4/d2   !average ice temperature, where there is ice in N.H.
            sms=d2/area * 100.0  !N.H. ice coverage, percent of total area
          else
            sum=0.0 
            smt=0.0 
            sms=0.0 
          endif
          if (mnproc.eq.1) then
          write (lp,'(i9,a,
     &              '' mean NH I thk. (m):'',f8.2,
     &                           ''  temp:'',f7.3,
     &                            '' pcen:'',f7.3)')
     &      nstep,c_ydh,
     &      sum,smt,sms
          call flush(lp)
          write(nod,'(i9,a,
     &              '' mean NH I thk. (m):'',f8.2,
     &                           ''  temp:'',f7.3,
     &                            '' pcen:'',f7.3)')
     &      nstep,c_ydh,
     &      sum,smt,sms
          call flush(nod)
          endif !1st tile
        endif  ! icegln .and. nreg.ne.0
c
        if     (icegln .and.
     &          (icmflg.eq.2 .or. ticegr.eq.0.0 .or.
     &           lwflag.eq.2 .or. sstflg.eq.2       )) then
c
c ---     ice mask
c
!$OMP     PARALLEL DO PRIVATE(j,l,i,tsur)
!$OMP&             SCHEDULE(STATIC,jblk)
          do j=1,jja
            do l=1,isp(j)
              do i=max(1,ifp(j,l)),min(ii,ilp(j,l))
c
c ---           don't allow a new tsur maximum, to preserve sea ice
c
                if (yrflag.lt.2) then
                  tsur = min( max( surtmp(i,j,l0), surtmp(i,j,l1),
     &                             surtmp(i,j,l2), surtmp(i,j,l3) ),
     &                        surtmp(i,j,l0)*w0+surtmp(i,j,l1)*w1+
     &                        surtmp(i,j,l2)*w2+surtmp(i,j,l3)*w3   )
                else
                  tsur = min( max( surtmp(i,j,l0), surtmp(i,j,l1) ),
     &                        surtmp(i,j,l0)*w0+surtmp(i,j,l1)*w1   )
                endif
                if     (tsur.le.tfrz_n) then
                  util1(i,j) = scp2(i,j)
                  if     (plat(i,j).lt.0.0) then
                    util2(i,j) = scp2(i,j)
                  else
                    util2(i,j) = 0.0
                  endif
                else
                  util1(i,j) = 0.0
                  util2(i,j) = 0.0
                endif
              enddo
            enddo
          enddo
!$OMP     END PARALLEL DO
          call xcsum(dsmt, util1,ip)
          call xcsum(dsms, util2,ip)
          smt=dsmt/area * 100.0
          sms=dsms/area * 100.0
          if (mnproc.eq.1) then
          write (lp,'(i9,a,
     &              '' mean ice mask pcen:'',f9.3,
     &                            ''  S.H:'',f7.3,
     &                            ''  N.H:'',f7.3)')
     &    nstep,c_ydh,
     &    smt,sms,smt-sms
          call flush(lp)
          write(nod,'(i9,a,
     &              '' mean ice mask pcen:'',f9.3,
     &                            ''  S.H:'',f7.3,
     &                            ''  N.H:'',f7.3)')
     &    nstep,c_ydh,
     &    smt,sms,smt-sms
          call flush(nod)
          endif !1st tile
        endif  !ice mask statistics
c
!$OMP   PARALLEL DO PRIVATE(j,l,i)
!$OMP&           SCHEDULE(STATIC,jblk)
        do j=1,jja
          do l=1,isp(j)
            do i=max(1,ifp(j,l)),min(ii,ilp(j,l))
              util1(i,j)=dpmixl(i,j,n)*scp2(i,j)
              util2(i,j)=dpmixl(i,j,n)*scp2(i,j)*tmix(i,j)
              util3(i,j)=dpmixl(i,j,n)*scp2(i,j)*smix(i,j)
              util4(i,j)=temp(i,j,1,n)*scp2(i,j)
              util5(i,j)=saln(i,j,1,n)*scp2(i,j)
            enddo
          enddo
        enddo
!$OMP   END PARALLEL DO
        call xcsum(dsum, util1,ip)
        call xcsum(dsmt, util2,ip)
        call xcsum(dsms, util3,ip)
        if     (dsum.ne.0.0d0) then
          sum=dsum/(area*onem)
          smt=dsmt/dsum
          sms=dsms/dsum
        else
          sum=0.0
          smt=0.0
          sms=0.0
        endif
        if (mnproc.eq.1) then
        write (lp,'(i9,a,
     &              '' mean mixl thk. (m):'',f8.2,
     &                           ''  temp:'',f7.3,
     &                            '' saln:'',f7.3)')
     &      nstep,c_ydh,
     &      sum,smt,sms
        call flush(lp)
        write(nod,'(i9,a,
     &              '' mean mixl thk. (m):'',f8.2,
     &                           ''  temp:'',f7.3,
     &                            '' saln:'',f7.3)')
     &      nstep,c_ydh,
     &      sum,smt,sms
        call flush(nod)
        endif !1st tile
c
        call xcsum(dsmt, util4,ip)
        call xcsum(dsms, util5,ip)
        smt=dsmt/area
        sms=dsms/area
        if (mnproc.eq.1) then
        write (lp,'(i9,a,
     &            '' mean surf thk. (m):'',f8.2,
     &                         ''   sst:'',f7.3,
     &                          ''  sss:'',f7.3)')
     &    nstep,c_ydh,
     &    dp00*qonem,smt,sms  !dp00 is max, not mean, surf thk.
        call flush(lp)
        write(nod,'(i9,a,
     &            '' mean surf thk. (m):'',f8.2,
     &                         ''   sst:'',f7.3,
     &                          ''  sss:'',f7.3)')
     &    nstep,c_ydh,
     &    dp00*qonem,smt,sms  !dp00 is max, not mean, surf thk.
        call flush(nod)
        endif !1st tile
c
        if     (relaxf .or. sstflg.ne.0) then
c
c ---     mean surface climatology.
c
!$OMP     PARALLEL DO PRIVATE(j,l,i)
!$OMP&             SCHEDULE(STATIC,jblk)
          do j=1,jja
            do l=1,isp(j)
              do i=max(1,ifp(j,l)),min(ii,ilp(j,l))
                if     (relaxf .and. sstflg.le.1) then
                  util1(i,j)=scp2(i,j)*
     &                (twall(i,j,1,lc0)*wc0+twall(i,j,1,lc1)*wc1
     &                +twall(i,j,1,lc2)*wc2+twall(i,j,1,lc3)*wc3)
                else !synoptic observed sst
                  util1(i,j)=scp2(i,j)*
     &                (seatmp(i,j,l0)*w0+seatmp(i,j,l1)*w1
     &                +seatmp(i,j,l2)*w2+seatmp(i,j,l3)*w3)
                endif
                if     (relaxf) then !sss
                  util2(i,j)=scp2(i,j)*
     &                  (swall(i,j,1,lc0)*wc0+swall(i,j,1,lc1)*wc1
     &                  +swall(i,j,1,lc2)*wc2+swall(i,j,1,lc3)*wc3)
                else !synoptic observed surface temperature
                  util1(i,j)=scp2(i,j)*
     &                (surtmp(i,j,l0)*w0+surtmp(i,j,l1)*w1
     &                +surtmp(i,j,l2)*w2+surtmp(i,j,l3)*w3)
                endif
              enddo
            enddo
          enddo
!$OMP     END PARALLEL DO
          call xcsum(dsmt, util1,ip)
          call xcsum(dsms, util2,ip)
          smt=dsmt/area
          sms=dsms/area
          if (mnproc.eq.1) then
          if     (relaxf) then
            write (lp,'(i9,a,
     &                '' mean clim thk. (m):'',f8.2,
     &                             ''   sst:'',f7.3,
     &                              ''  sss:'',f7.3)')
     &        nstep,c_ydh,
     &        thkmin,smt,sms
            call flush(lp)
            write(nod,'(i9,a,
     &                '' mean clim thk. (m):'',f8.2,
     &                             ''   sst:'',f7.3,
     &                              ''  sss:'',f7.3)')
     &        nstep,c_ydh,
     &        thkmin,smt,sms
            call flush(nod)
          else !.not.relaxf
            write (lp,'(i9,a,
     &                '' mean clim thk. (m):'',f8.2,
     &                             ''   sst:'',f7.3,
     &                              '' surt:'',f7.3)')
     &        nstep,c_ydh,
     &        thkmin,smt,sms
            call flush(lp)
            write(nod,'(i9,a,
     &                '' mean clim thk. (m):'',f8.2,
     &                             ''   sst:'',f7.3,
     &                              '' surt:'',f7.3)')
     &        nstep,c_ydh,
     &        thkmin,smt,sms
            call flush(nod)
          endif !relaxf:else
          endif !1st tile
        endif
c
        call xctilr(u(    1-nbdy,1-nbdy,1,n),1,kk, 1,1, halo_uv)
        call xctilr(v(    1-nbdy,1-nbdy,1,n),1,kk, 1,1, halo_vv)
        call xctilr(ubavg(1-nbdy,1-nbdy,  n),1, 1, 1,1, halo_uv)
        call xctilr(vbavg(1-nbdy,1-nbdy,  n),1, 1, 1,1, halo_vv)
c
        dsuma=0.0d0
        dsmta=0.0d0
        dsmsa=0.0d0
        dsmra=0.0d0
        dskea=0.0d0
        do k=1,kk
!$OMP     PARALLEL DO PRIVATE(j,l,i,utotp,vtotp)
!$OMP&             SCHEDULE(STATIC,jblk)
          do j=1,jja
            do l=1,isp(j)
              do i=max(1,ifp(j,l)),min(ii,ilp(j,l))
                utotp=0.5*( u(i,  j,k,n)+ubavg(i,  j,n) +
     &                      u(i+1,j,k,n)+ubavg(i+1,j,n)  )
                vtotp=0.5*( v(i,j,  k,n)+vbavg(i,j,  n) +
     &                      v(i,j+1,k,n)+vbavg(i,j+1,n)  )
                util1(i,j)=dp(i,j,k,n)*scp2(i,j)
                util2(i,j)=dp(i,j,k,n)*scp2(i,j)*temp(i,j,k,n)
                util3(i,j)=dp(i,j,k,n)*scp2(i,j)*saln(i,j,k,n)
                util5(i,j)=dp(i,j,k,n)*scp2(i,j)*th3d(i,j,k,n)
                util4(i,j)=dp(i,j,k,n)*scp2(i,j)*
     &                    0.5*(1000.0+th3d(i,j,k,n)+thbase)*
     &                        (utotp**2+vtotp**2)
              enddo
            enddo
          enddo
!$OMP     END PARALLEL DO
          call xcsum(dsum, util1,ip)
          call xcsum(dsmt, util2,ip)
          call xcsum(dsms, util3,ip)
          call xcsum(dsmr, util5,ip)
          call xcsum(dske, util4,ip)
          dsuma=dsuma+dsum
          dsmta=dsmta+dsmt
          dsmsa=dsmsa+dsms
          dsmra=dsmra+dsmr
          dskea=dskea+dske
          if     (dsum.ne.0.0d0) then
            sum=dsum/(area*onem)
            smt=dsmt/dsum
            sms=dsms/dsum
          else
            sum=0.0
            smt=0.0
            sms=0.0
          endif
          if (mnproc.eq.1) then
          write (lp,'(i9,a,
     &                '' mean L '',i2,'' thk. (m):'',f8.2,
     &                                   ''  temp:'',f7.3,
     &                                    '' saln:'',f7.3)')
     &        nstep,c_ydh,
     &        k,sum,smt,sms
          call flush(lp)
          write(nod,'(i9,a,
     &                '' mean L '',i2,'' thk. (m):'',f8.2,
     &                                   ''  temp:'',f7.3,
     &                                    '' saln:'',f7.3)')
     &        nstep,c_ydh,
     &        k,sum,smt,sms
          call flush(nod)
          endif !1st tile
        enddo !k
        sum=dskea/(area*onem)
        smt=dsmta/dsuma
        sms=dsmsa/dsuma
        smr=dsmra/dsuma
        if (mnproc.eq.1) then
        write (lp,'(i9,a,
     &                '' region-wide mean Kin. Energy:'',f20.10)')
     &      nstep,c_ydh,
     &        sum
        write (lp,'(i9,a,
     &                '' region-wide mean Temperature:'',f20.10)')
     &      nstep,c_ydh,
     &        smt
        write (lp,'(i9,a,
     &                '' region-wide mean Salinity:   '',f20.10)')
     &      nstep,c_ydh,
     &        sms
        write (lp,'(i9,a,
     &                '' region-wide mean Density Dev:'',f20.10)')
     &      nstep,c_ydh,
     &        smr
        call flush(lp)
        write(nod,'(i9,a,
     &                '' region-wide mean Kin. Energy:'',f20.10)')
     &      nstep,c_ydh,
     &        sum
        write(nod,'(i9,a,
     &                '' region-wide mean Temperature:'',f20.10)')
     &      nstep,c_ydh,
     &        smt
        write(nod,'(i9,a,
     &                '' region-wide mean Salinity:   '',f20.10)')
     &      nstep,c_ydh,
     &        sms
        write(nod,'(i9,a,
     &                '' region-wide mean Density Dev:'',f20.10)')
     &      nstep,c_ydh,
     &        smr
        call flush(nod)
        endif !1st tile
c
        do ktr= 1,ntracr
          dsumtr(ktr)=0.0d0
          do k=1,kk
!$OMP       PARALLEL DO PRIVATE(j,l,i)
!$OMP&               SCHEDULE(STATIC,jblk)
            do j=1,jja
              do l=1,isp(j)
                do i=max(1,ifp(j,l)),min(ii,ilp(j,l))
                  util2(i,j)=dp(i,j,k,n)*scp2(i,j)*tracer(i,j,k,n,ktr)
                enddo !i
              enddo !l
            enddo !j
!$OMP       END PARALLEL DO
            call xcsum(dsmt, util2,ip)
            dsumtr(ktr)=dsumtr(ktr)+dsmt
          enddo !k
          smt=dsumtr(ktr)/dsuma  !dsuma still good from K.E. loops
          if (mnproc.eq.1) then
          write (lp,'(i9,a,
     &                  '' region-wide mean tracer'',i3.2,
     &                  '':  '',f20.10)')
     &        nstep,c_ydh,
     &          ktr,smt
          call flush(lp)
          write(nod,'(i9,a,
     &                  '' region-wide mean tracer'',i3.2,
     &                  '':  '',f20.10)')
     &        nstep,c_ydh,
     &          ktr,smt
          call flush(nod)
          endif !1st tile
c ---     NaN detection, for each tracer.
          if     (hycom_isnaninf(smt)) then
            if (mnproc.eq.1) then
            write(lp,*)
            write(lp,*) 'error - NaN or Inf detected'
            write(lp,*)
            call flush(lp)
            endif !1st tile
            lfatal = .true.  !delay exit to allow archive output
          endif !NaN
          if     (ktr.ge.3 .and. trcflg(ktr-2).eq.903) then !NPZ
            smt=(dsumtr(ktr)+dsumtr(ktr-1)+dsumtr(ktr-2))/dsuma
            if (mnproc.eq.1) then
            write (lp,'(i9,a,
     &                    '' region-wide mean N+P+Z:      '',f20.10)')
     &          nstep,c_ydh,
     &            smt
            call flush(lp)
            write(nod,'(i9,a,
     &                    '' region-wide mean N+P+Z:      '',f20.10)')
     &          nstep,c_ydh,
     &            smt
            call flush(nod)
            endif !1st tile
          elseif (ktr.ge.4 .and. trcflg(ktr-3).eq.904) then !NPZD
            smt=(dsumtr(ktr)  +dsumtr(ktr-1)+
     &           dsumtr(ktr-2)+dsumtr(ktr-3))/dsuma
            if (mnproc.eq.1) then
            write (lp,'(i9,a,
     &                    '' region-wide mean N+P+Z+D:    '',f20.10)')
     &          nstep,c_ydh,
     &            smt
            call flush(lp)
            write(nod,'(i9,a,
     &                    '' region-wide mean N+P+Z+D:    '',f20.10)')
     &          nstep,c_ydh,
     &            smt
            call flush(nod)
            endif !1st tile
          endif !NPZ:NPZD
        enddo !ktr
      endif  !diagno ...
c
#if ! defined(USE_CCSM3)
c --- diagnose meridional overturning and heat flux
      if     (mod(dtime+dsmall,dmonth).lt.dsmall2) then
        call xctmr0(52)
        call overtn(dtime,dyear)
        call xctmr1(52)
      elseif (nstep.ge.nstep2) then
        call xctmr0(52)
        call overtn(dtime,dyear)
        call xctmr1(52)
      endif
#endif
c
      if     (meanfq.ne.0.0) then
        if     (.not. histmn) then
          call mean_add(n, 1.0)
        else  ! histmn
          call mean_add(n, 0.5)
c
          call xctmr0(53)
c
c ---     output to mean archive file
c
          call mean_end(dtime)
#if defined(USE_CCSM3)
          jday = days_in_prior_months(imonth)+iday
          call mean_archiv(n, iyear,imonth,jday,ihour)
#else
          call forday(time_ave,yrflag, iyear,jday,ihour)
          call mean_archiv(n, iyear,jday,ihour)
#endif
c
          call mean_zero(dtime)
          call mean_add(n, 0.5)
c
          call xctmr1(53)
        endif  ! histmn
      endif  !meanfq
c
      if (histry .or. hiprof .or. hitile .or. hisurf .or. lfatal) then
        call xctmr0(53)
c
c ---   output to archive file
c
#if defined(USE_CCSM3)
        jday = days_in_prior_months(imonth)+iday
#else
        call forday(dtime,yrflag, iyear,jday,ihour)
#endif
c
!$OMP   PARALLEL DO PRIVATE(j,k,l,i)
!$OMP&           SCHEDULE(STATIC,jblk)
        do j=1,jj
          if (isopyc .or. mxlkrt) then
            do l=1,isu(j)
              do i=max(1,ifu(j,l)),min(ii,ilu(j,l))
                umix(i,j)=u(i,j,1,n)
              enddo
            enddo
            do l=1,isv(j)
              do i=max(1,ifv(j,l)),min(ii,ilv(j,l))
                vmix(i,j)=v(i,j,1,n)
              enddo
            enddo
          endif
          if (histry) then
            do k= 1,kk
              do l=1,isp(j)
                do i=max(1,ifp(j,l)),min(ii,ilp(j,l))
c ---             convert diapycnal thickness changes into 
c ---             actual interface fluxes
                  if (k.gt.1) then
                    diaflx(i,j,k)=diaflx(i,j,k)/(2.*onem) +
     &                            diaflx(i,j,k-1)
                  else
                    diaflx(i,j,k)=diaflx(i,j,k)/(2.*onem)
                  endif
                enddo
              enddo
            enddo !k
          endif
        enddo !j
!$OMP   END PARALLEL DO
c
        if     (lfatal) then  !write archive and exit
          if (mnproc.eq.1) then
          write (intvl,'(i3.3)') 0
          endif !1st tile
          call archiv(n, kk, iyear,jday,ihour, intvl)
          call xcstop('(hycom)')
                 stop '(hycom)'   !won't get here
        else
          if (mnproc.eq.1) then
          write (intvl,'(i3.3)') int(dtime-timav+dsmall)
          endif !1st tile
          if (hisurf) then
            call archiv(n, 1,  iyear,jday,ihour, intvl)
          endif
          if (histry) then
            call archiv(n, kk, iyear,jday,ihour, intvl)
          endif
          if     (hiprof) then
            call archiv_prof(n, kk, iyear,jday,ihour)
          endif
          if     (hitile) then
            call archiv_tile(n, kk, iyear,jday,ihour)
          endif
        endif  !lfatal:else
c
        if (histry) then
!$OMP     PARALLEL DO PRIVATE(j,k,l,i)
!$OMP&             SCHEDULE(STATIC,jblk)
          do j=1,jj
            do k= 1,kk
              do l=1,isp(j)
                do i=max(1,ifp(j,l)),min(ii,ilp(j,l))
                  diaflx(i,j,k)=0.0
                enddo
              enddo
            enddo
          enddo
!$OMP     END PARALLEL DO
c
          timav=time
        endif
        call xctmr1(53)
      endif  ! histry.or.hiprof.or.hitile.or.hisurf.or.lfatal
c
c
      if (restrt) then
        call xctmr0(51)
c
c ---   output to restart and flux statitics files
c
        if (mxlkpp) then
!$OMP     PARALLEL DO PRIVATE(j,l,i)
!$OMP&             SCHEDULE(STATIC,jblk)
          do j=1,jj
            do l=1,isp(j)
              do i=max(1,ifp(j,l)),min(ii,ilp(j,l))
                dpmixl(i,j,m) = dpmixl(i,j,n)
              enddo
            enddo
          enddo
!$OMP     END PARALLEL DO
        endif
c
#if defined(USE_CCSM3)
        call ccsm3_time_date_stamp (ccsm3_string, 'ymds')
        
        flnmra = trim(flnmrsod)// '.'//trim(ccsm3_string)
        flnmrb = trim(flnmrsod)//'h.'//trim(ccsm3_string)
        
        if (mnproc == 1) then
          open(1,file=trim(pointer_filename),form='formatted',
     &            status='unknown')
          write(1,'(a)')trim(flnmra)
          write(1,'(a)')trim(flnmrb)
          write(1,*)time                        ! real,  in days
          write(1,*)dtime                       ! real8, in days
          write(1,*)nstep,iyear,imonth,iday     ! integer
     &,            elapsed_days
          write(1,*)eom, eoy                    ! logical
          close(1)
c
          write(lp,'(2a)')'RESTART: (mainloop) to file: ',trim(flnmra)
          write(lp,*)     '     time =                    ',time
          write(lp,*)     '     nstep,iyear,imonth,iday : ',
     &                          nstep,iyear,imonth,iday
          call flush(lp)
        endif !mnproc == 1
#else
        call forday(dtime,yrflag, iyear,jday,ihour)
        if (mnproc.eq.1) then
        write (lp,'(a,i9, 9x,a,i6.4, 9x,a,i5.3, 9x,a,i4.2)')
     &    ' time step',nstep,
     &    'y e a r',   iyear,
     &    'd a y',     jday,
     &    'h o u r',   ihour
        call flush(lp)
        endif !1st tile
c
        flnmra = flnmrso  !.a extension added by restart_out
        flnmrb = flnmrso  !.b extension added by restart_out
#endif
c
        call restart_out(nstep,dtime, flnmra,flnmrb, nstep.ge.nstep2)
c
c ---   set layer thickness (incl.bottom pressure) at u,v points
c ---   needed because restart_out may have modified dp
c
        call xctilr(dp(1-nbdy,1-nbdy,1,1),1,2*kk, nbdy,nbdy, halo_ps)
        call xctilr(dpmixl(1-nbdy,1-nbdy,1),1,2,  nbdy,nbdy, halo_ps)
c
        margin = nbdy
c
        do nm=1,2
!$OMP     PARALLEL DO PRIVATE(j,k,l,i)
!$OMP&             SCHEDULE(STATIC,jblk)
          do j=1-margin,jj+margin
            do l=1,isp(j)
              if     (nm.eq.mod(nstep+1,2)+1) then
                do i=max(1-margin,ifp(j,l)),min(ii+margin,ilp(j,l))
                  dpbl(i,j)=dpmixl(i,j,nm)
                enddo
              endif
              do i=max(1-margin,ifp(j,l)),min(ii+margin,ilp(j,l))
                p(i,j,1)=0.0
                do k=1,kk
                  p(i,j,k+1)=p(i,j,k)+dp(i,j,k,nm)
                enddo
              enddo
            enddo
          enddo
!$OMP     END PARALLEL DO
c
          call dpudpv(dpu(1-nbdy,1-nbdy,1,nm),
     &                dpv(1-nbdy,1-nbdy,1,nm),
     &                p,depthu,depthv, max(0,margin-1))
c            
        enddo  !nm=1,2
c
        call xctmr1(51)
      endif  ! restrt
c
      if     (histry .or. hiprof .or. hitile .or. hisurf) then
        if (mnproc.eq.1) then
        write (lp,'(a,i9,a,f9.2,a)')
     &    ' step',nstep,' day',dtime,' -- archiving completed --'
        call flush(lp)
        endif !1st tile
      endif
c
c --- read next incremental update.
c
      if (incflg.ne.0) then
        call incupd_rd(dtime)
      endif
c
      delt1=baclin+baclin
c
#if ! defined(USE_CCSM3)

      ! regular end-of-run condition
      end_of_run = nstep.ge.nstep2

      ! stepable end-of-run condition
      if (present(endtime)) then
        ! endtime present means that "end_of_run_cpl" needs to be set correctly
        nstep2_cpl=nint(endtime*(86400.0d0/baclin))

c        if(mnproc.eq.1) print *,"mod_hycom,nstep,nsteo2,nstep2_cpl =",
c     &    nstep,nstep2,nstep2_cpl

        if(nstep.ge.nstep2_cpl) then
          end_of_run_cpl = .true.
        else
          end_of_run_cpl = .false.
        endif
      endif

#endif
c
c --- at end: output float restart file 
c
      if     (synflt .and. end_of_run) then
        call floats_restart
      endif !synflt+end_of_run

      end subroutine HYCOM_Run

c===================================

      subroutine HYCOM_Final
c
c --- end of the run.
      if (mnproc.eq.1) then
      write(nod,'(a)') 'normal stop'
      call flush(nod)
      endif !1st tile
#if defined(USE_CCSM3)
c
c --- print active timers.
      call xctmrp
c
      call shr_timer_print ( timer_send_to_cpl  )
      call shr_timer_print ( timer_recv_from_cpl)
      call shr_timer_print ( timer_recv_to_send )
      call shr_timer_print ( timer_send_to_recv )
c
      call ccsm3_exit_HYCOM('hycom normal stop',normal_exit=.true.)
                      stop '(hycom normal stop)'  !won't get here
#else
      call xcstop('(normal)')  !calls xctmrp
             stop '(normal)'   !won't get here
#endif

      end subroutine HYCOM_Final
c===========================================

      end module mod_hycom
c
c
c> Revision history:
c>
c> May  1997 - removed statement "theta(1)=-thbase" after loop 14
c> June 1997 - added loop 60 to fix bug in vertical summation of -diaflx-
c> Oct. 1999 - option for krt or kpp mixed layer model - convec and diapfl
c>             not called for kpp mixing model
c> Oct. 1999 - dpbl (boundary layer thickness) is output in addition to
c>             dpmixl when the kpp mixing model is selected
c> May  2000 - conversion to SI units
c> Aug. 2000 - added isopycnic (MICOM) vertical coordinate option
c> Oct. 2000 - added option for high frequency atmospheric forcing
c> Nov. 2000 - archive time stamp is either time step or YYYY_DDD_HH
c> Aug. 2002 - added support for multiple tracers
c> Nov. 2002 - more basin-wide surface flux statistics
c> Dec. 2003 - more basin-wide mean statistics
c> Mar. 2005 - more accurate ice statistics
c> Jan. 2006 - mod_hycom with HYCOM_Init, HYCOM_Run, HYCOM_Final
c> Nov. 2006 - version 2.2
c> Nov. 2006 - added incremental update (for data assimilation)
c> Mar. 2007 - added srfhgt
c> Mar. 2010 - removed  DETIDE
c> Apr. 2010 - put back DETIDE
c> Apr. 2010 - added proffq
c> Apr. 2010 - added archiv_init
c> Mar. 2011 - mean archive now the same with and without tides
c> Apr. 2011 - surface archives separate from 3-D archives
