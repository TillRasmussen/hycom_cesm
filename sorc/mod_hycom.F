#if defined(ROW_LAND)
#define SEA_P .true.
#define SEA_U .true.
#define SEA_V .true.
#elif defined(ROW_ALLSEA)
#define SEA_P allip(j).or.ip(i,j).ne.0
#define SEA_U alliu(j).or.iu(i,j).ne.0
#define SEA_V alliv(j).or.iv(i,j).ne.0
#else
#define SEA_P ip(i,j).ne.0
#define SEA_U iu(i,j).ne.0
#define SEA_V iv(i,j).ne.0
#endif
      module mod_hycom
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      use mod_za         ! HYCOM I/O interface
      use mod_pipe       ! HYCOM debugging interface
      use mod_incupd     ! HYCOM incremental update (for data assimilation)
      use mod_floats     ! HYCOM synthetic floats, drifters and moorings
      use mod_tides      ! HYCOM tides
      use mod_archiv     ! HYCOM archives
      use mod_mean       ! HYCOM mean archives
      use mod_momtum     ! HYCOM momentum
      use mod_tsadvc     ! HYCOM scalar advection
#if defined(STOKES)
      use mod_stokes     ! HYCOM Stokes drift
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
      public HYCOM_Init, HYCOM_Run, HYCOM_Final
c
      logical, save, public  :: put_export       !set in main program
      logical, save, public  :: get_import       !set in main program
      logical, save, public  :: end_of_run       !set in HYCOM_Run
      integer, save, public  :: nts_day          !set in HYCOM_init, timesteps/day
      integer, save, public  :: nts_ice          !set in HYCOM_init, timesteps/ice
      logical, save, public  :: end_of_run_cpl   !set in HYCOM_Run for coupling
c
      integer, save, private ::
     &         m,n
      real*8,  save, private ::
     &        d1,d2,d3,d4,d2a,d3a,d4a,
     &        ddsurf,ddiagf,dproff,dtilef,drstrf,dmeanf,
     &        dske,dskea,dsmr,dsmra,dsms,dsmsa,dsmt,dsmta,dsum,dsuma,
#if defined(STOKES) 
     &        dskes,dskesa,
#endif
     &        dsumtr(mxtrcr),
     &        dtime,dtime0,dbimon,dmonth,dyear,dyear0,
     &        dsmall,dsmall2
      real,    save, private ::
     &        smr,sms,smt,smx,sum,smin,smax, tsur,
     &        coord,day1,day2,x,x1,time0,timav,cold,utotp,vtotp
      real,    save, private, allocatable ::
     &        sminy(:),smaxy(:)
      integer, save, private ::
     &        nstep0,nod,
     &        jja,
     &        lt,ma0,ma1,ma2,ma3,mc0,mc1,mc2,mc3,
     &           mk0,mk1,mk2,mk3,mr0,mr1,mr2,mr3,mnth,iofl,
     &        jday,ihour,iyear
      logical, save, private ::
     &         linit,diagsv,hisurf,histry,hiprof,hitile,histmn,
     &         restrt,diag_tide
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
!!Alex for pcpadj and new calendar
      real*8,  save, private ::
     &       vol, sai
      integer iyear_new,iday
      real    dtime_new
c
      contains


      subroutine HYCOM_Init(mpiCommunicator,
     &                      hycom_start_dtg,hycom_end_dtg,
     &                      pointer_filename,restart_write)
c
c --- Initialize (before the 1st time step).
c
      integer      i,j,k,l,nm,margin
      character*80 flnm,flnmra,flnmrb
      integer      mpi_comm_ocean,istat
      real          utot,vtot,usur1,usur2,vsur1,vsur2,uu,vv
c
!!Alex add variable for CESM
      integer,      intent(in), optional :: mpiCommunicator
      real,         intent(in), optional :: hycom_start_dtg
      real,         intent(in), optional :: hycom_end_dtg
      character*80, intent(in), optional :: pointer_filename
      logical,      intent(in), optional :: restart_write

      include 'stmt_fns.h'
c
c
c --- initialize SPMD processsing !!Alex
      if (present(mpiCommunicator)) then
          call MPI_Comm_Dup(mpiCommunicator,mpi_comm_ocean,istat)
          call xcspmd(mpi_comm_ocean)
      else
          call xcspmd
      endif

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
c --- initialize scalars
      call blkdat(hycom_start_dtg)  !must call before zaiost
c
c --- initialize array i/o.
      call zaiost
c
c --- initialize common variables
c
      call cb_allocate
      if (mnproc.eq.1) then
        call mem_stat_print('  cb_allocate:')
      endif !1st tile
c
c --- initiate named-pipe comparison utility
      call pipe_init
c
      nts_ice = icpfrq                     !no. time steps between ice coupling
      nts_day = nint(86400.0d0/baclin)     !no. time steps per day
      dsmall  = baclin/86400.0d0 * 0.25d0  !1/4 of a time step in days
      dsmall2 = dsmall*2.0d0
      if     (dsurfq.ge.1.0) then
        ddsurf = dsurfq
        write(6,'("Case 1 ddsurf =",G25.17)')ddsurf
      elseif (dsurfq.ne.0.0) then
        ddsurf = (baclin/86400.0d0)*
     &           max(1,nint((86400.0d0*dsurfq)/baclin))
        write(6,'("Case 1 ddsurf =",G25.17)')ddsurf
      else !no surface archives
        ddsurf = (baclin/86400.0d0)*0.99d0*huge(i)
      endif
      if     (diagfq.ge.1.0) then
        ddiagf = diagfq
      elseif (diagfq.ne.0.0) then
        ddiagf = (baclin/86400.0d0)*
     &           max(1,nint((86400.0d0*diagfq)/baclin))
      else !no 3-d archives
        ddiagf = (baclin/86400.0d0)*0.99d0*huge(i)
      endif
      if     (proffq.ge.1.0) then
        dproff = proffq
      elseif (proffq.ne.0.0) then
        dproff = (baclin/86400.0d0)*
     &           max(1,nint((86400.0d0*proffq)/baclin))
      else !no 3-d profiles at selection locations
        dproff = (baclin/86400.0d0)*0.99d0*huge(i)
      endif
      if     (tilefq.ge.1.0) then
        dtilef = tilefq
      elseif (tilefq.ne.0.0) then
        dtilef = (baclin/86400.0d0)*
     &           max(1,nint((86400.0d0*tilefq)/baclin))
      else !no tiled 3-d archives
        dtilef = (baclin/86400.0d0)*0.99d0*huge(i)
      endif
      if     (meanfq.ge.1.0) then
        dmeanf = meanfq
      elseif (meanfq.ne.0.0) then
        dmeanf = (baclin/86400.0d0)*
     &           max(1,nint((86400.0d0*meanfq)/baclin))
      else !no mean archives
        dmeanf = (baclin/86400.0d0)*0.99d0*huge(i)
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
!!Alex add calendar for CESM
      elseif (yrflag.eq.4) then
c ---   365 days, starting Jan 1 - CESM: NO-LEAP calendar
c ---   also implies high frequency atmospheric forcing
        dyear  = 365.0d0
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

cc --- model is to be integrated from time step 'nstep1' to 'nstep2'
!!Alex no limits file for CESM
      if (.not.present(hycom_start_dtg)  .or. 
     &    .not.present(hycom_end_dtg  )) then
          open( unit=uoff+99,file=trim(flnminp)//'limits')
          read(      uoff+99,*) day1,day2
          close(unit=uoff+99)
      endif
c --- non-positive day1 indicates a new initialization, or
c --- the start of a yrflag==3 case.
!!Alex  set limits  for CESM
      if (present(hycom_start_dtg)) day1 = hycom_start_dtg
      if (present(hycom_end_dtg  )) day2 = hycom_end_dtg
      linit =day1.le.0.0 
      day1  =abs(day1)
c
      dtime=day1
      nstep1=nint(dtime*(86400.0d0/baclin))
      dtime=(nstep1/nts_day)+mod(nstep1,nts_day)*(baclin/86400.0d0)
      day1 =dtime
c
      dtime=day2
      nstep2=nint(dtime*(86400.0d0/baclin))
      dtime=(nstep2/nts_day)+mod(nstep2,nts_day)*(baclin/86400.0d0)
      day2 =dtime
    
c --- set up forcing functions
c
      if (yrflag.lt.2) then
        call forfuna  ! monthly atmospheric forcing
      endif
      if     (jerlv0.eq. 0) then
        call forfunk  !  annual/monthly kpar
      elseif (jerlv0.eq.-1) then
        call forfunc  !  annual/monthly chl
      endif

#ifdef NUOPC
      if (.not. (cpl_orivers .and. cpl_irivers)) then
          if (linit) then
              call forfunp      !    annual/monthly rivers
          endif
      endif
#else
      call forfunp              !    annual/monthly rivers
#endif

      call forfunr              ! bimonthly/monthly climatology
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
        call forfund(tiddrg)    !tidal drag scalar or tensor
      else
        drgten(:,:,:,:)=0.0
      endif
c
c --- spatially varying sss relaxation !!Alex
c
      maskms(:,:)=1. !define the marginal sea mask
      if (sssflg.ne.0) then
          if     (sssrmu.lt.0.) then
              call forfunrs     ! spatially varying rmus
          elseif (sssrmu.gt.0.) then
              rmus(:,:)=1./(sssrmu*86400.)
          else 
              rmus(:,:)=0.
          endif
      endif 
c
c --- spatially varying sst relaxation !!Alex
c
      if (sstflg.ne.0) then
          if     (sstrmu.lt.0.) then
              call forfunrs     ! spatially varying rmut
          elseif (sstrmu.gt.0.) then
              rmut(:,:)=1./(sstrmu*86400.)
          else 
              rmut(:,:)=0.
          endif
      endif 
c
c --- "scalar" tidal SAL factor
c
      if     (tidflg.eq.0) then
        salfac(:,:)=0.0     !not used, set it for safety
      elseif (tidsal.lt.0.0) then
        call forfuns        !varying tidal SAL factor
      else
        salfac(:,:)=tidsal  !scalar  tidal SAL factor
      endif
c
c --- veldf2, veldf4 and thkdf4 may be spacially varying
c
      call forfundf
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

      if (linit) then
c
c ---   set up initial conditions
c
        nstep0=nstep1
        dtime0=(nstep0/nts_day)+mod(nstep0,nts_day)*(baclin/86400.0d0)
        time0=dtime0
        delt1=baclin
        if     (clmflg.eq.12) then
          mnth=   1+nint(mod(dtime0+dyear0,dyear)/dmonth)
        elseif (clmflg.eq.6) then
          mnth=2*(1+nint(mod(dtime0+dyear0,dyear)/dbimon))-1
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
!$OMP   PARALLEL DO PRIVATE(j,k,i)
!$OMP&           SCHEDULE(STATIC,jblk)
        do j=1,jj
          do i=1,ii
            if (SEA_P) then
                tmix(i,j) = temp(i,j,1,n)
                smix(i,j) = saln(i,j,1,n)
               thmix(i,j) = th3d(i,j,1,n)
              surflx(i,j) = 0.0
              salflx(i,j) = 0.0
            endif !ip
          enddo !i
          if (isopyc .or. mxlkrt) then
            do i=1,ii
              if (SEA_U) then
#if defined(STOKES) 
                umix(i,j)=u(i,j,1,n) + usd(i,j,1)
#else
                umix(i,j)=u(i,j,1,n)
#endif
              endif !iu
              if (SEA_V) then
#if defined(STOKES)
                vmix(i,j)=v(i,j,1,n) + vsd(i,j,1)
#else
                vmix(i,j)=v(i,j,1,n)
#endif
              endif !iv
            enddo !i
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
!!Alex add restart through pointer_filename (CESM)
        if (present(pointer_filename)) then
          open(1,file=trim(pointer_filename),form='formatted',
     &           status='old')
          read(1,'(a)') flnmrsi
          close(1)
        endif 

        flnmra = trim(flnmrsi)//'.a'
        flnmrb = trim(flnmrsi)//'.b'
        call restart_in(nstep0,dtime0, flnmra,flnmrb)
        surflx(:,:) = 0.0
        salflx(:,:) = 0.0
        nstep0=nint(dtime0*(86400.0d0/baclin))
        dtime0=(nstep0/nts_day)+mod(nstep0,nts_day)*(baclin/86400.0d0)
        time0=dtime0
        delt1=baclin+baclin
        if (mnproc.eq.1) then
        write (lp,'(a,f8.1,a,i9,a, a,f8.1,a,i9,a)')
     &    'restart on day',time0,' (step',nstep0,')',
     &    ', wanted day',   day1,' (step',nstep1,')'
        endif !1st tile
        if (nstep0.ne.nstep1) then
          if (mnproc.eq.1) then
          write(lp,'(/a/a,f8.1/a,f8.1/)')
     &      'error in hycom - wrong restart (or limits) file',
     &      'restart file day is ',time0,
     &      'limits start day is ',day1
          endif !1st tile
          call xcstop('(hycom)')
                 stop '(hycom)'   !won't get here
        endif !nstep0.ne.nstep1
c
        if     (clmflg.eq.12) then
          mnth=   1+nint(mod(dtime0+dyear0,dyear)/dmonth)
        elseif (clmflg.eq.6) then
          mnth=2*(1+nint(mod(dtime0+dyear0,dyear)/dbimon))-1
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
          l0=1
          l1=2
          l2=3
          l3=4
          w0=0.0
          w1=0.0
          w2=0.0
          w3=0.0
          call momtum_hs(n,m)  !calculate srfhgt
          nstep=nstep0
          time=dtime0
          call forday(dtime0,yrflag, iyear,jday,ihour)
          if (mnproc.eq.1) then
          write (intvl,'(i3.3)') 0
          endif !1st tile
          surflx(:,:) = 0.0
          salflx(:,:) = 0.0
          if     (difout) then
            vcty(:,:,:) = 0.0
            dift(:,:,:) = 0.0
            difs(:,:,:) = 0.0
          endif
          call archiv(n, kk, iyear,jday,ihour, intvl)
        endif !archive output
      endif !initial conditions
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
      nstep = nstep0+1  !for pipe_compare
      do nm=1,2
c
!$OMP   PARALLEL DO PRIVATE(j,k,i)
!$OMP&           SCHEDULE(STATIC,jblk)
        do j=1-margin,jj+margin
          if     (nm.eq.mod(nstep+1,2)+1) then
            do i=1-margin,ii+margin
              if (SEA_P) then
                dpbl( i,j)=dpmixl(i,j,nm)
                dpbbl(i,j)=thkbot*onem
              endif !ip
            enddo !i
          endif !nm
          do i=1-margin,ii+margin
            if (SEA_P) then
              p(i,j,1)=0.0
              do k=1,kk
                p(i,j,k+1)=p(i,j,k)+dp(i,j,k,nm)
              enddo !k
            endif !ip
          enddo !i
        enddo !j
!$OMP   END PARALLEL DO
c
        call dpudpv(dpu(1-nbdy,1-nbdy,1,nm),
     &              dpv(1-nbdy,1-nbdy,1,nm),
     &              p,depthu,depthv, margin,max(0,margin-1))
c
        if (.false.) then
c
c ---     ISOPYC TO HYBRID RESTART ONLY
          nstep=nstep1
          n=nm
          m=mod(n,2)+1
          call hybgen(m,n, .false.)
          call xctilr(dp(1-nbdy,1-nbdy,1,n),1,kk, nbdy,nbdy, halo_ps)
          margin = nbdy
c
!$OMP     PARALLEL DO PRIVATE(j,k,i)
!$OMP&             SCHEDULE(STATIC,jblk)
          do j=1-margin,jj+margin
!DIR$       PREFERVECTOR
            do i=1-margin,ii+margin
              if (SEA_P) then
                p(i,j,1)=0.0
                do k=1,kk
                  p(i,j,k+1)=p(i,j,k)+dp(i,j,k,n)
                enddo !k
              endif !ip
            enddo !i
          enddo !j
!$OMP     END PARALLEL DO
          call dpudpv(dpu(1-nbdy,1-nbdy,1,n),
     &                dpv(1-nbdy,1-nbdy,1,n),
     &                p,depthu,depthv, margin,max(0,margin-1))
             dpo = dp !for initial pipe_comparall
             call pipe_comparall(m,n, 'hybgen, step')
        endif  !isopyc to hybrid restart only
c
      enddo  !nm=1,2
      nstep = nstep-1  !restore

!!Alex add precipitation adjustment 
      if (pcpadj .and. linit ) then
          if (mnproc.eq.1) print*,'Alex pcpadj and linit'
! --- initialize sum_precip, ssh_initial and pcp_fact
          sum_precip  = 0.d0
          ssh_initial = 0.d0
          pcp_fact    = 1.d0
          pcp_facth   = pcp_fact

! --- initialize sal_initial 
          do k=1,kdm
!$OMP PARALLEL DO PRIVATE(j,i,k)
!$OMP&         SCHEDULE(STATIC,jblk)
            do j=1,jj
              do i=1,ii
                if (SEA_P) then
                    util1(i,j)=saln(i,j,k,1)
     &                        *scp2(i,j)*dp(i,j,k,1)*qonem*maskms(i,j)
                    util2(i,j)=scp2(i,j)*dp(i,j,k,1)*qonem*maskms(i,j)
                endif 
              enddo             !i
            enddo               !j
!$OMP END PARALLEL DO
            call xcsum(vol,util2,ipa)
            call xcsum(sal_initial(k),util1,ipa)
            sal_initial(k)=sal_initial(k)/vol
          enddo
          
! --- write initial factor
          if  (mnproc.eq.1) then
            write(lp,'(a15,1pe22.15)') 'pcp_fact    = ', pcp_fact
            write(lp,'(a15,1pe22.15)') 'sum_precip  = ', sum_precip
            write(lp,'(a15,1pe22.15)') 'ssh_initial = ', ssh_initial
            do k=1,kdm
             write(lp,'(a15,1x,i2.2, 2x,1pe22.15)') 'sal_initial  = ',
     &                                              k, sal_initial(k)    
            enddo
          endif
      elseif (pcpadj .and. .not.linit) then
!!Alex add precipitation adjustement
          if (mnproc.eq.1) print*,'Alex pcpadj and not linit'
! --- write initial factor
          if  (mnproc.eq.1) then
            write(lp,'(a15,1pe22.15)') 'pcp_fact    = ', pcp_fact
            write(lp,'(a15,1pe22.15)') 'sum_precip  = ', sum_precip
            write(lp,'(a15,1pe22.15)') 'ssh_initial = ', ssh_initial
            do k=1,kdm
             write(lp,'(a15,1x,i2.2, 2x,1pe22.15)') 'sal_initial  = ',
     &                                              k, sal_initial(k)    
            enddo
          endif
! --- get the value of pcp_facth for thermf
          pcp_facth=pcp_fact
          if (present(hycom_start_dtg)) then
! --- in a coupled setting, precip are adjusted in the coupler
              pcp_facth=1.d0
          endif 
          if (mnproc.eq.1) print*,'Alex pcp_facth = ',pcp_facth
      else    
          pcp_fact =1.d0
          pcp_facth=1.d0        
      endif
!!Alex add precipitation adjustment 
      
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
        if (mnproc.eq.1) then
          call mem_stat_print('mean_allocate:')
        endif !1st tile
        m=mod(nstep0  ,2)+1
        n=mod(nstep0+1,2)+1
        nstep=nstep0
        time =dtime0
        l0=1
        l1=2
        l2=3
        l3=4
        w0=0.0
        w1=0.0
        w2=0.0
        w3=0.0
c        call mean_zero(DBLE(time))
        call mean_zero(time)
        call momtum_hs(n,m)  !calculate srfhgt
        call mean_add(n, 0.5)
      endif
c
      nod=14 
      nstep=nstep1
      if (mnproc.eq.1) then
      write (lp,'(/2(a,f8.1),2(a,i9),a/)') 'model starts at day',
     &   time0,', goes to day',time0+day2-day1,'   (steps',nstep1,
     &   ' --',nstep2,')'
      open (unit=nod,file=trim(flnminp)//'summary_out',status='unknown')
      write(nod,'(/2(a,f8.1),2(a,i9),a/)') 'model starts at day',
     &   time0,', goes to day',time0+day2-day1,'   (steps',nstep1,
     &   ' --',nstep2,')'
      endif !1st tile
c
      timav=time0
      m=mod(nstep  ,2)+1
      n=mod(nstep+1,2)+1
c
           dpo = dp !for initial pipe_comparall
           call pipe_comparall(m,n, 'restrt, step')
c
      if (synflt) then
c ---   initialize synthetic floats/moorings
        call floats_init(m,n,time0)
        margin = nbdy
      endif
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
        if     (windf) then
          w0=-99.9
          w1=-99.0
          w2=0.0
          w3=0.0
!!Alex no file flux on restart
#ifdef NUOPC
          if (.not. (cpl_swflx  .and. cpl_lwmdnflx .and. cpl_lwmupflx
     &        .and.  cpl_taux   .and. cpl_tauy     .and. cpl_precip))
     &         then
              if (linit) then
                  call forfunh(dtime0)
              endif 
          endif
#else
          call forfunh(dtime0)
          
#endif 
        elseif (mslprf) then
          w0=-99.9
          w1=-99.0
          w2=0.0
          w3=0.0
          call forfunhz
          call forfunhp(dtime0)
        else
          w0=0.0
          w1=0.0
          w2=0.0
          w3=0.0
          call forfunhz
        endif
      endif
c
#if defined(STOKES)
c
c --- set up fields for Stokes Drift Velocities
c       (set to zero if stdflg==0)
c --- note that stokes_set calls stokes_forfun if necessary
c
      call stokes_set(dtime0)
#endif
c
#ifdef NUOPC    
c ---   jerlov used in call to swfrac_ij !!Alex mv here from forfun
       if     (jerlv0.gt.0) then
c ---     calculate jerlov water type,
c ---     which governs the penetration depth of shortwave radiation.
!$OMP     PARALLEL DO PRIVATE(j,i)
!$OMP&             SCHEDULE(STATIC,jblk)
          do j=1-nbdy,jj+nbdy
            do i=1-nbdy,ii+nbdy
c ---         map shallow depths to high jerlov numbers
              jerlov(i,j)=6-max(1,min(5,int(depths(i,j)/15.0)))
              jerlov(i,j)=max(jerlv0,jerlov(i,j))
            enddo
          enddo
        else
c ---     jerlv0= 0 uses an input annual/monthly kpar field
c ---     jerlv0=-1 uses an input annual/monthly chl  field
!$OMP     PARALLEL DO PRIVATE(j,i)
!$OMP&             SCHEDULE(STATIC,jblk)
          do j=1-nbdy,jj+nbdy
            do i=1-nbdy,ii+nbdy
              jerlov(i,j)=jerlv0
            enddo
          enddo
        endif

!$OMP     PARALLEL DO PRIVATE(j,i)
!$OMP&             SCHEDULE(STATIC,jblk)
          do j=1-nbdy,jj+nbdy
            do i=1-nbdy,ii+nbdy
              oftaux(i,j) = 0.0
              oftauy(i,j) = 0.0
              offlux(i,j) = 0.0
            enddo !i
          enddo !j
#endif
      
      if (jerlv0.le.0) then
c ---   read in kpar or chk field for 4 consecutive months
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

      if (present(hycom_start_dtg)) then
!!Alex update halo for calculation of  of seas surface slope for CICE (NUOPC)
          call xctilr(srfhgt( 1-nbdy,1-nbdy), 1, 1, 6, 6, halo_ps)
          call xctilr(u(    1-nbdy,1-nbdy,1,1),1,2*kk, 6,6, halo_uv) !! for export
          call xctilr(v(    1-nbdy,1-nbdy,1,1),1,2*kk, 6,6, halo_vv) !! for export
          call xctilr(ubavg(1-nbdy,1-nbdy,1  ),1,   2, 6,6, halo_uv) !! for export
          call xctilr(vbavg(1-nbdy,1-nbdy,1  ),1,   2, 6,6, halo_vv) !! for export
      endif 
      
c
c --- report initialization time.
c 
      call xctmrp
      call xctmr0(78)  !time since HYCOM_Init
c
      end subroutine HYCOM_Init

      subroutine HYCOM_Run(endtime,pointer_filename,restart_write)
c
c --- -------------------------
c --- execute a single timestep
c --- -------------------------
c
!!Alex add variable for CESM    
      real,         intent(in), optional :: endtime
      character*80, intent(in), optional :: pointer_filename
      logical,      intent(in), optional :: restart_write
      real          utot,vtot,usur1,usur2,vsur1,vsur2,uu,vv
      real          nstep2_cpl
      logical       restrt_cpl

      logical      lfatal
      integer      i,j,k,ktr,nm,margin
      character*80 flnmra,flnmrb
c      real*8       u3max,u3min
c      integer      iumax3,jumax3,iumin3,jumin3
c
      logical hycom_isnaninf  !function to detect NaN and Inf
c
      include 'stmt_fns.h'
c
      
c --- letter 'm' refers to mid-time level (example: dp(i,j,k,m) )
c --- letter 'n' refers to old and new time level
c
      end_of_run      = .false.   ! initialize on every entry
      end_of_run_cpl  = .false.   ! initialize on every entry
c
      m=mod(nstep  ,2)+1
      n=mod(nstep+1,2)+1
c
      nstep=nstep+1
      dtime=(nstep/nts_day)+mod(nstep,nts_day)*(baclin/86400.0d0)
c
      time  =dtime
      time_8=dtime  !'baroclinic' time for body force tides
      if     (tidflg.gt.0 .and.
     &        mod(dtime+dsmall,hours1).lt.dsmall2) then
        call tides_detide(n, .true.)  !update 49-hour filter
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
#ifdef NUOPC
          if (.not. (cpl_swflx  .and. cpl_lwmdnflx .and. cpl_lwmupflx
     &        .and.  cpl_taux   .and. cpl_tauy     .and. cpl_precip)) 
     &        then
               call forfunh(dtime)
          endif
#else
          call forfunh(dtime)

#endif 
      elseif (mslprf) then
c ---   pressure can be the only atmospheric forcing,
c ---   set weights and fields for high frequency pressure forcing.
        call forfunhp(dtime)
      endif
#if defined(STOKES)
c
c --- set weights and input fields for Stokes Drift
      if     (nsdzi.gt.0) then
        call stokes_forfun(dtime,n)
      endif !nsdzi
#endif
c
c --- set weights for quasi-hermite time interpolation for kpar.
      if     (jerlv0.le.0) then
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
         call pipe_comparall(n,m, 'ENTERm, step')
         call pipe_comparall(m,n, 'ENTERn, step')
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
            call pipe_comparall(m,n, 'thermi, step') !thermf_oi
      endif !icegln:icecpl:else
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
        call hybgen(m,n, hybraf)
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
      
! --- Check if beginning of year
      call forday (dtime,yrflag, iyear,iday,ihour)
      dtime_new=(nstep+1)/(86400.0d0/baclin)
      call forday (dtime_new,yrflag, iyear_new,iday,ihour)
      if (pcpadj.and.iyear_new.gt.iyear) then
!        if (pcpadj) then
          call precip_adjust(m,n)
          if (mnproc.eq.1) then
              write(lp,'(a15,1pe22.15)') 'pcp_fact    = ', pcp_fact
              write(lp,'(a15,1pe22.15)') 'sum_precip  = ', sum_precip
              write(lp,'(a15,1pe22.15)') 'ssh_initial = ', ssh_initial
              do k=1,kdm
               write(lp,'(a15,1x,i2.2, 2x,1pe22.15)') 'sal_initial  = ',
     &              k, sal_initial(k)  
              enddo
          endif 
      endif
! --- get the value of pcp_facth for thermf
      pcp_facth=pcp_fact
      if (present(endtime)) then
! --- in a coupled setting, precip are adjusted in the coupler
          pcp_facth=1.d0
      endif 

      if (present(endtime)) then 
!!Alex update halo before export 
          call xctilr(srfhgt( 1-nbdy,1-nbdy  ),1,   1, 6,6, halo_ps)
          call xctilr(u(    1-nbdy,1-nbdy,1,1),1,2*kk, 6,6, halo_uv) !! for export
          call xctilr(v(    1-nbdy,1-nbdy,1,1),1,2*kk, 6,6, halo_vv) !! for export
          call xctilr(ubavg(1-nbdy,1-nbdy,  1),1,   2, 6,6, halo_uv) !! for export
          call xctilr(vbavg(1-nbdy,1-nbdy,  1),1,   2, 6,6, halo_vv) !! for export
      endif 
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
        call forday(dtime,yrflag, iyear,jday,ihour)
        write(c_ydh,'('' ('',i4.4,''/'',i3.3,1x,i2.2,'')'')')
     &    iyear,jday,ihour
c
c ---   diagnose mean sea surface height
        if     (.not.allocated(sminy)) then
          allocate( sminy(1:jj), smaxy(1:jj) )
        endif
!$OMP   PARALLEL DO PRIVATE(j,i)
!$OMP&           SCHEDULE(STATIC,jblk)
        do j=1,jj
          sminy(j)= huge(sminy(j))
          smaxy(j)=-huge(smaxy(j))
          do i=1,ii
            if (SEA_P) then
              if     (tidflg.gt.0) then
                util2(i,j)=(srfhgt(i,j)/g)**2*scp2(i,j)
              endif
              util3(i,j)=srfhgt(i,j)*scp2(i,j)
              util4(i,j)=montg1(i,j)*scp2(i,j)
              sminy(j)=min(sminy(j),srfhgt(i,j))
              smaxy(j)=max(smaxy(j),srfhgt(i,j))
            endif !ip
          enddo !i
        enddo !j
!$OMP   END PARALLEL DO
        smin=minval(sminy(1:jj))
        smax=maxval(smaxy(1:jj))
        call xcminr(smin)
        call xcmaxr(smax)
        call xcsum( dsum, util3,ipa)
        call xcsum( dsmt, util4,ipa)

        sum=dsum
        smt=dsmt
c        write(6,'("sum,dsum=",G25.17,G25.17)')sum,dsum
c        print *,'srfhgt(1,1)=',srfhgt(1,1)
c        print *,'scp2(1,1)  =',scp2(1,1)
c       write(6,'(I3,I3,G25.17)')iumax3,jumax3,u3max
c       write(6,'(I3,I3,G25.17)')iumin3,jumin3,u3min
        if (mnproc.eq.1) then
        write (lp,'(i9,a,
     &              '' mean      SSH (mm):'',f8.2,
     &              ''  ('',1pe8.1,'' to '',e8.1,'')'')')
     &  nstep,c_ydh,
     &  sum/(area*thref*onemm),smin/(thref*onemm),smax/(thref*onemm)
        write (lp,'(i9,a,
     .              '' mean MontgPot (mm):'',f8.2)')
     .  nstep,c_ydh,
     .  smt/(area*thref*onemm)
        call flush(lp)
        write(nod,'(i9,a,
     &              '' mean      SSH (mm):'',f8.2,
     &              ''  ('',1pe8.1,'' to '',e8.1,'')'')')
     &  nstep,c_ydh,
     &  sum/(area*thref*onemm),smin/(thref*onemm),smax/(thref*onemm)
        write(nod,'(i9,a,
     .              '' mean MontgPot (mm):'',f8.2)')
     .  nstep,c_ydh,
     .  smt/(area*thref*onemm)
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
          call xcsum( dsms, util2,ipa)
          sms=dsms/area
c         
          call xctilr(u(    1-nbdy,1-nbdy,1,n),1,kk, 1,1, halo_uv)
          call xctilr(v(    1-nbdy,1-nbdy,1,n),1,kk, 1,1, halo_vv)
          call xctilr(ubavg(1-nbdy,1-nbdy,  n),1, 1, 1,1, halo_uv)
          call xctilr(vbavg(1-nbdy,1-nbdy,  n),1, 1, 1,1, halo_vv)
c         
#if defined(STOKES) 
          dskesa=0.0d0
#endif
          dskea =0.0d0
          do k=1,kk
!$OMP       PARALLEL DO PRIVATE(j,i,utotp,vtotp)
!$OMP&               SCHEDULE(STATIC,jblk)
            do j=1,jj
              do i=1,ii
                if (SEA_P) then
                  utotp=0.5*( u(i,  j,k,n)+ubavg(i,  j,n) +
     &                        u(i+1,j,k,n)+ubavg(i+1,j,n)  )
                  vtotp=0.5*( v(i,j,  k,n)+vbavg(i,j,  n) +
     &                        v(i,j+1,k,n)+vbavg(i,j+1,n)  )
#if defined(STOKES)
                  util6(i,j)=dp(i,j,k,n)*scp2(i,j)*
     &                      0.5*(1000.0+th3d(i,j,k,n)+thbase)*
     &                          (utotp**2+vtotp**2)
c
                  utotp=utotp+0.5*( usd(i,  j,k)+usdbavg(i,  j) +
     &                              usd(i+1,j,k)+usdbavg(i+1,j)  )
                  vtotp=vtotp+0.5*( vsd(i,j,  k)+vsdbavg(i,j) +
     &                              vsd(i,j+1,k)+vsdbavg(i,j+1)  )
#endif
                  util4(i,j)=dp(i,j,k,n)*scp2(i,j)*
     &                      0.5*(1000.0+th3d(i,j,k,n)+thbase)*
     &                          (utotp**2+vtotp**2)
                endif !ip
              enddo !i
            enddo !j
!$OMP       END PARALLEL DO
#if defined(STOKES)
            call xcsum(dskes, util6,ipa)
            dskesa=dskesa+dskes
#endif
            call xcsum(dske, util4,ipa)
            dskea=dskea+dske
          enddo !k
#if defined(STOKES)
          smx=(dskea-dskesa)/(area*onem)
#endif
          sum=dskea/(area*onem)
          if (mnproc.eq.1) then
#if defined(STOKES)
          write (lp,'(i9,a,
     &                  '' region-wide mean SKE:'',f20.10)')
     &        nstep,c_ydh,
     &          smx
#endif
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
        call forday(dtime,yrflag, iyear,jday,ihour)
        write(c_ydh,'('' ('',i4.4,''/'',i3.3,1x,i2.2,'')'')')
     &    iyear,jday,ihour
c
        if (thermo .or. sstflg.gt.0 .or. srelax) then
!$OMP     PARALLEL DO PRIVATE(j,i)
!$OMP&             SCHEDULE(STATIC,jblk)
          do j=1,jj
            do i=1,ii
              if (SEA_P) then
                util1(i,j)=buoflx(i,j)*scp2(i,j)
                util2(i,j)=bhtflx(i,j)*scp2(i,j)
              endif !ip
            enddo !i
          enddo !j
!$OMP     END PARALLEL DO
          call xcsum(dsum, util1,ipa)
          call xcsum(dsmt, util2,ipa)
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
!$OMP     PARALLEL DO PRIVATE(j,i)
!$OMP&             SCHEDULE(STATIC,jblk)
          do j=1,jj
            do i=1,ii
              if (SEA_P) then
                util1(i,j)=surflx(i,j)*scp2(i,j)
                util2(i,j)=mixflx(i,j)*scp2(i,j)
                util3(i,j)=sstflx(i,j)*scp2(i,j)
              endif !ip
            enddo !i
          enddo !j
!$OMP     END PARALLEL DO
          call xcsum(dsum, util1,ipa)
          call xcsum(dsmt, util2,ipa)
          call xcsum(d3,   util3,ipa)
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
!$OMP     PARALLEL DO PRIVATE(j,i)
!$OMP&             SCHEDULE(STATIC,jblk)
          do j=1,jj
            do i=1,ii
              if (SEA_P) then
                util1(i,j)=salflx(i,j)*scp2(i,j)/saln(i,j,1,n)
                util2(i,j)=sssflx(i,j)*scp2(i,j)/saln(i,j,1,n)
              endif !ip
            enddo !i
          enddo !j
!$OMP     END PARALLEL DO
          call xcsum(dsms, util1,ipa)
          call xcsum(dsum, util2,ipa)
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
        if (iceflg.ne.0) then  ! basin-wide ice
c ---     use CICE fields for ice statistics when available
          if     (iceflg.eq.1) then
            si_c(:,:) = covice(:,:)
            si_h(:,:) = thkice(:,:)
            si_t(:,:) = temice(:,:)
          endif
!$OMP     PARALLEL DO PRIVATE(j,i)
!$OMP&             SCHEDULE(STATIC,jblk)
          do j=1,jj
            do i=1,ii
              if (SEA_P) then
                if     (si_c(i,j).ne.0.0) then
                  util2(i,j)=si_c(i,j)*          scp2(i,j)
                  util3(i,j)=          si_h(i,j)*scp2(i,j)
                  util4(i,j)=si_c(i,j)*si_t(i,j)*scp2(i,j)
                else
                  util2(i,j)=0.0
                  util3(i,j)=0.0
                  util4(i,j)=0.0
                endif
              endif !ip
            enddo !i
          enddo !j
!$OMP     END PARALLEL DO
          call xcsum(d2, util2,ipa)
          call xcsum(d3, util3,ipa)
          call xcsum(d4, util4,ipa)
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
!$OMP     PARALLEL DO PRIVATE(j,i)
!$OMP&             SCHEDULE(STATIC,jblk)
          do j=1,jj
            do i=1,ii
              if (SEA_P) then
                if     (si_c(i,j).ne.0.0 .and.
     &                  plat(i,j).lt.0.0      ) then
                  util2(i,j)=si_c(i,j)*          scp2(i,j)
                  util3(i,j)=          si_h(i,j)*scp2(i,j)
                  util4(i,j)=si_c(i,j)*si_t(i,j)*scp2(i,j)
                else
                  util2(i,j)=0.0
                  util3(i,j)=0.0
                  util4(i,j)=0.0
                endif
              endif !ip
            enddo !i
          enddo !j
!$OMP     END PARALLEL DO
          call xcsum(d2, util2,ipa)
          call xcsum(d3, util3,ipa)
          call xcsum(d4, util4,ipa)
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
        if     (icegln .and. icmflg.eq.3) then
c
c ---     HYCOM covice when relaxing to CICE si_
c
!$OMP     PARALLEL DO PRIVATE(j,i)
!$OMP&             SCHEDULE(STATIC,jblk)
          do j=1,jj
            do i=1,ii
              if (SEA_P) then
                util1(i,j) = scp2(i,j)*covice(i,j)
                if     (plat(i,j).lt.0.0) then
                  util2(i,j) = util1(i,j)
                else
                  util2(i,j) = 0.0
                endif
              endif !ip
            enddo !i
          enddo !j
!$OMP     END PARALLEL DO
          call xcsum(dsmt, util1,ipa)
          call xcsum(dsms, util2,ipa)
          smt=dsmt/area * 100.0
          sms=dsms/area * 100.0
          if (mnproc.eq.1) then
          write (lp,'(i9,a,
     &              '' mean   covice pcen:'',f9.3,
     &                            ''  S.H:'',f7.3,
     &                            ''  N.H:'',f7.3)')
     &    nstep,c_ydh,
     &    smt,sms,smt-sms
          call flush(lp)
          write(nod,'(i9,a,
     &              '' mean   covice pcen:'',f9.3,
     &                            ''  S.H:'',f7.3,
     &                            ''  N.H:'',f7.3)')
     &    nstep,c_ydh,
     &    smt,sms,smt-sms
          call flush(nod)
          endif !1st tile
        endif  !HYCOM covice
c
!$OMP   PARALLEL DO PRIVATE(j,i)
!$OMP&           SCHEDULE(STATIC,jblk)
        do j=1,jj
          do i=1,ii
            if (SEA_P) then
              util1(i,j)=dpmixl(i,j,n)*scp2(i,j)
              util2(i,j)=dpmixl(i,j,n)*scp2(i,j)*tmix(i,j)
              util3(i,j)=dpmixl(i,j,n)*scp2(i,j)*smix(i,j)
              util4(i,j)=temp(i,j,1,n)*scp2(i,j)
              util5(i,j)=saln(i,j,1,n)*scp2(i,j)
            endif !ip
          enddo !i
        enddo !j
!$OMP   END PARALLEL DO
        call xcsum(dsum, util1,ipa)
        call xcsum(dsmt, util2,ipa)
        call xcsum(dsms, util3,ipa)
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
        call xcsum(dsmt, util4,ipa)
        call xcsum(dsms, util5,ipa)
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
!$OMP     PARALLEL DO PRIVATE(j,i)
!$OMP&             SCHEDULE(STATIC,jblk)
          do j=1,jj
            do i=1,ii
              if (SEA_P) then
                if     (relaxf .and. sstflg.le.1) then
                  util1(i,j)=scp2(i,j)*
     &                (twall(i,j,1,lc0)*wc0+twall(i,j,1,lc1)*wc1
     &                +twall(i,j,1,lc2)*wc2+twall(i,j,1,lc3)*wc3)
                elseif (natm.eq.2) then !hf synoptic observed sst
                  util1(i,j)=scp2(i,j)*
     &                (seatmp(i,j,l0)*w0+seatmp(i,j,l1)*w1)
                else !monthly synoptic observed sst
                  util1(i,j)=scp2(i,j)*
     &                (seatmp(i,j,l0)*w0+seatmp(i,j,l1)*w1
     &                +seatmp(i,j,l2)*w2+seatmp(i,j,l3)*w3)
                endif
                if     (relaxf) then !sss
                  util2(i,j)=scp2(i,j)*
     &                  (swall(i,j,1,lc0)*wc0+swall(i,j,1,lc1)*wc1
     &                  +swall(i,j,1,lc2)*wc2+swall(i,j,1,lc3)*wc3)
                elseif (natm.eq.2) then !hf synoptic observed surface temp
                  util1(i,j)=scp2(i,j)*
     &                (surtmp(i,j,l0)*w0+surtmp(i,j,l1)*w1)
                else !monthly synoptic observed surface temperature
                  util1(i,j)=scp2(i,j)*
     &                (surtmp(i,j,l0)*w0+surtmp(i,j,l1)*w1
     &                +surtmp(i,j,l2)*w2+surtmp(i,j,l3)*w3)
                endif
              endif !ip
            enddo !i
          enddo !j
!$OMP     END PARALLEL DO
          call xcsum(dsmt, util1,ipa)
          call xcsum(dsms, util2,ipa)
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
#if defined(STOKES)
        dskesa=0.0d0
#endif
        do k=1,kk
!$OMP     PARALLEL DO PRIVATE(j,i,utotp,vtotp)
!$OMP&             SCHEDULE(STATIC,jblk)
          do j=1,jj
            do i=1,ii
              if (SEA_P) then
                  utotp=0.5*( u(i,  j,k,n)+ubavg(i,  j,n) +
     &                        u(i+1,j,k,n)+ubavg(i+1,j,n)  )
                  vtotp=0.5*( v(i,j,  k,n)+vbavg(i,j,  n) +
     &                        v(i,j+1,k,n)+vbavg(i,j+1,n)  )
#if defined(STOKES)
                  util6(i,j)=dp(i,j,k,n)*scp2(i,j)*
     &                      0.5*(1000.0+th3d(i,j,k,n)+thbase)*
     &                          (utotp**2+vtotp**2)
c
                  utotp=utotp+0.5*( usd(i,  j,k)+usdbavg(i,  j) +
     &                              usd(i+1,j,k)+usdbavg(i+1,j)  )
                  vtotp=vtotp+0.5*( vsd(i,j,  k)+vsdbavg(i,j) +
     &                              vsd(i,j+1,k)+vsdbavg(i,j+1)  )
#endif
                util4(i,j)=dp(i,j,k,n)*scp2(i,j)*
     &                    0.5*(1000.0+th3d(i,j,k,n)+thbase)*
     &                        (utotp**2+vtotp**2)
c
                util1(i,j)=dp(i,j,k,n)*scp2(i,j)
                util2(i,j)=dp(i,j,k,n)*scp2(i,j)*temp(i,j,k,n)
                util3(i,j)=dp(i,j,k,n)*scp2(i,j)*saln(i,j,k,n)
                util5(i,j)=dp(i,j,k,n)*scp2(i,j)*th3d(i,j,k,n)
              endif !ip
            enddo !i
          enddo !j
!$OMP     END PARALLEL DO
#if defined(STOKES)
          call xcsum(dskes, util6,ipa)
          dskesa=dskesa+dskes
#endif
          call xcsum(dsum, util1,ipa)
          call xcsum(dsmt, util2,ipa)
          call xcsum(dsms, util3,ipa)
          call xcsum(dsmr, util5,ipa)
          call xcsum(dske, util4,ipa)
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
#if defined(STOKES)
        smx=(dskea-dskesa)/(area*onem)
#endif
        sum=dskea/(area*onem)
        smt=dsmta/dsuma
        sms=dsmsa/dsuma
        smr=dsmra/dsuma
        if (mnproc.eq.1) then
#if defined(STOKES)
        write (lp,'(i9,a,
     &                '' region-wide mean Stokes K.E.:'',f20.10)')
     &      nstep,c_ydh,
     &        smx
#endif
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
#if defined(STOKES)
        write (nod,'(i9,a,
     &                '' region-wide mean Stokes K.E.:'',f20.10)')
     &      nstep,c_ydh,
     &        smx
#endif
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
!$OMP       PARALLEL DO PRIVATE(j,i)
!$OMP&               SCHEDULE(STATIC,jblk)
            do j=1,jj
              do i=1,ii
                if (SEA_P) then
                  util2(i,j)=dp(i,j,k,n)*scp2(i,j)*tracer(i,j,k,n,ktr)
                endif !ip
              enddo !i
            enddo !j
!$OMP       END PARALLEL DO
            call xcsum(dsmt, util2,ipa)
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
          call forday(time_ave,yrflag, iyear,jday,ihour)
          call mean_archiv(n, iyear,jday,ihour)
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
        call forday(dtime,yrflag, iyear,jday,ihour)
c
!$OMP   PARALLEL DO PRIVATE(j,k,i)
!$OMP&           SCHEDULE(STATIC,jblk)
        do j=1,jj
          if (isopyc .or. mxlkrt) then
            do i=1,ii
              if (SEA_U) then
                umix(i,j)=u(i,j,1,n)
              endif !iu
              if (SEA_V) then
                vmix(i,j)=v(i,j,1,n)
              endif !iv
            enddo !i
          endif !isopyc .or. mxlkrt
          if (histry) then
            do k= 1,kk
              do i=1,ii
                if (SEA_P) then
c ---             convert diapycnal thickness changes into 
c ---             actual interface fluxes
                  if (k.gt.1) then
                    diaflx(i,j,k)=diaflx(i,j,k)/(2.*onem) +
     &                            diaflx(i,j,k-1)
                  else
                    diaflx(i,j,k)=diaflx(i,j,k)/(2.*onem)
                  endif
                endif !ip
              enddo !i
            enddo !k
          endif !histry
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
!$OMP     PARALLEL DO PRIVATE(j,k,i)
!$OMP&             SCHEDULE(STATIC,jblk)
          do j=1,jj
            do k= 1,kk
              do i=1,ii
                if (SEA_P) then
                  diaflx(i,j,k)=0.0
                endif !ip
              enddo !i
            enddo !k
          enddo !j
!$OMP     END PARALLEL DO
c
          timav=time
        endif
        call xctmr1(53)
      endif  ! histry.or.hiprof.or.hitile.or.hisurf.or.lfatal
c
c

!!Alex end of coupling sequence
      ! regular end-of-run condition
      end_of_run = nstep.ge.nstep2

      ! stepable end-of-run condition
      if (present(endtime)) then
        ! endtime present means that "end_of_run_cpl" needs to be set correctly
        nstep2_cpl=nint(endtime*(86400.0d0/baclin))

        if(mnproc.eq.1) print *,"mod_hycom,nstep,nstep2,nstep2_cpl =",
     &    nstep,nstep2,nstep2_cpl
        if(mnproc.eq.1) print *,"endtime =",
     &    endtime

        if(nstep.ge.nstep2_cpl) then
          end_of_run_cpl = .true.
        else
          end_of_run_cpl = .false.
        endif
      endif

      if (present(restart_write)) then
          restrt_cpl = restart_write .and. end_of_run_cpl
      else
          restrt_cpl = .false.
      endif 
      if (restrt .or. restrt_cpl) then !!Alex
        call xctmr0(51)
c
c ---   output to restart and flux statitics files
c
        if (mxlkpp) then
!$OMP     PARALLEL DO PRIVATE(j,i)
!$OMP&             SCHEDULE(STATIC,jblk)
          do j=1,jj
            do i=1,ii
              if (SEA_P) then
                dpmixl(i,j,m) = dpmixl(i,j,n)
              endif !ip
            enddo !i
          enddo !j
!$OMP     END PARALLEL DO
        endif
c
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
!!Alex handle restart for CESM rpointer filename
c
c --- get the restart filename
c
        if (restrt_cpl) then
          write(flnmra,'(a,i4.4,a,i3.3,a,i2.2)') 
     &                 'restart_', iyear,'-',jday,'-',ihour
          open(1,file=trim(pointer_filename),form='formatted',
     &            status='unknown')
          write(1,'(a)')trim(flnmra)
          close(1)
          flnmrb = trim(flnmra)
        else           
          flnmra = flnmrso  !.a extension added by restart_out
          flnmrb = flnmrso  !.b extension added by restart_out
        endif 
c
        call restart_out(nstep,dtime, flnmra,flnmrb, nstep.ge.nstep2,
     &                   restrt_cpl ) !!Alex
c
c ---   set layer thickness (incl.bottom pressure) at u,v points
c ---   needed because restart_out may have modified dp
c
        call dpthuv
c
        call xctilr(dp(1-nbdy,1-nbdy,1,1),1,2*kk, nbdy,nbdy, halo_ps)
        call xctilr(dpmixl(1-nbdy,1-nbdy,1),1,2,  nbdy,nbdy, halo_ps)
c
        margin = nbdy
c
        nstep = nstep+1  !for pipe_compare
        do nm=1,2
!$OMP     PARALLEL DO PRIVATE(j,k,i)
!$OMP&             SCHEDULE(STATIC,jblk)
          do j=1-margin,jj+margin
            if     (nm.eq.mod(nstep+1,2)+1) then
              do i=1-margin,ii+margin
                if (SEA_P) then
                  dpbl( i,j)=dpmixl(i,j,nm)
                endif !ip
              enddo !i
            endif !nm
            do i=1-margin,ii+margin
              if (SEA_P) then
                p(i,j,1)=0.0
                do k=1,kk
                  p(i,j,k+1)=p(i,j,k)+dp(i,j,k,nm)
                enddo !k
              endif !ip
            enddo !i
          enddo !j
!$OMP     END PARALLEL DO
c
          call dpudpv(dpu(1-nbdy,1-nbdy,1,nm),
     &                dpv(1-nbdy,1-nbdy,1,nm),
     &                p,depthu,depthv, margin,max(0,margin-1))
c            
        enddo  !nm=1,2
        nstep = nstep-1  !restore
c
c ---   update montg to preserve bit for bit reproducability on restart
        m=mod(nstep  ,2)+1
        n=mod(nstep+1,2)+1
        if     (meanfq.ne.0.0) then
c ---     assume meanfq not changed to zero when rerun from restart
          call momtum_hs(n,m)
          surflx(:,:) = 0.0
          salflx(:,:) = 0.0
          if     (histmn) then
            call mean_zero(dtime)
            call mean_add(n, 0.5)
          endif  ! histmn
        endif !meanfq
        call momtum_hs(n,m)
c
        call xctmr1(51)
      endif                     ! restrt
c

      if     (histry .or. hiprof .or. hitile .or. hisurf) then
        if (mnproc.eq.1) then
        write (lp,'(a,i9,a,f13.5,a)')
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
c
c --- at end: output float restart file 
c
      if     (synflt .and. end_of_run) then
        call floats_restart
      endif !synflt+end_of_run

      end subroutine HYCOM_Run

      subroutine HYCOM_Final
c
c --- end of the run.
      if (mnproc.eq.1) then
      write(nod,'(a)') 'normal stop'
      call flush(nod)
      endif !1st tile
      call xcstop('(normal)')  !calls xctmrp
             stop '(normal)'   !won't get here
      end subroutine HYCOM_Final

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
c> Nov. 2011 - can now start from climatology when yrflag=3
c> Jan. 2012 - smooth imported ice drift and exported ocean currents
c> Jan. 2012 - added thkcdw
c> Aug. 2012 - use CICE fields for ice statistics when available
c> Aug. 2012 - call pipe_init after blkdat
c> Dec. 2012 - initialize l0-3 and w0-3 before first call to momtum_hs
c> Jan. 2013 - replace dragrh with drgten
c> Apr. 2013 - added incupd_si_c
c> Aug. 2013 - optionally added mod_stokes
c> Nov. 2013 - added jerlv0=-1 and forfunc
c> Jan. 2014 - added mslprf and forfunhp
c> Apr. 2014 - added ice shelf logic (ishlf)
c> Apr. 2014 - replace ip with ipa for mass sums, reduces need for jja
c> May  2014 - use land/sea masks (e.g. ip) to skip land
c> Feb. 2015 - removed dtime0 from calculation of dtime
c> Feb. 2015 - fixed issues with bit for bit reproducability on restart
