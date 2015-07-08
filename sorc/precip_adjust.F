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
      subroutine precip_adjust(m,n)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      implicit none
c
c
      integer m,n
      integer i,j,k
! --- ----------------------------------------------------------
! --- Calculate a precip_fact to adjust the water balance 
! --- (to ensure fix volume on global scale)
! --- Performed once at end-of-year (eoy)
! --- based on precip_adjustment of POP
! --- 
! --- ----------------------------------------------------------

      real*8                    ssh_final ! final  mean ssh
      real*8, dimension(kdm) :: sal_final ! final  mean salinity profile
      real*8                    ann_avg_precip              

      real*8, parameter :: 
     &    precip_mean = 3.4e-5,
     &    ocn_ref_salinity = 34.7d0

      real*8, dimension(kdm) :: vol ! mean vol of each layer

      real*8 ::
     &    secpyr,sal_tendency,vol_glob,precip_tav,fw_tendency,area_ms
!-----------------------------------------------------------------------
!
!  Perform end of year adjustment calculations
!
!-----------------------------------------------------------------------

      
      call xctilr(saln(  1-nbdy,1-nbdy,1,n),1,  kk, 6,6, halo_ps)
      call xctilr(  dp(  1-nbdy,1-nbdy,1,n),1,  kk, 6,6, halo_ps)
      call xctilr(srfhgt(  1-nbdy,1-nbdy),1,  1, 6,6, halo_ps)
      
      if      (yrflag.eq.0) then
          secpyr=360.00d0*86400.0d0
      elseif (yrflag.lt.3) then
          secpyr=366.00d0*86400.0d0
      elseif (yrflag.ge.3) then
          secpyr=365.25d0*86400.0d0
      endif
      
! --- Compute the surface volume-averaged salinity and
! --- average surface height (for variable thickness sfc layer)
! --- note that it is evaluated at the current time level.

! --- Compute the averaged sea surface height.

      util1(:,:) = 0.d0
      util2(:,:) = 0.d0
!$OMP PARALLEL DO PRIVATE(j,i)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1,jj
        do i=1,ii
          if (SEA_P) then
              util1(i,j) = srfhgt(i,j)* scp2(i,j)*maskms(i,j)*qthref/g
              util2(i,j) = scp2(i,j)*maskms(i,j)
          endif                 
        enddo                   !i  
      enddo                     !j
!$OMP END PARALLEL DO

      call xcsum(area_ms,util2,ipa) 
      call xcsum(ssh_final,util1,ipa)
      ssh_final=ssh_final/ area_ms /secpyr

! --- Compute the volume-averaged salinity for each layer.
      do k=1,kdm          
!$OMP PARALLEL DO PRIVATE(j,i)
!$OMP&         SCHEDULE(STATIC,jblk)
        do j=1,jj
          do i=1,ii
            if (SEA_P) then
                util1(i,j)=saln(i,j,k,n)
     &                    *scp2(i,j)*dp(i,j,k,n)*qonem*maskms(i,j)
                util2(i,j)=scp2(i,j)*dp(i,j,k,n)*qonem*maskms(i,j)
            endif
          enddo                 !i
        enddo                   !j
!$OMP END PARALLEL DO

        call xcsum(vol(k),util2,ipa)
        call xcsum(sal_final(k),util1,ipa)
        if (vol(k).gt.0d0) then
           sal_final(k)=sal_final(k)/max(vol(k),epsil)
        endif
      enddo 


!-----------------------------------------------------------------------
!
!  compute tendency of salinity for eack "k" layer
!
!-----------------------------------------------------------------------

      do k=1,kdm
        sal_initial(k) = (sal_final(k) - sal_initial(k))/secpyr
      enddo

!-----------------------------------------------------------------------
!
!  form the global volume-averaged tendency to be used in "precip_fact"
!  computation
!
!-----------------------------------------------------------------------

      sal_tendency = 0.d0
      do k=1,kdm
        vol_glob=vol_glob+vol(k)
        sal_tendency = sal_tendency + vol(k)*sal_initial(k)
      enddo

      sal_tendency = sal_tendency/vol_glob

      if (mnproc.eq.1) then
          write (lp,'(a58,1pe22.15)') 
     &    ' precip_adjustment: volume-averaged salinity tendency = ', 
     &                                              sal_tendency
      endif
      
!-----------------------------------------------------------------------
!
!  convert "sal_tendency" from (psu/s) to -(kg/m^2/s). 
!  assumes density of fresh water = 1000 kg/m**3
!
!-----------------------------------------------------------------------

      sal_tendency = - sal_tendency*vol_glob*qthref
     &               / area_ms / ocn_ref_salinity

!-----------------------------------------------------------------------
!
!  compute annual change in mass due to freshwater flux (kg/m^2/s)
!
!-----------------------------------------------------------------------

      fw_tendency = ssh_final - ssh_initial

      if (mnproc.eq.1) then
          write (lp,'(a22)') ' precip_adjustment: '
          write (lp,'(a28,1pe22.15)') '   sal_tendency (kg/m^2/s): ', 
     &                                    sal_tendency
          write (lp,'(a28,1pe22.15)') '   fw_tendency (kg/m^2/s):  ', 
     &                                    fw_tendency
      endif

!-----------------------------------------------------------------------
!
!  change "precip_fact" based on tendency of freshwater and previous
!  amount of precipitation 
!
!-----------------------------------------------------------------------
      precip_tav = precip_mean
!      precip_tav = ann_avg_precip/precip_fact

      pcp_fact = pcp_fact - (sal_tendency+fw_tendency)/precip_tav

      if (mnproc.eq.1) then
          write(lp,*) ' Changed precipitation factor to ',pcp_fact
      endif


      sal_initial = sal_final
      ssh_initial = ssh_final


      end subroutine precip_adjust
