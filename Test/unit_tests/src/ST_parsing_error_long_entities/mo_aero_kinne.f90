#ifdef __xlC__
@PROCESS STRICT
#endif
!>
!! @par Copyright
!! This code is subject to the MPI-M-Software - License - Agreement in it's most recent form.
!! Please see URL http://www.mpimet.mpg.de/en/science/models/model-distribution.html and the
!! file COPYING in the root of the source tree for this code.
!! Where software is supplied by third parties, it is indicated in the headers of the routines.
!!
MODULE mo_aero_kinne
!-------------------------------------------------------------------------
!
!    Sebastian Rast, Stefan Kinne, MPI Met, Hamburg, november 2008
!
!-------------------------------------------------------------------------
!BOP
!
! !MODULE: mo_aero_kinne
!
! !DESCRIPTION:
! Introduction of optical properties of aerosols from a climatology
!
! !REVISION HISTORY:
! original source by J.S.Rast, S. Kinne (2008-11-06)
!
! !USES:
  USE mo_kind,                 ONLY: wp
  USE mo_decomposition,        ONLY: ldc=>local_decomposition, gdc=>global_decomposition
  USE mo_rrtm_params,          ONLY: nbndlw
  USE mo_exception,            ONLY: finish

  IMPLICIT NONE

  PRIVATE
  PUBLIC                           :: su_aero_kinne, read_aero_kinne, &
                                      set_aop_kinne, cleanup_aero_kinne
!, discf_kinne, aop_lw_kinne, &

! !LOCAL VARIABLES

  REAL(wp), ALLOCATABLE            :: aod_c_s(:,:,:,:), aod_f_s(:,:,:,:), &
                                      ssa_c_s(:,:,:,:), ssa_f_s(:,:,:,:), &
                                      asy_c_s(:,:,:,:), asy_f_s(:,:,:,:), &
                                      aod_c_f(:,:,:,:),                   &
                                      ssa_c_f(:,:,:,:),                   &
                                      asy_c_f(:,:,:,:),                   &
                                      z_km_aer_c_mo(:,:,:,:), &
                                      z_km_aer_f_mo(:,:,:,:)
!  REAL(wp), ALLOCATABLE            :: discf(:,:), aero_strat(:,:)
  INTEGER, PARAMETER               :: lev_clim=40
  REAL(wp)                         :: dz_clim, rdz_clim
  LOGICAL                          :: laero_set=.false.

CONTAINS
!EOP
!-------------------------------------------------------------------------
!BOC
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: su_aero_kinne
!
! !SUBROUTINE INTERFACE:
SUBROUTINE su_aero_kinne(nb_sw)
!
! !DESCRIPTION:
! set up memory for optical aerosol parameters for new aerosol climatology
! compiled by S. Kinne, called by setrad.
! This is a modified version for the radiation only program
!
! !REVISION HISTORY:
! original source by J.S. Rast (2009-11-30), 
! modified for radiation_prog by J.S. Rast (2011-07-04)
!
! !USES:
! !INPUT PARAMETERS
  INTEGER, INTENT(in)            :: nb_sw

! !LOCAL VARIABLES

  INTEGER, PARAMETER             :: ntime=1

! allocate memory for optical properties
  ALLOCATE(aod_c_s(ldc%nproma,nb_sw,ldc%ngpblks,ntime))
  ALLOCATE(aod_f_s(ldc%nproma,nb_sw,ldc%ngpblks,ntime))
  ALLOCATE(ssa_c_s(ldc%nproma,nb_sw,ldc%ngpblks,ntime))
  ALLOCATE(ssa_f_s(ldc%nproma,nb_sw,ldc%ngpblks,ntime))
  ALLOCATE(asy_c_s(ldc%nproma,nb_sw,ldc%ngpblks,ntime))
  ALLOCATE(asy_f_s(ldc%nproma,nb_sw,ldc%ngpblks,ntime))
  ALLOCATE(aod_c_f(ldc%nproma,nbndlw,ldc%ngpblks,ntime))
  ALLOCATE(ssa_c_f(ldc%nproma,nbndlw,ldc%ngpblks,ntime))
  ALLOCATE(asy_c_f(ldc%nproma,nbndlw,ldc%ngpblks,ntime))
  ALLOCATE(z_km_aer_c_mo(ldc%nproma,lev_clim,ldc%ngpblks,ntime))
  ALLOCATE(z_km_aer_f_mo(ldc%nproma,lev_clim,ldc%ngpblks,ntime))
END SUBROUTINE su_aero_kinne
!EOP
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: read_aero_kinne
!
!SUBROUTINE interface
!!$SUBROUTINE read_aero_kinne(rad_step_1, rad_step_2, nb_sw)
SUBROUTINE read_aero_kinne(nb_sw)
!
! !DESCRIPTION:
! This routine reads the aerosol data on a yearly basis 
!
! !REVISION HISTORY:
! original source by J.S. Rast (2010-01-08)
! 
! !USES:

!!$  USE mo_time_conversion,    ONLY: time_days
!!$  USE mo_time_control,       ONLY: get_date_components
  
  !INPUT PARMETES
!!$  TYPE(time_days), INTENT(in)   :: rad_step_1, rad_step_2
  INTEGER, INTENT(in)           :: nb_sw

  !LOCAL VARIABLES
!!$  INTEGER                       :: icurrentyear, inextyear
!!$  INTEGER                       :: zyrm1, zyr, zyrp1
!!$  LOGICAL                       :: lnewyear
  CHARACTER(len=20)             :: cfname_base,cyr
  CHARACTER(len=25)             :: cfname

!!$  CALL get_date_components (rad_step_1, year=icurrentyear)
!!$  CALL get_date_components (rad_step_2, year=inextyear)
!!$  lnewyear=icurrentyear/=inextyear

!!$  IF (.NOT.lnewyear .AND. laero_set) return
!!$  zyrm1=inextyear-1
!!$  zyr=inextyear
!!$  zyrp1=inextyear+1
! coarse mode aerosol, solar radiation
  cfname_base='aero_coarse'
  WRITE(cyr,*) 9999
  cfname=TRIM(cfname_base)//'_'//TRIM(ADJUSTL(cyr))//'.nc'
  CALL aero_read_opt ( &
    ldc%nproma,           ldc%ngpblks,              nb_sw,              &
    lev_clim,             aod_c_s(:,:,:,1),         'aod',              &
    ssa_c_s(:,:,:,1),     'ssa',                    asy_c_s(:,:,:,1),   &
    'asy',                z_km_aer_c_mo(:,:,:,1),   'z_aer_coarse_mo',  &
    dz_clim,              'delta_z',                1,                  &
    1,                    cfname                                        )
  rdz_clim=1.d0/dz_clim
! fine mode aerosol, solar radiation
  cfname_base='aero_fine'
  WRITE(cyr,*) 9999
  cfname=TRIM(cfname_base)//'_'//TRIM(ADJUSTL(cyr))//'.nc'
  CALL aero_read_opt ( &
    ldc%nproma,           ldc%ngpblks,              nb_sw,              &
    lev_clim,             aod_f_s(:,:,:,1),         'aod',              &
    ssa_f_s(:,:,:,1),     'ssa',                    asy_f_s(:,:,:,1),   &
    'asy',                z_km_aer_f_mo(:,:,:,1),   'z_aer_fine_mo',    &
    dz_clim,              'delta_z',                1,                  &
    1,                    cfname                                        )
! (coarse mode) aerosol, far infrared
  cfname_base='aero_farir'
  WRITE(cyr,*) 9999
  cfname=TRIM(cfname_base)//'_'//TRIM(ADJUSTL(cyr))//'.nc'
  CALL aero_read_opt ( &
    ldc%nproma,           ldc%ngpblks,              nbndlw,             &
    lev_clim,             aod_c_f(:,:,:,1),         'aod',              &
    ssa_c_f(:,:,:,1),     'ssa',                    asy_c_f(:,:,:,1),   &
    'asy',                z_km_aer_c_mo(:,:,:,1),   'z_aer_coarse_mo',  &
    dz_clim,              'delta_z',                1,                  &
    1,                    cfname                                        )
  laero_set=.true.
END SUBROUTINE read_aero_kinne
!EOP
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: set_aop_kinne
!
!SUBROUTINE interface
SUBROUTINE set_aop_kinne ( &
          & kproma,                 kbdim,              klev,             &
          & krow,                   nb_lw,              nb_sw,            &
          & paer_tau_lw_vr,         paer_tau_sw_vr,     paer_piz_sw_vr,   &
          & paer_cg_sw_vr,          ppd_hl,             pp_fl,            &
          & tk_fl,                  pgeom1 )
!
! !DESCRIPTION:
! set aerosol optical properties for all wave length bands (solar and IR)
! in the case of the climatology of optical properties compiled by S.Kinne.
! The height profile is taken into account.
! Adjusted for use in radiation_prog. ATTENTION: pgeom1 is not set!!!!
!
! !REVISION HISTORY:
! original source by J.S. Rast (2009-11-03)
! 
! !USES:
  USE mo_interpo,       ONLY: nmw1_m, nmw2_m, wgt1_m, wgt2_m
  USE mo_physical_constants,     ONLY: grav, rgrav, rd
  USE mo_memory_g3b,    ONLY: geosp

! !INPUT PARAMETERS
  INTEGER,INTENT(in)  :: kproma, &! actual block length
                         kbdim,  &! maximum block length
                         krow,   &! block index
                         klev,   &! number of vertical levels
                         nb_lw,  &! number of wave length bands (far IR)
                         nb_sw    ! number of wave length bands (solar)
  REAL(wp),INTENT(in) :: ppd_hl(kbdim,klev)  ,& ! layer pressure thickness 
                         pp_fl(kbdim,klev)   ,& ! pressure at "full levels"
                         tk_fl(kbdim,klev)   ,& ! temperature at "full lev."
                         pgeom1(kbdim,klev)     ! geopotential above ground
! !OUTPUT PARAMETERS
  REAL(wp),INTENT(out),DIMENSION(kbdim,klev,nb_lw):: &
   paer_tau_lw_vr      !aerosol optical depth (far IR)
  REAL(wp),INTENT(out),DIMENSION(kbdim,klev,nb_sw):: &
   paer_tau_sw_vr,   & !aerosol optical depth (solar), sum_i(tau_i)
   paer_piz_sw_vr,   & !weighted sum of single scattering albedos, 
                       !sum_i(tau_i*omega_i)
   paer_cg_sw_vr       !weighted sum of asymmetry factors, 
                       !sum_i(tau_i*omega_i*g_i)

! !LOCAL VARIABLES
  
  INTEGER                     :: jl,jk,jwl
  REAL(wp), DIMENSION(kbdim,klev)   :: zh, &    ! altitude above surface
                                       zdeltag, & ! layer thickness
                                       zh_vr, &
                                       zdeltag_vr
  REAL(wp), DIMENSION(kbdim)        :: zq_int ! integral height profile
  REAL(wp), DIMENSION(kbdim,nb_lw)  :: zs_i
  REAL(wp), DIMENSION(kbdim,nb_sw)  :: zt_c, zt_f, &
                                       zs_c, zs_f, &
                                       zg_c, zg_f, & ! time interpolated
                                       ! aod, ssa ,ssa*asy 
                                       ! (coarse (c), fine natural (n), 
                                       !  fine anthropogenic (a))
                                       ztaua_c,ztaua_f ! optical depths
                                       ! at various altitudes
  REAL(wp), DIMENSION(kbdim,klev)   :: zq_aod_c, zq_aod_f ! altitude profile
                                       ! on echam grid (coarse and fine mode)
  INTEGER, DIMENSION(kbdim)         :: kindex ! index field
  REAL(wp), PARAMETER               :: rdog=rd/grav

! (i) calculate altitude above NN and layer thickness in 
!     echam for altitude profiles
     zdeltag(1:kproma,1:klev)= &
          & ppd_hl(1:kproma,1:klev)* &
          & tk_fl(1:kproma,1:klev)/pp_fl(1:kproma,1:klev)*rdog
!!$     DO jk=1,klev
!!$        zh(1:kproma,jk)=(pgeom1(1:kproma,jk)+geosp(1:kproma,krow))*rgrav
!!$     END DO
     zh(1:kproma,klev)=0.5_wp*ppd_hl(1:kproma,klev)*tk_fl(1:kproma,klev)* &
                       rdog/pp_fl(1:kproma,klev)+geosp(1:kproma,krow,1)*rgrav
     DO jk=klev-1,1,-1
       zh(1:kproma,jk)=zh(1:kproma,jk+1) + &
                       0.5_wp*ppd_hl(1:kproma,jk+1)*tk_fl(1:kproma,jk+1)* &
                       rdog/pp_fl(1:kproma,jk+1) + &
                       0.5_wp*ppd_hl(1:kproma,jk)*tk_fl(1:kproma,jk)* &
                       rdog/pp_fl(1:kproma,jk)
     END DO
     DO jk=1,klev
        zdeltag_vr(1:kproma,jk)=zdeltag(1:kproma,klev-jk+1)
        zh_vr(1:kproma,jk)=zh(1:kproma,klev-jk+1)
     END DO
! (ii) calculate height profiles on echam grid for coarse and fine mode
     zq_aod_f(1:kproma,1:klev)=0._wp
     zq_aod_c(1:kproma,1:klev)=0._wp
     DO jk=1,klev
        kindex(1:kproma)=MAX(INT(zh_vr(1:kproma,jk)*rdz_clim+0.5_wp),1)
        DO jl=1,kproma
           IF (kindex(jl) > 0 .and. kindex(jl) <= lev_clim ) THEN
              zq_aod_c(jl,jk)= &
                & z_km_aer_c_mo(jl,kindex(jl),krow,nmw1_m)*wgt1_m+ &
                & z_km_aer_c_mo(jl,kindex(jl),krow,nmw2_m)*wgt2_m
              zq_aod_f(jl,jk)= &
               & z_km_aer_f_mo(jl,kindex(jl),krow,nmw1_m)*wgt1_m+ &
               & z_km_aer_f_mo(jl,kindex(jl),krow,nmw2_m)*wgt2_m
           END IF
        END DO
     END DO
! normalize height profile for coarse mode
     zq_int(1:kproma)=0._wp
     DO jk=1,klev
        zq_int(1:kproma)=zq_int(1:kproma)+ &
                       & zq_aod_c(1:kproma,jk)*zdeltag_vr(1:kproma,jk)
     ENDDO
     WHERE (zq_int(1:kproma) <= 0._wp)
        zq_int(1:kproma)=1._wp
     END WHERE
     DO jk=1,klev
        zq_aod_c(1:kproma,jk)=zdeltag_vr(1:kproma,jk)*zq_aod_c(1:kproma,jk)/ &
                            & zq_int(1:kproma)
     END DO
! normalize height profile for fine mode
     zq_int(1:kproma)=0._wp
     DO jk=1,klev
        zq_int(1:kproma)=zq_int(1:kproma)+ &
                       & zq_aod_f(1:kproma,jk)*zdeltag_vr(1:kproma,jk)
     ENDDO
     WHERE (zq_int(1:kproma) <= 0._wp)
        zq_int(1:kproma)=1._wp
     END WHERE
     DO jk=1,klev
        zq_aod_f(1:kproma,jk)=zdeltag_vr(1:kproma,jk)*zq_aod_f(1:kproma,jk)/ &
                            & zq_int(1:kproma)
     END DO

! (iii) far infrared
     zs_i(1:kproma,1:nb_lw)=1._wp-(wgt1_m*ssa_c_f(1:kproma,1:nb_lw,krow,nmw1_m)+ &
                                   wgt2_m*ssa_c_f(1:kproma,1:nb_lw,krow,nmw2_m))
     DO jk=1,klev
        DO jwl=1,nb_lw
           paer_tau_lw_vr(1:kproma,jk,jwl)=zq_aod_c(1:kproma,jk) * &
                zs_i(1:kproma,jwl) * &
                (wgt1_m*aod_c_f(1:kproma,jwl,krow,nmw1_m) + &
                 wgt2_m*aod_c_f(1:kproma,jwl,krow,nmw2_m)) 
        END DO
     END DO
! (iii) solar radiation
! time interpolated single scattering albedo (omega_f, omega_c)
     zs_c(1:kproma,1:nb_sw) = ssa_c_s(1:kproma,1:nb_sw,krow,nmw1_m)*wgt1_m + &
                              ssa_c_s(1:kproma,1:nb_sw,krow,nmw2_m)*wgt2_m
     zs_f(1:kproma,1:nb_sw) = ssa_f_s(1:kproma,1:nb_sw,krow,nmw1_m)*wgt1_m + &
                              ssa_f_s(1:kproma,1:nb_sw,krow,nmw2_m)*wgt2_m
! time interpolated asymmetry factor x ssa (omega_c*g_c, omega_{n,a}*g_{n,a})
     zg_c(1:kproma,1:nb_sw) = zs_c(1:kproma,1:nb_sw) * &
                              (asy_c_s(1:kproma,1:nb_sw,krow,nmw1_m)*wgt1_m + &
                               asy_c_s(1:kproma,1:nb_sw,krow,nmw2_m)*wgt2_m)
     zg_f(1:kproma,1:nb_sw) = zs_f(1:kproma,1:nb_sw) * &
                              (asy_f_s(1:kproma,1:nb_sw,krow,nmw1_m)*wgt1_m + &
                               asy_f_s(1:kproma,1:nb_sw,krow,nmw2_m)*wgt2_m)
! time interpolated aerosol optical depths
     zt_c(1:kproma,1:nb_sw)=wgt1_m*aod_c_s(1:kproma,1:nb_sw,krow,nmw1_m) + &
                          & wgt2_m*aod_c_s(1:kproma,1:nb_sw,krow,nmw2_m)
     zt_f(1:kproma,1:nb_sw)=wgt1_m*aod_f_s(1:kproma,1:nb_sw,krow,nmw1_m) + &
                          & wgt2_m*aod_f_s(1:kproma,1:nb_sw,krow,nmw2_m)
! height interpolation
! calculate optical properties
  DO jk=1,klev
! aerosol optical depth 
     DO jwl=1,nb_sw
        ztaua_c(1:kproma,jwl) = zt_c(1:kproma,jwl)*zq_aod_c(1:kproma,jk)
        ztaua_f(1:kproma,jwl) = zt_f(1:kproma,jwl)*zq_aod_f(1:kproma,jk)
     END DO
     paer_tau_sw_vr(1:kproma,jk,1:nb_sw) = ztaua_c(1:kproma,1:nb_sw) + &
                                         & ztaua_f(1:kproma,1:nb_sw) 
     paer_piz_sw_vr(1:kproma,jk,1:nb_sw) = &
                   & ztaua_c(1:kproma,1:nb_sw)*zs_c(1:kproma,1:nb_sw) + &
                   & ztaua_f(1:kproma,1:nb_sw)*zs_f(1:kproma,1:nb_sw)
     WHERE (paer_tau_sw_vr(1:kproma,jk,1:nb_sw) /= 0._wp) 
        paer_piz_sw_vr(1:kproma,jk,1:nb_sw)=paer_piz_sw_vr(1:kproma,jk,1:nb_sw)&
                                           /paer_tau_sw_vr(1:kproma,jk,1:nb_sw)
     ELSEWHERE
        paer_piz_sw_vr(1:kproma,jk,1:nb_sw)=1._wp
     END WHERE
     paer_cg_sw_vr(1:kproma,jk,1:nb_sw)  = &
     &ztaua_c(1:kproma,1:nb_sw)*zs_c(1:kproma,1:nb_sw)*zg_c(1:kproma,1:nb_sw)+&
     &ztaua_f(1:kproma,1:nb_sw)*zs_f(1:kproma,1:nb_sw)*zg_f(1:kproma,1:nb_sw)
     WHERE (paer_tau_sw_vr(1:kproma,jk,1:nb_sw) /= 0._wp) 
        paer_cg_sw_vr(1:kproma,jk,1:nb_sw)=paer_cg_sw_vr(1:kproma,jk,1:nb_sw)/&
                                          paer_piz_sw_vr(1:kproma,jk,1:nb_sw)/&
                                          paer_tau_sw_vr(1:kproma,jk,1:nb_sw)
     ELSEWHERE
        paer_cg_sw_vr(1:kproma,jk,1:nb_sw)=0._wp
     END WHERE
  ENDDO
END SUBROUTINE set_aop_kinne
!EOP
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: aero_read_opt
!
! !SUBROUTINE INTERFACE:
SUBROUTINE aero_read_opt ( &
  kbdim,           ngpblks,            nwl,               lev_clim,        &
  aod,             caod,               ssa,               cssa,            &
  asy,             casy,               aer_ex,            caer_ex,         &
  dz_clim,         cdz_clim,           imnthb,            imnthe,          &
  cfname,          asl,                casl)
!
! !DESCRIPTION:
! read optical aerosol parameters from file containg
! aod, ssa, asy, aer_ex (altitude dependent extinction), dz_clim (layer 
! thickness in meters), lev_clim (number of levels), and (optional) surface 
! altitude in meters.
!
! !REVISION HISTORY:
! original source by J.S. Rast (2010-01-08)
!
! !USES:
  USE mo_mpi,                 ONLY: p_parallel_io, p_io, p_bcast
  USE mo_decomposition,       ONLY: global_decomposition
  USE mo_transpose,           ONLY: scatter_gp
  USE mo_read_netcdf77,       ONLY: read_var_hs_nf77_3d, &
                                    read_diml_nf77, read_var_nf77_0d, &
                                    read_var_nf77_2d

! !INPUT PARAMETERS
  INTEGER, INTENT(in)            :: imnthb, imnthe !begin and end month 2b read
  INTEGER, INTENT(in)            :: kbdim, ngpblks, nwl, lev_clim
  CHARACTER(len=*), INTENT(in)   :: cfname
  CHARACTER(len=*), INTENT(in)   :: caod, cssa, casy, caer_ex, cdz_clim
  CHARACTER(len=*), INTENT(in), OPTIONAL     :: casl
  REAL(wp), INTENT(out)          :: aod(kbdim,nwl,ngpblks,imnthb:imnthe), &
                                    ssa(kbdim,nwl,ngpblks,imnthb:imnthe), &
                                    asy(kbdim,nwl,ngpblks,imnthb:imnthe)
  REAL(wp), INTENT(out)          :: aer_ex(kbdim,lev_clim,ngpblks,imnthb:imnthe)
  REAL(wp), INTENT(out), OPTIONAL:: asl(kbdim,ngpblks)
  REAL(wp), INTENT(out)          :: dz_clim

! !LOCAL VARIABLES
  REAL(wp), POINTER              :: faero1(:,:,:),faero2(:,:,:), &
                                    faero3(:,:,:)
  REAL(wp), POINTER              :: field_g3(:,:,:)
  REAL(wp), POINTER              :: field_g2(:,:)
  LOGICAL                        :: lex
  INTEGER                        :: j,jk,ierr,inwl,ilev_clim

  IF (p_parallel_io) THEN
     INQUIRE (file=TRIM(cfname), exist=lex)
     IF (.NOT. lex) THEN
        CALL finish('aero_read_opt','file '//TRIM(cfname)//' does not exist')
     END IF
     inwl=read_diml_nf77(TRIM(cfname),'lnwl')
     IF (inwl /= nwl) THEN
        CALL finish('aero_read_opt','incompatible number of wavelengths in file ' &
                    //TRIM(cfname))
     END IF
  END IF
  IF (p_parallel_io) THEN
     ALLOCATE(faero1(ldc%nlon,nwl,ldc%nlat))
     ALLOCATE(faero2(ldc%nlon,nwl,ldc%nlat))
     ALLOCATE(faero3(ldc%nlon,nwl,ldc%nlat))
  END IF
  DO j=imnthb,imnthe
     IF (p_parallel_io) THEN
        CALL read_var_hs_nf77_3d(TRIM(cfname),'lon','lnwl','lat','time',j, &
             TRIM(caod),faero1,ierr)
        CALL read_var_hs_nf77_3d(TRIM(cfname),'lon','lnwl','lat','time',j, &
             TRIM(cssa),faero2,ierr)
        CALL read_var_hs_nf77_3d(TRIM(cfname),'lon','lnwl','lat','time',j, &
             TRIM(casy),faero3,ierr)
     END IF
     CALL scatter_gp (faero1,aod(:,:,:,j),global_decomposition)
     CALL scatter_gp (faero2,ssa(:,:,:,j),global_decomposition)
     CALL scatter_gp (faero3,asy(:,:,:,j),global_decomposition)
  ENDDO
  IF (p_parallel_io) THEN
     ilev_clim= read_diml_nf77(TRIM(cfname),'lev')
     IF (ilev_clim /= lev_clim) THEN
        CALL finish('aero_read_opt','incompatible number of levels in file ' &
                    //TRIM(cfname))
     END IF
     CALL read_var_nf77_0d(TRIM(cfname),TRIM(cdz_clim),dz_clim,ierr)
     ALLOCATE(field_g3(ldc%nlon,lev_clim,ldc%nlat))
  END IF
  CALL p_bcast(dz_clim,p_io)
  DO j=imnthb,imnthe
     IF (p_parallel_io) THEN
        CALL read_var_hs_nf77_3d(TRIM(cfname),'lon','lev','lat','time', &
             j,TRIM(caer_ex),field_g3,ierr)
     END IF
     DO jk=1,lev_clim
        IF (p_parallel_io) THEN
           NULLIFY(field_g2)
           field_g2=>field_g3(:,jk,:)
        END IF
        CALL scatter_gp(field_g2,aer_ex(:,jk,:,j),global_decomposition)
     END DO
  END DO
  IF ( PRESENT(asl) .AND. PRESENT(casl) ) THEN
     IF (p_parallel_io) THEN
        ALLOCATE(field_g2(ldc%nlon,ldc%nlat))
        CALL read_var_nf77_2d(TRIM(cfname),'lon','lat',TRIM(casl), &
                                 field_g2,ierr)
     END IF
     CALL scatter_gp(field_g2,asl,global_decomposition)
  END IF
  IF ( (PRESENT(asl) .AND. .NOT. PRESENT(casl)) .OR. &
       (.NOT. PRESENT(asl) .AND. PRESENT(casl))) THEN
     CALL finish('aero_read_opt','wrong usage: either both casl and asl ' &
                    //'or none of both have to be present')
  END IF
  IF (p_parallel_io) THEN
     DEALLOCATE(faero1,faero2,faero3,field_g3)
     IF ( PRESENT(asl) .AND. PRESENT(casl) ) DEALLOCATE(field_g2)
  END IF
  END SUBROUTINE aero_read_opt
!END SUBROUTINE 
!EOP
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: cleanup_aero_kinne
!
SUBROUTINE cleanup_aero_kinne
!
! !DESCRIPTION: deallocate allocated fields
! 
!
! !REVISION HISTORY: original sourse: J.S.Rast, MPI-Hamburg 2001-01-31
! 
!
! !USES:
! !LOCAL VARIABLES

  IF(ALLOCATED(aod_c_s)) DEALLOCATE(aod_c_s)
  IF(ALLOCATED(aod_f_s)) DEALLOCATE(aod_f_s)
  IF(ALLOCATED(ssa_c_s)) DEALLOCATE(ssa_c_s)
  IF(ALLOCATED(ssa_f_s)) DEALLOCATE(ssa_f_s)
  IF(ALLOCATED(asy_c_s)) DEALLOCATE(asy_c_s)
  IF(ALLOCATED(asy_f_s)) DEALLOCATE(asy_f_s)
  IF(ALLOCATED(aod_c_f)) DEALLOCATE(aod_c_f)
  IF(ALLOCATED(ssa_c_f)) DEALLOCATE(ssa_c_f)
  IF(ALLOCATED(asy_c_f)) DEALLOCATE(asy_c_f)
  IF(ALLOCATED(z_km_aer_c_mo)) DEALLOCATE(z_km_aer_c_mo)
  IF(ALLOCATED(z_km_aer_f_mo)) DEALLOCATE(z_km_aer_f_mo)

  laero_set = .FALSE.

END SUBROUTINE cleanup_aero_kinne
!EOP
!-------------------------------------------------------------------------
!EOC
!-------------------------------------------------------------------------
END MODULE mo_aero_kinne
