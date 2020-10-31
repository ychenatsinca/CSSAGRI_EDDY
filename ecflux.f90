!-----Eddy Covariance Fortran Calculation Module 
!-----Contact: Yi-Ying Chen (yiyingchen@gate.sinica.edu.tw)
!-----Date: 2019-03-17
!-----Research Center for Environmental Changes
!-----Coordinate rotation!
!-----SITA: Rotation about Z axis (force average V=0)
!-----PHI:  Rotation about Y axis (force average W=0)
!-----PSI:  New Y and Z Axis Rotate About axis (forcr average vw=0)
!-----SPL:  WINDOW SIZE
!-----U, V, W: Ture velocity (AFTER CORRECTION)
!-----UAVG, VAVG, WAVG: WINDOW AVERAGE WIND SPEED
!-----Commend for R share library > ecflux.so 
!     R CMD SHLIB ecflux.f90
!     or gfortran -c -fPIC ecflux.f90 | gfortran -shared -o ecflux.so ecflux.o 
!-------------------------------------------------------------------
      subroutine ecflux(spl,u,v,w,v1,v2,v3,v4,sita,phi,uavg,wd,ustar, &
      & f1,f2,f3,f4)
      implicit none
      integer :: i,j,k,spl
      real*8, external :: cov 
      real*8 :: pi
      real*8 :: vavg,wavg
      real*8 :: vwavg,vvavg,wwavg 
      real*8 :: dum1,dum2,dum3                                        
      real*8, intent(inout) :: u(spl),v(spl),w(spl),v1(spl),v2(spl),& 
      & v3(spl),v4(spl) 
      real*8, intent(out)   :: sita,phi,uavg,wd,ustar,f1,f2,f3,f4
!-----
      pi=4.*atan(float(1))   
!-----
      do i=1,spl
      uavg=u(i)+uavg
      vavg=v(i)+vavg      
      wavg=w(i)+wavg     
!      write(*,*) 'raw data:',ta(i),qa(i),co2(i) 
      end do
!-----
      uavg=uavg/spl
      vavg=vavg/spl
      wavg=wavg/spl
!     write(6,'(3(A5,F8.3))') 'UAVG=',uavg,'VAVG=',vavg,'WAVG=',wavg
!-----      
      if(uavg .eq. 0.)then
            sita=pi/2.
      else 
            sita=atan(abs(vavg/uavg))
      end if 
!-----
      if(sqrt(uavg**2.+vavg**2.) .eq. 0.) then
        phi=pi/2.
        else
        phi=atan(wavg/sqrt(uavg**2.+vavg**2.))
      end if     
!     write(*,'(A9,A6,F8.2,A5,F8.2)') 'ROTATION:','PHI:',(180/PI)*PHI,'SITA:',(180/PI)*SITA
      do i=1,spl
         dum1=u(i) 
         dum2=u(i)*cos(sita)+v(i)*sin(sita)
         u(i)=(u(i)*cos(sita)+v(i)*sin(sita))*cos(phi)+w(i)*sin(phi)  
         v(i)=v(i)*cos(sita)-dum1*sin(sita)
         w(i)=w(i)*cos(phi)-dum2*sin(phi)
      END DO
!-----
      do i=1,spl
      !uavg=u(i)+uavg
      !vavg=v(i)+vavg      
      wavg=w(i)+wavg     
      end do
!-----
      uavg=sqrt(uavg*uavg + vavg*vavg)
      !vavg=vavg/spl
      wavg=wavg/spl
!-----Calculation of ustar 
      ustar=(cov(w,u,spl)**2.+cov(w,v,spl)**2.)**.25 
!-----Calculate surface fluxes 
      f1=cov(w,v1,spl)       
      f2=cov(w,v2,spl)
      f3=cov(w,v3,spl)
      f4=cov(w,v4,spl)
      write(6,'(5(A3,F15.5))') 'u*:',ustar,' f1:',f1,' f2:',f2,& 
      & ' f3:',f3, ' f4:',f4
!-----write output information 
      !write(6,'(3(A5,F8.3))') 'UAVG=',uavg,'VAVG=',vavg,'WAVG=',wavg
      !write(6,*) 'Rotation Angle About Axes:'
      write(6,*)'Z-Axes=',(180/pi)*sita,'Y-Axes=',(180/pi)*phi
!-----Calculate wind direction
      wd = (180/pi)*sita 
!-----Offset to wind camposs define N=0      
      if (( uavg .ge. 0.) .and. ( vavg .gt. 0. ))  wd = 270. - wd
      if (( uavg .ge. 0.) .and. ( vavg .lt. 0. ))  wd = 270. + abs(wd) 
      if (( uavg .le. 0.) .and. ( vavg .lt. 0. ))  wd = 180. - wd
      if (( uavg .le. 0.) .and. ( vavg .gt. 0. ))  wd = 180. + abs(wd) 
      if ( wd .lt. 0) wd = wd + 360
      write(6,*)'Wind_dir=', wd
      end subroutine ecflux   
!==== End of Main Program ====

!.....Covariance funtion
      real*8 function cov(var1, var2, n)
      implicit none
      real*8, intent(in)    :: var1(n), var2(n)
      integer, intent(in) :: n
      real*8, external :: avg
      real*8 :: dev, acc, avg1, avg2 
      integer :: i
        
      acc=0.
      dev=0.
      avg1=0.
      avg2=0.
      avg1=avg(var1, n)
      avg2=avg(var2, n)
      do i=1, n
         dev=(var1(i)-avg1)*(var2(i)-avg2)
         acc=dev+acc
      end do
      cov=acc/float(n)      
      
      return
      end function cov         

!.....Average function    
      real*8 function avg(var, n)
      implicit none
      integer, intent(in) :: n 
      real*8,  intent(in) :: var(n)     
        
      avg=0.
      avg=sum(var)/float(n)
       
      return
      end function avg    
        
!.....Standard deviation funtion
      real*8 function std(var1, n)
      implicit none      
      integer, intent(in) :: n
      real*8,  intent(in) :: var1(n)
      real*8, external :: avg
      real*8           :: acc, dev, avg1       
      integer :: i
      
      acc=0.
      dev=0.
      avg1=0.
      avg1=avg(var1, n)
      do i=1, n
         dev=(var1(i)-avg1)**2.
         acc=acc+dev
      end do
      acc=acc/float(n)
      std=sqrt(acc)      
        
      return
      end function std  
!==========================================     
!=====Stability Integral Function "Persi_m"
!===========================================
      real*8 function persi_m(zeta)
      real*8 :: zeta, y, x,xm
      real*8, parameter :: a=0.33,b=0.41, c=0.33, d=0.057,n=0.78 ! Parameters
      character(len=3)  :: profile
!=====  
      
       profile='B-D'   !Businger-Dyer Profile 
!      Profile='Brt'   !Brutsaert     Profile

       if (profile .eq. 'Brt') then            
!===Brutsaert Profile=== 
           if ( (zeta .gt. 0.) .and.( zeta .le. 1.) ) then
              persi_m=-5.*zeta                                                   
              !for stable condiction
           else if ( (zeta .gt. 0.) .and.( zeta .gt. 1.) ) then
              persi_m=-5. -5.*log(zeta)
           else        
              y=-zeta
              x=(y/a)**(1./3.)                                                 
              !for unstable condiction
              if ( y .gt. b**(-3.))  y= b**(-3.)
              persi_m=log(a+y)-3.*b*(y**(1./3.))+ ((b*(a**(1./3.)))/2.)*log( ((1+x)**2.)/ (1-x+x*x) ) &
              +(3.**.5)*b*(a**(1./3.))* atan((2*x-1)/(3.**.5))+ (-log(a)+(3**.5)*b*(a**(1./3.))*(3.14159/6.))           
           endif
       else                                     
!===Businger-Dyer Profile===     
          if (zeta .gt. 0.)  then  
             persi_m=-5.*zeta       
             !for stable condiction
          else      
             xm=(1.-16.*(zeta))**.25  
             persi_m=2.*log((1+xm)/2.)+log((1+xm*xm)/2.)-2*atan(xm)+3.14159/2.  
             !for unstable condiction	           
          endif
     
       endif

      !Set to Neurtal Condiction
      !Profile='Ntl'
      !If (profile .eq. 'Ntl') persi_m=0. 
      return
      end function persi_m

!==========================================
!=====Stability Integral Function "Persi_v"
!==========================================
      real*8 function persi_v(zeta)
      Real*8 :: zeta, y, x
      Real*8,parameter :: a=0.33,b=0.41, c=0.33, d=0.057,n=0.78 ! Parameters
      character(len=3) :: profile
!=====	
             
         if  ( (zeta .gt. 0.) .and.( zeta .le. 1.) ) then    
              persi_v=-5.*zeta                   !for stable condiction
         else if (  zeta .gt. 1. ) then
              persi_v=-5. -5.*log(zeta)
         else 
              y = -zeta
              x = (y/a)**(1./3.)
              persi_v=((1-d)/n)*log((c+y**n)/c)  !for unstable condiction 
          endif

      !Set to Neurtal Condiction
      !Profile='Ntl'
	  !If (profile .eq. 'Ntl') persi_v=0.
   
      return
      end function persi_v      
 
!=======================================================
!subroutine airden for the air property calculation 
!temp[oC] rh[%] pres[KPa] density[kg/m3] visc[m2/s]
!=======================================================	  
      real*8 function airden(temp, rh, pres, density, visc)
      implicit none
      real*8, intent(in) :: temp,rh,pres   
      real*8,intent(out) :: density,visc
      !local variables
      real*8 :: es,ea,lv
      real*8 :: denvap,dendry,t,q
      real*8 :: r,cp  
      
      r = 287.          ! Gas Constant of Air (J/kg/K)
      t = temp + 273.15 ! Absolute Temperature (K)
      cp = 1006.        ! Specific heat of air (J/kg/K)
      !pres = pres/1000. ! Air Presssure (kPa) 
   
      es = 0.6105*exp(17.27*temp/(237.3+temp))  ! Saturation Vapor pres (kPa)
      ea = es*RH/100.                           ! Vapor pressure (kPa)
 
      denvap = (0.622*ea*1000)/(r*t)     ! Density of Vapor (kg/m3)
      dendry = (pres-ea)*1000/(r*t)      ! Density of Dry Air (kg/m3)
      density = denvap + dendry          ! Density of Moist Air (kg/m3)

      q = denvap/density                 ! Specific humidity (m3/m3)
      visc = (0.0094*temp+1.3193)*1.0E-5 ! kinematic Viscosity (m2/s) 
      lv = 4.186*(597.46-0.5662*temp)    ! Latent Heat (J/g)
    
      WRITE(*,*) 'T:',temp, 'pressure:[Kpa]', pres
      WRITE(*,*) 'Air density:' , density
      
      return
      end function airden


