module eramodule
implicit none



!interface eratime
!	module procedure eratime
!end interface
!	!
!interface eracoord
!	module procedure  eracoord
!end interface
!
!
!
!public :: eratime
!public :: eracoord

contains
!=====================================================================



!subroutine eratime(year,month,day,hour,erafile,timestep)
!!---------------------------------------------------------------------
!! function: convert datetime to related era interim file and timestep
!! example:
!!   integer::timestep
!!   character::erafile
!!   call eratime(1998,03,25,04,erafile,timestep)
!!---------------------------------------------------------------------
!    ! define variables
!    integer::year,month,day,hour
!    integer::timestep
!    integer,dimension(124,2)::datelist
!    integer::i,j,k !for loop
!    integer,parameter::hourinteval=6
!    integer::searchhour
!    character(len=2)::monthchar
!    character(len=4)::yearchar
!    character(len=6)::erafile
!    ! ---START FUNCTION---
!    erafile='NaN'
!    timestep=-9999
!    ! round search hour
!    searchhour=nint(dble(hour)/dble(hourinteval))*hourinteval
!    if (searchhour>=24) then
!        searchhour=0
!    end if
!    ! build datelist
!    k=1
!    do i=1,31
!    do j=1,4
!        datelist(k,1)=i
!        datelist(k,2)=(j-1)*hourinteval
!        k=k+1
!    end do !j
!    end do !i
!    ! search timestep
!    do i=1,124
!		if ((datelist(i,1)==day).and.(datelist(i,2)==searchhour)) then
!			timestep=i
!			exit
!        end if
!    end do !i
!    ! create erafile
!    write(monthchar,"(I2.2)") month
!    write(yearchar,"(I4)") year
!    erafile=yearchar//monthchar
!end subroutine
!
!
!subroutine eracoord(lat,lon,latpos,lonpos)
!!---------------------------------------------------------------------
!! function: convert datetime to related era interim file and timestep
!! example:
!!   integer::timestep
!!   character::erafile
!!   call eratime(1998,03,25,04,erafile,timestep)
!!---------------------------------------------------------------------
!	real*8,intent(in)::lat,lon
!	integer*4::searchlat,searchlon
!	integer*4,intent(out)::latpos,lonpos
!	integer*4::eralat(241),eralon(480)
!	real*8,parameter::resolution=0.75
!	integer::i
!	! ---START FUNCTION---
!	latpos=-9999;lonpos=-9999
!	! round search latitude and longitude
!	searchlat=((nint(lat/resolution))*resolution)*100
!	searchlon=((nint(lon/resolution))*resolution)*100
!	! build eralat and eralon
!	do i=1,241
!		eralat(i)=100*(90-(resolution*(i-1)));
!    end do
!    do i=1,480
!		eralon(i)=100*(0+(resolution*(i-1)));
!    end do
!    ! search latitude and longitude
!	do i=1,241
!		if (eralat(i)==searchlat) then
!			latpos=i
!			exit
!        end if
!    end do
!    do i=1,480
!		if (eralon(i)==searchlon) then
!			lonpos=i
!			exit
!        end if
!    end do
!end subroutine

subroutine build_eralatlon(outlat,outlon)
	real,dimension(241,480),intent(out)::outlat,outlon
	real,parameter::resolution=0.75
	integer::nrowx,ncolx

	! build latlon
    do nrowx=1,size(outlat,1)
		do ncolx=1,size(outlat,2)
			outlat(nrowx,ncolx)=(90-(resolution*(nrowx-1)));
			outlon(nrowx,ncolx)=(0+(resolution*(ncolx-1)));;
        end do
    end do

    ! change longitude from [0,360] to [-180,180]
    do nrowx=1,size(outlat,1)
		do ncolx=1,size(outlat,2)
			if (outlon(nrowx,ncolx)>180) then
				outlon(nrowx,ncolx)=(outlon(nrowx,ncolx))-360.0
			end if
        end do
    end do
end subroutine


subroutine eratime_desc(fileread,datenum_in,timestep,nearesttime)
    ! load module
    use netcdf
    use netcdfmodule
    use matdate

    ! define variable
    character(len=*)::fileread
    integer::timestep
    real*8::nearesttime,datenum_in,init_datenum
    integer,dimension(200)::gettime=-9999
    real*8,dimension(200)::getdatenum=-9999,datediff=-9999
    integer::ncid,DimID,nclen
    integer::ii
    integer::debug=0

    ! initiate variable
    timestep=-9999
    nearesttime=-9999.0

    ! begin check file
    call check(nf90_open(fileread,NF90_NOWRITE,ncid))
    call check(nf90_inq_dimid(ncid, "time",DimID))
    call check(nf90_inquire_dimension(ncid,DimID,len=nclen))
    call check(nf90_close(ncid))

    ! notify checking result
    write(*,*) '------------------------------'
    write(*,*) 'get eraInterim time dimension'
    write(*,*) 'ncid=',ncid
    write(*,*) 'DimID=',DimID
    write(*,*) 'length=',nclen
    write(*,*) '------------------------------'

    ! read time dimension
    call nc_read(fileread,"time",gettime(1:nclen))

    !initiate datenum
    init_datenum=datenum(1900,1,1,0,0)
    if (debug>=4) write(*,*) 'init date:', init_datenum

    ! convert time using datenum
    do ii=1,nclen
        getdatenum(ii)=init_datenum+(dble(gettime(ii))/24.0)
        if (debug>=4) write(*,*) 'datenum',ii,getdatenum(ii)
    end do

    ! calculate time difference
    do ii=1,nclen
        datediff(ii)=abs(getdatenum(ii)-datenum_in)
        if (debug>=4) write(*,*) ii, datediff(ii)
    end do

    ! get minimum location and nearest time
    timestep=minloc(datediff(1:nclen),1)
    nearesttime=getdatenum(timestep)
    if (debug>=4) write(*,*) 'output',timestep,nearesttime
end subroutine


subroutine check(status)
	use netcdf
    integer, intent ( in) :: status
    if(status /= nf90_noerr) then
      print *, trim(nf90_strerror(status))
      stop "Stopped"
    end if
end subroutine check


end module
