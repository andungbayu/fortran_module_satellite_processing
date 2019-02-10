module mainmodule
implicit none
!
! -----------------------global variables---------------------------

! Public interfaces
!
	interface f90_read
			module procedure    f90_read1d,f90_read2d,f90_read3d
	end interface
	!
	interface f90_read_double
			module procedure 	f90_read2dbl
    end interface

	interface f90_save
			module procedure    f90_save1d,f90_save2d,f90_save3d
	end interface
	!
	interface average
			module procedure    average1d,average2d
	end interface
	!
	interface shum2mixratio
			module procedure    shum2mixratio_2d,shum2mixratio_3d
	end interface

	interface num2str
			module procedure    num2str_int,num2str_dble
	end interface

	interface match
			module procedure    match_int,match_logic,match_int1d,match_logic1d
	end interface

	interface reallocate
			module procedure	reallocate_1D_int
    end interface

    interface sortrows
			module procedure    sortrows_int,sortrows_real,sortrows_dble
	end interface
!
!
!
! ####################
!
! private variables
	character (len=10), parameter :: binformat='F10.3',binformat_w='D17.10'
	private :: binformat, binformat_w
!
! public interface declaration
	public :: f90_read
	public :: f90_save
	public :: shum2mixratio
	public :: num2str
	public :: match
	public :: average
	public ::reallocate
!
! ####################
!
! ---------------------------function-------------------------------
contains
!
!
function f90_read1d(filename,dims)
implicit none
	character (len=50):: dummy
	character (len=*) :: filename
	character (len=20) :: readformat
	integer :: dims,m
	real*8 :: f90_read1d(dims)
	! formatting IO stream
	readformat='('//trim(binformat)//')'
	! open IO stream
	open (15,file=filename)
	! read header
	do m=1,3
		read(15,'(A50)') dummy
	end do
	! read data
	do m=1,dims
		read(15,readformat) f90_read1d(m)
	end do
	! close IO stream
	close(15)
end function f90_read1d
!
!
function f90_read2d(filename,dims1,dims2,label)
implicit none
	character (len=50):: dummy
	character (len=*) :: filename
	character (len=20) :: readformat
	character (len=5) :: dimchar
	integer :: dims1,dims2,m
	real*8 :: f90_read2d(dims1,dims2)
	logical,optional::label
	! formatting IO stream
	write(dimchar,'(I5)') dims2
	readformat='('//trim(dimchar)//trim(binformat)//')'
	! open IO stream
	open (15,file=filename)
	! check label existence
	if (present(label)) then
		! read header
		write(*,*) 'read header'
		do m=1,3
			read(15,'(A50)') dummy
		end do
	end if
	! read data
	do m=1,dims1
		write(*,*) 'dim1',m
		read(15,readformat) f90_read2d(m,:)
	end do
	! close IO stream
	close(15)
end function f90_read2d
!
!
function f90_read3d(filename,dims1,dims2,dims3)
implicit none
	character (len=50):: dummy
	character (len=*) :: filename
	character (len=20) :: readformat
	character (len=5) :: dimchar
	integer :: dims1,dims2,dims3,m,n
	real*8 :: f90_read3d(dims1,dims2,dims3)
	! formatting IO stream
	write(dimchar,'(I5)') dims3
	readformat='('//trim(dimchar)//trim(binformat)//')'
	! open IO stream
	open (15,file=filename)
	! read header
	do m=1,3
		read(15,'(A50)') dummy
	end do
	! read data
	do m=1,dims1
	do n=1,dims2
		read(15,readformat) f90_read3d(m,n,:)
	end do
	end do
	! close IO stream
	close(15)
end function f90_read3d
!
!
subroutine f90_save1d(filename,datain,dims1)
implicit none
	character (len=50):: dummy
	character (len=*) :: filename
	character (len=20) :: readformat
	character (len=5) :: dimchar
	integer :: dims1,m
	real*8 :: datain(dims1)
	! formatting IO stream
	readformat='('//trim(binformat_w)//')'
	! open IO stream
	open(15,file=filename)
	! write data
	do m=1,dims1
		write(15,readformat) datain(m)
	end do
	! close IO stream
	close(15)
end subroutine f90_save1d
!
!
subroutine f90_save2d(filename,datain,dims1,dims2)
implicit none
	character (len=50):: dummy
	character (len=*) :: filename
	character (len=20) :: readformat
	character (len=5) :: dimchar
	integer :: dims1,dims2,m
	real*8 :: datain(dims1,dims2)
	! formatting IO stream
	write(dimchar,'(I5)') dims2
	readformat='('//trim(dimchar)//trim(binformat_w)//')'
	! open IO stream
	open(15,file=filename)
	! write data
	do m=1,dims1
		write(15,readformat) datain(m,:)
	end do
	! close IO stream
	close(15)
end subroutine f90_save2d
!
!
subroutine f90_save3d(filename,datain,dims1,dims2,dims3)
implicit none
	character (len=50):: dummy
	character (len=*) :: filename
	character (len=20) :: readformat
	character (len=5) :: dimchar
	integer :: dims1,dims2,dims3,m,n
	real*8 :: datain(dims1,dims2,dims3)
	! formatting IO stream
	write(dimchar,'(I5)') dims3
	readformat='('//trim(dimchar)//trim(binformat_w)//')'
	! open IO stream
	open(15,file=filename)
	! write data
	do m=1,dims1
	do n=1,dims2
		write(15,readformat) datain(m,n,:)
	end do
	end do
	! close IO stream
	close(15)
end subroutine f90_save3d
!
!
function f90_read2dbl(filename,dims1,dims2,readrow)
!-------------------------------------------------------------
! usage	: read bin for 1 dimensional or 2 dimensional data
! input	: a) filename=character file to read
!		  b) dims1,dims2=dimension of the text file
!         c) readrow=limit of the maximum row to read
! i.e	: data=f90_read2dbl('tes.bin',120,240,80)
!-------------------------------------------------------------
implicit none
	character (len=50):: dummy
	character (len=*) :: filename
	character (len=20)::readformat
	character (len=5) :: dimchar
	integer :: dims1,dims2,m
	real*8 :: f90_read2dbl(dims1,dims2)
	integer, optional :: readrow
	! formatting IO stream
	write(dimchar,'(I5)') dims2
	readformat='('//trim(dimchar)//trim(binformat_w)//')'
	! open IO stream
	open (15,file=filename)
	! read header
	!do m=1,3
		!read(15,'(A50)') dummy
	!end do
	! read data
	if (present(readrow)) then
		do m=1,readrow
		read(15,readformat) f90_read2dbl(m,:)
		!write(*,*) f90_read2dbl(m,:)
		end do
	else
		do m=1,dims1
		read(15,readformat) f90_read2dbl(m,:)
		!write(*,*) f90_read2dbl(m,:)
		end do
	end if
	! close IO stream
	close(15)
end function f90_read2dbl
!
!                        end IOstream module
!-----------------------------------------------------------------
!



!              begin humidity  conversion module
! ================================================================
!
function shum2mixratio_2d(shum,hpa,sizey,sizex)
	integer :: sizey, sizex
	real*8  :: m_dry, m_wet , c
	real*8, intent(in) :: shum(sizey,sizex), hpa(sizey,sizex)
	real*8  :: shum2mixratio_2d(sizey,sizex),e(sizey,sizex)
	real*8  :: pa(sizey,sizex),pd(sizey,sizex)
    ! start
	m_dry = 28.9644;  ! kg / kmol,  molar mass of dry air
	m_wet = 18.0152;  ! kg / kmol,  molar mass of water vapor
	c=m_wet/m_dry;    ! water vapor composition
	pa=hpa*1000.00;   ! convert hPa to Pa
	e = (shum*pa) / (c+(1 - c)*shum); ! calculate partial pressure
	pd = pa - e; ! calculate dry air pressure
	shum2mixratio_2d = (e/pd)*c;
end function
!
!
function shum2mixratio_3d(shum,hpa,sizey,sizex,sizez)
	integer :: sizey, sizex, sizez
	real*8  :: m_dry, m_wet , c
	real*8, intent(in) :: shum(sizey,sizex,sizez), hpa(sizey,sizex,sizez)
	real*8  :: shum2mixratio_3d(sizey,sizex,sizez), pd(sizey,sizex,sizez)
	real*8  :: e(sizey,sizex,sizez), pa(sizey,sizex,sizez)
    ! start
	m_dry = 28.9644;  ! kg / kmol,  molar mass of dry air
	m_wet = 18.0152;  ! kg / kmol,  molar mass of water vapor
	c=m_wet/m_dry;    ! water vapor composition
	pa=hpa*1000.00;   ! convert hPa to Pa
	e = (shum*pa) / (c+(1 - c)*shum); ! calculate partial pressure
	pd = pa - e; ! calculate dry air pressure
	shum2mixratio_3d = (e/pd)*c;
end function
!
!
!             end humidity  conversion module
!-----------------------------------------------------------------
!
!

function repmat(datain,sizein,sizeout)
	integer, intent(in) :: sizein,sizeout
	real*8, intent(in) :: datain(sizein)
	real*8 :: repmat(sizein,sizeout)
    repmat=spread(datain,2,sizeout)
end function
!
!
function repchar(getlist)
	! ------------------------------------------------------
	! function to replace backslash to slash
	! example: outstr=repchar('saya\makan')
	! output:  'saya/makan'
	!-------------------------------------------------------
	character (len=120), intent(in) :: getlist
	character (len=120) :: repchar
	character (len=120) :: get1,get2
	integer :: t
	! start
	repchar=getlist
	rep: do
	t=index(repchar,'\')
	if (t==0) exit rep
	get1=repchar(1:t-1)
	get2=repchar(t+1:120)
	repchar=trim(get1) // '/' // trim(get2)
	end do rep
end function


function repstring(getlist,oldstring,newstring)
	! ------------------------------------------------------
	! function to replace some words inside string
	! example: outstr=repstring('saya makan','makan','tidur')
	! output:  'saya tidur'
	!-------------------------------------------------------

	! define variables
	character(len=*),intent(in)::getlist
	character(len=120)::repstring
	character(len=*)::oldstring,newstring
	character (len=120)::get1,get2
	integer::t
	integer::test=0
	! initiate character by copy
	repstring=getlist
	! loop to inside character
	rep: do
	    ! find character index
	    t=index(repstring,oldstring)
	    if (test>=4)write(*,*)'str pos found',t
	    ! exit loop if char not found
	    if (t==0) exit rep
	    ! replace character if any
	    get1=repstring(1:t-1)
	    get2=repstring(t+len(oldstring):len(repstring))
	    repstring=get1(1:t-1)//newstring//get2
	end do rep
end function


function finddata(stringlist,charsearch,listdim)								 ! function to find era interim data for specific date and month
	integer, intent(in) :: listdim
	character (len=120) :: stringlist(listdim)
	character (len=120) :: finddata
	character (len=120) :: getlist
	character (len=6), intent(in) :: charsearch
	integer :: t, idxdate
	! start
	searchtemp: do t=1,size(stringlist)										 ! loop to read file list line by line
			idxdate=index(stringlist(t),charsearch);							 ! find specific year and month from file list
			if ( idxdate>0 ) then                                               ! exit if found matching value
				getlist=stringlist(t);                                         	 ! obtain specific row
				finddata=repchar(getlist);										 ! replace backslash to slash character
				exit searchtemp												 	 ! exit loop if specific filename found
		    endif
		end do searchtemp
end function finddata
!
!
function findgroupdata(stringlist,charsearch,listdim,startend)
	!-------------------------------------------------------------
	! function to find era interim data for specific date and month
	! usage		: find a group of filelist matched with specific criteria
	! input		: a) stringlist = input list of file to read
	!             b) charsearch = string to search each row
	!			  c) listdim = number of row of string list to read
	!             d) startend = optional, subset of string to check
	! example	: getfile=findgroupdata(filelist,'2008',94000,[41,44])
	!-------------------------------------------------------------
	!
	integer, intent(in) :: listdim
	character (len=120), intent(in) :: stringlist(listdim)
	character (len=*), intent(in) :: charsearch
	character (len=120) :: storelist(listdim)
	character (len=120) :: findgroupdata(15000)
	character (len=120) :: getlist,rowlist
	integer :: t, i, idxdate
	integer,optional :: startend(2)
	! start
	i=0;
   if (present(startend)) then
	do t=1,size(stringlist)										 	 			! loop to read file list line by line
		rowlist=stringlist(t)
		idxdate=index(rowlist(startend(1):startend(2)),charsearch);								! find specific year and month from file list
		if ( idxdate>0 ) then                                                	! exit if found matching value
			i=i+1
			getlist=stringlist(t);                                         	 	! obtain specific row
			storelist(i)=repchar(getlist);									 	! replace backslash to slash character
		endif
	end do
   else
	do t=1,size(stringlist)										 	 			! loop to read file list line by line
		idxdate=index(stringlist(t),charsearch);							 	! find specific year and month from file list
		if ( idxdate>0 ) then                                                	! exit if found matching value
			i=i+1
			getlist=stringlist(t);                                         	 	! obtain specific row
			storelist(i)=repchar(getlist);									 	! replace backslash to slash character
		endif
	end do
   end if
    findgroupdata='NaN'
	findgroupdata(1:i)=storelist(1:i);
end function
!
!
function find(datain,sizey,sizex,searchvalue,dir)							    ! function to find index location between two values
	integer :: i
	integer, intent(in) :: sizex,sizey,dir
	real*8, intent(in) :: datain(sizey,sizex)
	real*8, intent(in) :: searchvalue
	integer :: find
	real*8 :: linein,lineout
	find=-9999 ! initial value
	! find data
    if (dir==1) then                                                             ! if direction = vertical
		do i=1,sizey												 	 		 ! loop each row
			if (datain(i,1)==searchvalue) then  								 ! get valid condition
			!if ( datain(i,1)-searchvalue <= 0 .and. datain(i+1,1)-searchvalue > 0) then  ! get valid condition
            find=i                                                         		 ! copy loop value to index
            exit														 		 ! exit loop after finding the value
            end if
        end do                                                            		 ! end loop
    elseif (dir==2) then                                                         ! if direction = horizontal
		! check direction
		do i=1,sizex
			if ( datain(1,i)==searchvalue) then
            find=i
            exit
            end if
        end do
    endif
end function
!
!
function findindex(datain,sizey,sizex,searchvalue,dir)							 ! function to find index location between two values
	integer :: i
	integer, intent(in) :: sizex,sizey,dir
	real*8, intent(in) :: datain(sizey,sizex)
	real*8, intent(in) :: searchvalue
	integer :: findindex
	real*8 :: linein,lineout
	findindex=-9999;
    if (dir==1) then                                                             ! if direction = vertical
		!check direction
		linein=datain(1,1);lineout=datain(sizey,1)
		if (linein<lineout) then
		yloop1 : do i=1,sizey												 	 		 ! loop each row
			if ( datain(i,1)-searchvalue <= 0 .and. datain(i+1,1)-searchvalue > 0) then  ! get valid condition
            findindex= i                                                         		 ! copy loop value to index
            exit yloop1															 ! exit loop after finding the value
            end if
        end do yloop1                                                            ! end loop
        else
		yloop2 : do i=1,sizey												 	 		 ! loop each row
			if ( datain(i,1)-searchvalue >= 0 .and. datain(i+1,1)-searchvalue < 0) then  ! get valid condition
            findindex= i                                                         		 ! copy loop value to index
            exit yloop2															 ! exit loop after finding the value
            end if
        end do yloop2
        endif
    elseif (dir==2) then                                                         ! if direction = horizontal
		! check direction
		linein=datain(1,1);lineout=datain(1,sizex)
		if (linein<lineout) then
		xloop1 : do i=1,sizex
			if ( datain(1,i)-searchvalue <=0 .and. datain(1,i+1)-searchvalue > 0) then
            findindex= i
            exit xloop1
            end if
        end do xloop1
        else
		xloop2 : do i=1,sizex
			if ( datain(1,i)-searchvalue >=0 .and. datain(1,i+1)-searchvalue < 0) then
            findindex= i
            exit xloop2
            end if
        end do xloop2
		endif
    endif
end function
!
!
function nearestpos(datain,sizey,sizex,searchvalue,dir,distance)				 ! function to find index location between two values
	integer :: i
	integer, intent(in) :: sizex,sizey,dir
	real*8, intent(in) :: datain(sizey,sizex)
	real*8, intent(in) :: searchvalue
	integer :: findindex,nearestpos
	real*8 :: linein,lineout,distance
	findindex=-9999;
    if (dir==1) then                                                             ! if direction = vertical
		!check direction
		yloop1 : do i=1,sizey												 	 ! loop each row
			if ((datain(i,1)-searchvalue)**2<=distance**2) then  				 ! get valid condition
            findindex= i                                                         ! copy loop value to index
            exit yloop1															 ! exit loop after finding the value
            end if
        end do yloop1                                                            ! end loop
    elseif (dir==2) then                                                         ! if direction = horizontal
		! check direction
		xloop1 : do i=1,sizex
			if ((datain(1,i)-searchvalue)**2<=distance**2) then
            findindex= i
            exit xloop1
            end if
        end do xloop1
    endif
    nearestpos=findindex
end function
!
!
function interp2d(datain,datay,datax,size_y,size_x,interpy,interpx,size_iy,size_ix) ! function to interpolate
    integer, intent(in) :: size_x, size_y, size_ix, size_iy
    real*8, intent(in) :: datain(size_y,size_x)
    real*8, intent(in) :: datax(size_y,size_x)
    real*8, intent(in) :: datay(size_y,size_x)
    real*8, intent(in) :: interpy(size_iy,size_ix)
    real*8, intent(in) :: interpx(size_iy,size_ix)
	real*8 :: interp2d(size_iy,size_ix), interp(size_iy,size_ix)
	integer :: x,y, idx_x, idx_y
	real*8 :: x_min,x_max,y_min,y_max
	real*8 :: dx,dy
	! indexing
	do x=1,size_ix
		do y=1,size_iy
			idx_x=findindex(datax,size(datain,1),size(datain,2),interpx(y,x),2)	 ! find array index contain x direction
            idx_y=findindex(datay,size(datain,1),size(datain,2),interpy(y,x),1)	 ! find array index contain y direction
            x_min=datax(idx_y,idx_x);											 ! get minimum lon grid around the point
            x_max=datax(idx_y,idx_x+1);											 ! get maximum lon grid around the point
            y_min=datay(idx_y,idx_x);											 ! get minimum lat grid around the point
            y_max=datay(idx_y+1,idx_x);											 ! get maximum lat grid around the point
            ! interp
            dx=(interpx(y,x)-x_min)/(x_max-x_min);                               ! calculate horiz difference
            dy=(interpy(y,x)-y_min)/(y_max-y_min);								 ! calculate vertical difference
            interp(y,x)=((1-dx)*(1-dy)*datain(idx_y,idx_x)) + &
				(dx*(1-dy)*datain(idx_y,idx_x+1)) + &
				(dx*dy*datain(idx_y+1,idx_x+1)) + &
                ((1-dx)*dy*datain(idx_y+1,idx_x))								 ! calculate bilinear interpolation
        end do
    end do
    interp2d=interp;															 ! assign value to final result
end function
!
!
function regrid(datain,latitude,longitude,size_lat,size_lon,regridlat,regridlon) ! function to interpolate
    integer :: size_lat,size_lon
    real*8 :: longitude(size_lat,size_lon),latitude(size_lat,size_lon)
	real*8 :: regridlon(size_lat,size_lon),regridlat(size_lat,size_lon)
    integer :: col,row
    integer :: idx_col,idx_row
    real*8    :: col_min,col_max,row_min,row_max
    real*8 :: dx,dy
    real*8 :: datain(size_lat,size_lon),regrid(size_lat,size_lon)
    ! start program
    regrid=-9999
    do col=1,size(datain,2)
		do row=1,size(datain,1)
			idx_col=findindex(longitude,size(datain,1),size(datain,2),regridlon(row,col),2) ! find array index contain x direction
            idx_row=findindex(latitude,size(datain,1),size(datain,2),regridlat(row,col),1)  ! find array index contain y direction
            if ((idx_col>0) .and. (idx_row>0)) then
            col_min=longitude(idx_row,idx_col);									! get minimum lon grid around the point
            col_max=longitude(idx_row,idx_col+1);								! get maximum lon grid around the point
            row_min=latitude(idx_row,idx_col);									! get minimum lat grid around the point
            row_max=latitude(idx_row+1,idx_col);								! get maximum lat grid around the point
            ! regrid
            dx=(regridlon(row,col)-col_min)/(col_max-col_min);                  ! calculate horiz difference
            dy=(regridlat(row,col)-row_min)/(row_max-row_min);					! calculate vertical difference
            regrid(row,col)=((1-dx)*(1-dy)*datain(idx_row,idx_col)) + &
				(dx*(1-dy)*datain(idx_row,idx_col+1)) + &
				(dx*dy*datain(idx_row+1,idx_col+1)) + &
                ((1-dx)*dy*datain(idx_row+1,idx_col))							! calculate bilinear interpolation
            endif
        end do
    end do
end function
!
!
function cdiff(datain,size_y,size_x,dir)										 ! function to calculate horizontal difference
	integer, intent(in) :: size_x, size_y,dir                                    ! WARNING!!! Boundary is not valid
	real*8, intent(in) :: datain(size_y,size_x)
    real*8 :: data_uIn(size_y,size_x+2), data_uOut(size_y,size_x+2)
    real*8 :: data_uDiff(size_y,size_x+2)
	real*8 :: data_vIn(size_y+2,size_x), data_vOut(size_y+2,size_x)
	real*8 :: data_vDiff(size_y+2,size_x)
    real*8 :: cdiff(size_y,size_x)
    ! start
	if (dir==2) then															 ! if direction is horizontal
		data_uIn=0;data_uOut=0;													 ! give initial value as 0
		data_uOut(:,1:size_x)=datain;											 ! copy data to extended initial
		data_uIn(:,3:(size_x+2))=datain;										 ! copy data to extended end
		data_uDiff=data_uOut-data_uIn;											 ! calculate difference
		cdiff=data_uDiff(:,2:(size_x+1));									     ! crop the area difference
	else if (dir==1) then														 ! if direction is vertical
		data_vIn=0;data_vOut=0;													 ! give initial value as 0
		data_vOut(1:size_y,:)=datain;											 ! copy data to extended initial
		data_vIn(3:(size_y+2),:)=datain;                                         ! copy data to extended end
		data_vDiff=data_vOut-data_vIn;                                           ! calculate difference
		cdiff=data_vDiff(2:(size_y+1),:);                                        ! crop the area difference
	else
		write(*,*)"direction is false"
	endif
end function cdiff
!
!
function vdiff(datain,size_x,size_y,size_z)          							 ! function to calculate vertical difference
	integer :: t,t_row                                                           ! WARNING !!! upper boundary is not valid
	integer, intent(in) :: size_x, size_y, size_z
	real*8, intent(in) :: datain(size_x,size_y,size_z)
	integer :: idx_up, idx_down
    real*8 :: outdata(size_x,size_y,size_z), vdiff(size_x,size_y,size_z)
    real*8 :: upvalue(size_x,size_y),downvalue(size_x,size_y)
    outdata=0
	! calculate vdiff
	do t=1,size_z
		if ((t>1) .or. (t<size_z)) then
		idx_up=t+1;
		idx_down=t-1;
		upvalue=datain(:,:,idx_up);
		downvalue=datain(:,:,idx_down);
		outdata(:,:,t)=downvalue-upvalue;
		endif
	end do !for t
	outdata(:,:,1)=0;
	outdata(:,:,size_z)=0;
    vdiff=outdata;
end function
!
!
function vdiff_winterp(datain,size_x,size_y,size_z)          					 ! function to calculate vertical difference with interpolation
	integer :: t,t_row                                                           ! WARNING !!! upper boundary is not valid
	integer, intent(in) :: size_x, size_y, size_z
	real*8, intent(in) :: datain(size_x,size_y,size_z)
	integer :: idx_up, idx_down
    real*8 :: interpolated(size_x,size_y,2*size_z)
    real*8 :: outdata(size_x,size_y,size_z), vdiff_winterp(size_x,size_y,size_z)
    real*8 :: upvalue(size_x,size_y),downvalue(size_x,size_y)
    outdata=0
    ! expand lev to 2 times
	do t=1,size_z                                                    			 ! loop to original vertical level
	t_row=t*2;																	 ! double original size
	interpolated(:,:,t_row)=datain(:,:,t);                                    	 ! assign original data to fill space
	if (t<size_z) then
	interpolated(:,:,t_row+1)=(datain(:,:,t)+datain(:,:,t+1))/2;               	 ! assign average data to fill between space
	end if
	end do !for t
	interpolated(:,:,1)=interpolated(:,:,2)+(interpolated(:,:,2)-interpolated(:,:,3));
	! calculate vdiff
	do t=1,size_z
		idx_up=(t*2)+1;
		idx_down=(t*2)-1;
		upvalue=interpolated(:,:,idx_up);
		downvalue=interpolated(:,:,idx_down);
		outdata(:,:,t)=upvalue-downvalue;
	end do !for t
    vdiff_winterp=outdata;
end function
!
!
function transpose3d(datain,size_y,size_x,size_z);
    integer, intent(in) :: size_x,size_y,size_z
    integer :: i,j,k
    real*8 :: datain(size_y,size_x,size_z)
    real*8 :: transpose3d(size_x,size_y,size_z)
    do i=1,size_y
	do j=1,size_x
	do k=1,size_z
        transpose3d(j,i,k)=datain(i,j,k)
    end do
    end do
    end do
end function
!
!
function match_logic(datain,logicin,nrow,ncol)
	integer :: nrow,ncol
	real*8 :: datain(nrow,ncol), match_logic(nrow,ncol)
	logical :: logicin(nrow)
	integer :: i,t
	! get matched data
	match_logic=-9999 ! write NaN value
	t=0;
    do i=1,nrow
		if (logicin(i) .eqv. .TRUE.) then
		t=t+1;
		match_logic(t,:)=datain(i,:)
        end if
    end do
end function
!
!
function match_int(datain,logicin,nrow,ncol)
	integer :: nrow,ncol
	real*8 :: datain(nrow,ncol), match_int(nrow,ncol)
	integer :: logicin(nrow)
	integer :: i,t
	! get matched data
	match_int=-9999 ! write NaN value
	t=0;
    do i=1,nrow
		if (logicin(i)==1) then
		t=t+1;
		match_int(t,:)=datain(i,:)
        end if
    end do
end function
!
!
function match_logic1d(datain,logicin,nrow)
	integer :: nrow,ncol
	real*8 :: datain(nrow), match_logic1d(nrow)
	logical :: logicin(nrow)
	integer :: i,t
	! get matched data
	match_logic1d=-9999 ! write NaN value
	t=0;
    do i=1,nrow
		if (logicin(i) .eqv. .TRUE.) then
		t=t+1;
		match_logic1d(t)=datain(i)
        end if
    end do
end function
!
!
function match_int1d(datain,logicin,nrow)
	integer :: nrow,ncol
	real*8 :: datain(nrow), match_int1d(nrow)
	integer :: logicin(nrow)
	integer :: i,t
	! get matched data
	match_int1d=-9999 ! write NaN value
	t=0;
    do i=1,nrow
		if (logicin(i)==1) then
		t=t+1;
		match_int1d(t)=datain(i)
        end if
    end do
end function
!
!
function valunique(datain,sizey,sizex,uniquecol)
	! variable
	integer, intent(in) :: sizey,sizex,uniquecol
	real*8, intent(in) :: datain(sizey,sizex)
	integer :: t, i, j
	logical :: a
	real*8 :: uniquearray(sizey,sizex+1)
	real*8 :: valunique (sizey,sizex)
	real*8 :: signval, getval, maxvalue
	logical :: getlogic(sizey)
	! copy initial value and add 1 array
	uniquearray(:,1:sizex)=datain;   ! copy initial value
	uniquearray(:,sizex+1)=-9999;    ! unsingned index
	valunique=-9999				 	 ! original value of valunique
	! loop by identifying unsingned index
	t=1;
		! find unsigned value
		do i=1,sizey
			signval=uniquearray(i,sizex+1);
			getval=uniquearray(i,uniquecol);
			if (signval==-9999) then
			getlogic=uniquearray(:,uniquecol)==getval;
			where (getlogic) uniquearray(:,sizex+1) = t
			t=t+1;
			end if
		end do
	!
	! align the result
	t=1
	maxvalue=maxval(uniquearray(:,sizex+1));
	do i=1,size(uniquearray,1)
	signval=uniquearray(i,sizex+1)
	getval=uniquearray(i,uniquecol);
	if (nint(signval)==t) then
	valunique(t,1)=getval
	t=t+1;
	cycle
	end if
	end do
end function
!
!
subroutine dbz2z_2d(datain,minvalue,maxvalue)
	!-------------------------------------------------------------
	! function: convert dbz to z for 2 dimensional  input
	! input   :
	! 			real*8::datain(:,:)
    !			real*8::minvalue,maxvalue
	! usage   :
	!			real*8::refl(10,10)
	!			call dbz2z_2d(refl,-4000,5000)
	! output  : => z value (2 dimension
	!-------------------------------------------------------------
    real*8::datain(:,:)
    real*8,allocatable::copydata(:,:)
    real*8::minvalue,maxvalue
    ! allocate array
    allocate(copydata(size(datain,1),size(datain,2)))
    copydata=datain
    datain=datain/100
    datain=(10**(datain*0.1))
    where ((copydata>maxvalue).or.(copydata<minvalue)) datain=-9999
end subroutine
!
!
subroutine flush2d(datain,countdata,addvalue,countvalue,rowpos,nrow,ncol,dims)
	!-------------------------------------------------------------
	! function: combine two data based on value and count
	! input   :  real*8,dimension(:,:)::datain
	!			 real*8,dimension(:)::addvalue
	!			 integer*4::countvalue(:),rowpos
	!		     integer::dims
	! usage   :  real*8::a(10),b(10)
	!			 real*8::c(10,10),d(10,10)
	!			 integer::i
	!			 c=100;d=1;a=50;b=1
	!			 call flush2d(c,d,a,nint(b),5,1)
	! output  : => new c,d
	!-------------------------------------------------------------
	real*8,dimension(:,:)::datain
	real*8::countdata(:,:),countvalue(:)
	real*8,dimension(:)::addvalue
	integer::rowpos
	integer::dims,nrow,ncol
	real*8,dimension(nrow,ncol)::expandvalue,expandcount,totalinit
	! calculate total initial value
	expandvalue=0
	expandcount=0
	! classify expansion
    expandvalue(rowpos,:)=addvalue
    expandcount(rowpos,:)=countvalue
	! combine data
	totalinit=datain*countdata
	countdata=countdata+expandcount
	datain=(totalinit+expandvalue)/(countdata)
	where (countdata==0) datain=0
	! write data
!	write(*,*) datain
!	write(*,*) ' '
!	write(*,*) ' '
!	write(*,*) countdata
end subroutine
!
!
subroutine average1d(datain,datacount,addvalue,countvalue,addpos)
	!-------------------------------------------------------------
	! function: combine two data based on value and count
	! input   :  real*8,dimension(:,:)::datain
	!			 real*8,dimension(:)::addvalue
	!			 integer*4::countvalue(:),rowpos
	!		     integer::dims
	! usage   :  real*8::a(10),b(10)
	!			 real*8::c(10,10),d(10,10)
	!			 integer::i
	!			 c=100;d=1;a=50;b=1
	!			 call average1d(c,d,13.5,1,[5])
	! output  : => new c,d
	!-------------------------------------------------------------
	real,dimension(:),intent(inout)::datain
	integer,intent(inout)::datacount(:)
	real::addvalue
	integer::countvalue
	integer::addpos
	! calculate total initial value
    datain(addpos)=((datain(addpos)*datacount(addpos)) + &
		addvalue) /(datacount(addpos)+countvalue)
	datacount(addpos)=datacount(addpos)+countvalue
end subroutine
!
!
subroutine average2d(datain,datacount,addvalue,countvalue,addpos)
	!-------------------------------------------------------------
	! function: combine two data based on value and count
	! input   :  real*8,dimension(:,:)::datain
	!			 real*8,dimension(:)::addvalue
	!			 integer*4::countvalue(:),rowpos
	!		     integer::dims
	! usage   :  real*8::a(10),b(10)
	!			 real*8::c(10,10),d(10,10)
	!			 integer::i
	!			 c=100;d=1;a=50;b=1
	!			 call average2d(c,d,13.5,1,[5,1])
	! output  : => new c,d
	!-------------------------------------------------------------
	real,dimension(:,:),intent(inout)::datain
	real,intent(inout)::datacount(:,:)
	real::addvalue,countvalue
	integer::addpos(2)
	! calculate total initial value
    datain(addpos(1),addpos(2))=((datain(addpos(1),addpos(2))*datacount(addpos(1),addpos(2))) + &
		addvalue) /(datacount(addpos(1),addpos(2))+countvalue)
	datacount(addpos(1),addpos(2))=datacount(addpos(1),addpos(2))+countvalue
end subroutine
!
!
function gunzip(gzfile,extractfile,pipename)
	character(len=*),intent(in)::gzfile,extractfile,pipename
	character(len=300)::runsystem
	integer::getinfo(13)
	integer::io
	integer::gunzip,stdout,stats
	! run function
	!runsystem='gzip -tcd '//trim(gzfile)
	!call system(runsystem)
	runsystem='gzip -cd '//trim(gzfile)//' > '//trim(extractfile)//' ; echo $? > '//trim(pipename)//' &';
	write(*,*) runsystem;
	call system(runsystem)
	open(12,file=trim(pipename))
	read(12,*,iostat=io) stats
	close(12)
	if ((stats/=0).or.(io/=0)) then
		gunzip=0
	else
		gunzip=1
	end if
end function


subroutine extract(gzfile,extractfile)
	character(len=*),intent(in)::gzfile,extractfile
	character(len=300)::runsystem
	integer::getinfo(13)
	integer::io
	integer::gunzip,stdout,stats
	! run function
	runsystem='gzip -cd '//trim(gzfile)//' > '//trim(extractfile);
	write(*,*) runsystem;
	call system(runsystem)
end subroutine
!
!
function linspace(init,interval,fin,length)
	!-------------------------------------------------------------
	! function: create linear space similar to matlab [init:int:end]
	! input   : a) init,interval,fin=input range and inteval
	!			b) length= array length to obtain
	! usage   : height=linspace(1,1,100,100)
	! output  : => matlab [1:1:100]
	!-------------------------------------------------------------
	real, intent(in) :: init,interval,fin
	integer*4, intent(in) :: length
	integer :: nrow
	real*8 :: linspace(length)
	!
	do nrow=1,length
        linspace(nrow)=init +(interval*(nrow-1))
	end do
	!
end function
!
!
function num2str_int(datain,length)
	!-------------------------------------------------------------
	! function: convert double to char
	! input   : a) datain=input data
	!			b) length= string length to obtain
	! usage   : string=num2str(2008,10)
	! output  : => 0000002008
	!-------------------------------------------------------------
	integer*4 :: datain
	integer*4 :: dataint
	integer :: length
	character (len=50):: getnum
	character (len=length) :: num2str_int
	!
	dataint=datain
	write(getnum,"(I50.50)") dataint
    num2str_int=getnum(int(51-dble(length)):50)
end function
!
!
function num2str_dble(datain,length)
	!-------------------------------------------------------------
	! function: convert double to char
	! input   : a) datain=input data
	!			b) length= string length to obtain
	! usage   : string=num2str(2008,10)
	! output  : => 0000002008
	!-------------------------------------------------------------
	real*8 :: datain
	integer*4 :: dataint
	integer :: length
	character (len=50):: getnum
	character (len=length) :: num2str_dble
	!
	dataint=int(datain)
	write(getnum,"(I50.50)") dataint
    num2str_dble=getnum(int(51-dble(length)):50)
end function
!
!
function str2dble(datain)
	!-------------------------------------------------------------
    ! Function: convert string to double
    ! Example : num=str2dble('002109')
    ! Output  : 2109.00000
    !-------------------------------------------------------------
	character (len=*), intent(in) :: datain
	real*8 :: str2dble
	!
	read(datain,*) str2dble
end function
!
!
function str2num(datain)
	!-------------------------------------------------------------
    ! Function: convert string to integer
    ! Example : num=str2dble('002109')
    ! Output  : 2109
    !-------------------------------------------------------------
	character (len=*), intent(in) :: datain
	integer :: str2num
	real*8 :: dbleval
	!
	read(datain,*) dbleval
	str2num=nint(dbleval)
end function
!
!
function vector2idx(vector,nrowx)
	!-------------------------------------------------------------
    ! Function: convert vectorization index to 2 dimension index
    ! Example : num=vector2idx(12,6)
    ! Output  : [2,6]
    !-------------------------------------------------------------
	integer::vector2idx(2)
	integer::nrowx
	integer::vector

	! calculating number of row
	vector2idx(1)=floor(dble(vector)/(dble(nrowx)+1.0))+1

	! calculating number of column
	vector2idx(2)=(mod(vector,nrowx))+1
end function


subroutine reallocate_1D_int(datain,newlen)
	integer,allocatable,intent(inout),dimension(:)::datain
	integer,intent(in)::newlen
	integer,allocatable,dimension(:)::copy

	! allocate copy data
	allocate(copy(size(datain,1)))

	! copy original data to copy array
	copy=datain

	! deallocate and reallocate input
	deallocate(datain)
	allocate(datain(newlen))

	! return back data to original array
	datain(1:size(copy,1))=copy
	deallocate(copy)

end subroutine



subroutine compressfolder(folderin,folderout)
	!-------------------------------------------------------------
	! function: compress folder to tar.gz file
	! example: call compressfolder('outfile','outfile.tar.gz')
	! output: tar.gz file
	!-------------------------------------------------------------
	character(len=*)::folderin,folderout
	character(len=360)::syntax

	! build syntax
	syntax='tar -czvf '//trim(folderout)//' '//trim(folderin)

	! display and run syntac
	write(*,*) syntax
	call system(syntax)
end subroutine




subroutine checkprogress
	character::dummy
		write(*,*) 'continue??? [y/n]'
		read(*,*) dummy
		if (dummy.eq.'n') stop
end subroutine



subroutine sortrows_int(datain,dataout,idx)
	!-----------------------------------------------------------------------
	! function: sort vector (1D) and index position of the original vector
	! example: sortrows(rand(100),dataout,idx)
	! output: sorted data (dataout) and index position (idx)
	!-----------------------------------------------------------------------
	integer,dimension(:),intent(in)::datain
	integer,dimension(:),intent(out)::dataout,idx
	integer::i,j,b
	integer::a

	! copy data input to data output
	dataout=datain

	! numbering index
	do i=1,size(dataout,1)
		idx(i)=i;
    end do

	! begin sort
	do j=2,size(dataout,1)

		! copy current value
		a=datain(j)
		b=idx(j)

		! compare current value with previous one
		do i=j-1,1,-1

			! skip to index 10 if find smaller value
			if (dataout(i)<=a) goto 10

			! shift value if higher than current value
			dataout(i+1)=dataout(i)
			idx(i+1)=idx(i)

		end do

		! indexing minimum loop (pos==1)
		i=0

		! executing skip or minimum function
		10 dataout(i+1)=a
		   idx(i+1)=b

  end do
end subroutine


subroutine sortrows_real(datain,dataout,idx)
	!-----------------------------------------------------------------------
	! function: sort vector (1D) and index position of the original vector
	! example: sortrows(rand(100),dataout,idx)
	! output: sorted data (dataout) and index position (idx)
	!-----------------------------------------------------------------------
	real,dimension(:),intent(in)::datain
	integer,dimension(:),intent(out)::idx
	real,dimension(:),intent(out)::dataout
	integer::i,j,b
	real::a

	! copy data input to data output
	dataout=datain

	! numbering index
	do i=1,size(dataout,1)
		idx(i)=i;
    end do

	! begin sort
	do j=2,size(dataout,1)

		! copy current value
		a=datain(j)
		b=idx(j)

		! compare current value with previous one
		do i=j-1,1,-1

			! skip to index 10 if find smaller value
			if (dataout(i)<=a) goto 10

			! shift value if higher than current value
			dataout(i+1)=dataout(i)
			idx(i+1)=idx(i)

		end do

		! indexing minimum loop (pos==1)
		i=0

		! executing skip or minimum function
		10 dataout(i+1)=a
		   idx(i+1)=b

  end do
end subroutine


subroutine sortrows_dble(datain,dataout,idx)
	!-----------------------------------------------------------------------
	! function: sort vector (1D) and index position of the original vector
	! example: sortrows(rand(100),dataout,idx)
	! output: sorted data (dataout) and index position (idx)
	!-----------------------------------------------------------------------
	real*8,dimension(:),intent(in)::datain
	integer,dimension(:),intent(out)::idx
	real*8,dimension(:),intent(out)::dataout
	integer::i,j,b
	real*8::a

	! copy data input to data output
	dataout=datain

	! numbering index
	do i=1,size(dataout,1)
		idx(i)=i;
    end do

	! begin sort
	do j=2,size(dataout,1)

		! copy current value
		a=datain(j)
		b=idx(j)

		! compare current value with previous one
		do i=j-1,1,-1

			! skip to index 10 if find smaller value
			if (dataout(i)<=a) goto 10

			! shift value if higher than current value
			dataout(i+1)=dataout(i)
			idx(i+1)=idx(i)

		end do

		! indexing minimum loop (pos==1)
		i=0

		! executing skip or minimum function
		10 dataout(i+1)=a
		   idx(i+1)=b

  end do
end subroutine


subroutine sort(tableinout,sortpos,row,col)
	!-----------------------------------------------------------------------
	! function: sort table (dble) by giving index row to sort
	! example: sort(rand(100,10),5,100,10)
	! output: sorted table (tableinout)
	!-----------------------------------------------------------------------
	integer,intent(in)::row,col,sortpos
	integer::nrow
	real*8,dimension(row,col),intent(inout)::tableinout
	real*8,dimension(row,col)::copytable
	integer,dimension(row)::sortidx
	real*8,dimension(row)::sortdata

	! begin sort rows
	call sortrows(tableinout(:,sortpos),sortdata,sortidx)

	! sorting the entire table
	copytable=tableinout
	do nrow=1,row
		tableinout(nrow,:)=copytable(sortidx(nrow),:)
    end do
end subroutine


subroutine sort_int(tableinout,sortpos,row,col)
	!-----------------------------------------------------------------------
	! function: sort table (int32) by giving index row to sort
	! example: sort(rand(100,10),5,100,10)
	! output: sorted table (tableinout)
	!-----------------------------------------------------------------------
	integer,intent(in)::row,col,sortpos
	integer::nrow
	integer,dimension(row,col),intent(inout)::tableinout
	integer,dimension(row,col)::copytable
	integer,dimension(row)::sortidx
	integer,dimension(row)::sortdata

	! begin sort rows
	call sortrows(tableinout(:,sortpos),sortdata,sortidx)

	! sorting the entire table
	copytable=tableinout
	do nrow=1,row
		tableinout(nrow,:)=copytable(sortidx(nrow),:)
    end do
end subroutine


subroutine open_slot(slot)
    character(len=*)::slot
    integer::slot_init

    ! define slot running process
    slot_init=1

    ! write process notification to slot file
	open(15,file=slot)
		write(15,'(I1)') slot_init
	close(15)
end subroutine


subroutine close_slot(slot)
    character(len=*)::slot
    integer::slot_end

    ! define slot running process
    slot_end=0

    ! write process notification to slot file
	open(15,file=slot)
		write(15,'(I1)') slot_end
	close(15)
end subroutine





!---------------------------------------------------------------------

end module mainmodule
