module matdate
implicit none
!
!
contains


function datenum(getyear,getmonth,getday,gethour,getminute,hourlyint)
    use libdate
    implicit none
    ! define variables
    integer, intent(in) :: getyear,getmonth,getday,gethour,getminute
    integer, intent(in), optional :: hourlyint
    type(DATETYPE) :: datein
    type(DATETYPE) :: zerotime
    real*8 :: convertdays, datenum
    ! give initial value
    zerotime = datetype( 0, 0, 30, 0, 0 )
    ! calculate time difference (in days)
    datein= datetype(getyear,getmonth,getday,0,0);
    convertdays=timelag(datein,zerotime);
    convertdays=convertdays+(gethour/24.000)+(getminute/(24.000*60.000));
    if (present(hourlyint)) then
    ! round days based on given hourly interval
    convertdays=(nint(convertdays*(24.000/dble(hourlyint))))/(24.000/dble(hourlyint));
    end if
    datenum=convertdays;
end function


function chardate(year,month,day)												 ! function to convert integer year and month to char
	character (len=8) :: chardate
	character (len=4) :: yearchar
	character (len=2) :: monthchar
	character (len=2) :: daychar
	integer, intent(in) :: year,month
	integer, intent(in), optional :: day
	! start
	if (present(day)) then
	write(yearchar,"(I4)") year													 ! write year as 4 char
	write(monthchar,"(I2.2)") month												 ! write month as 2 char with leading zero
	write(daychar,"(I2.2)") day												 	 ! write month as 2 char with leading zero
	chardate=yearchar // monthchar // daychar
	else
    write(yearchar,"(I4)") year													 ! write year as 4 char
	write(monthchar,"(I2.2)") month												 ! write month as 2 char with leading zero
	chardate=yearchar // monthchar
    end if
end function


function charjulian(year,day)												    ! function to convert integer year and month to char
	character (len=7) :: charjulian
	character (len=4) :: yearchar
	character (len=3) :: daychar
	integer, intent(in) :: year,day
	! start
	write(yearchar,"(I4)") year													 ! write year as 4 char
	write(daychar,"(I3.3)") day												 	 ! write month as 2 char with leading zero
	charjulian=yearchar//daychar
end function


subroutine dateint(datenum,year,month,day,hour,minute)
    use libdate
    implicit none
    ! define variables
    real*8, intent(in) :: datenum
    integer, intent(out), optional :: year,month,day,hour,minute
    type(DATETYPE) :: zerotime, rounddate
    real*8 :: convertdays, hourdiff, mindiff
    character (len=10) :: yearchar, monthchar, daychar, hourchar, minchar
    ! give initial value
    zerotime = datetype( 0, 0, 30, 0, 0 )
    convertdays= datenum;
    hourdiff=24.000*(convertdays-nint(convertdays))
    convertdays=nint(convertdays);
    mindiff=60.000*(hourdiff-nint(hourdiff))
    hourdiff=nint(hourdiff);
    rounddate=zerotime+datetype(0,0,convertdays,0,0)
    rounddate=rounddate+datetype(0,0,0,hourdiff,0)
    rounddate=rounddate+datetype(0,0,0,0,mindiff)
    call format_date(rounddate, 'yyyy', yearchar)
    call format_date(rounddate, 'ms', monthchar)
    call format_date(rounddate, 'ds', daychar)
    call format_date(rounddate, 'HS', hourchar)
    call format_date(rounddate, 'MS', minchar)
    read(yearchar,*) year
    read(monthchar,*) month
    read(daychar,*) day
    read(hourchar,*) hour
    read(minchar,*) minute
end subroutine


subroutine datejulian(datenumin,year,month,day,hour,minute)
    use libdate
    implicit none
    ! define variables
    real*8, intent(in) :: datenumin
    integer, intent(out), optional :: year,month,day,hour,minute
    type(DATETYPE) :: zerotime, rounddate
    real*8 :: convertdays,hourdiff,mindiff,yeardatenum
    character (len=10) :: yearchar, monthchar, daychar, hourchar, minchar
    ! give initial value
    zerotime = datetype( 0, 0, 30, 0, 0 )
    convertdays= datenumin;
    hourdiff=24.000*(convertdays-nint(convertdays))
    convertdays=nint(convertdays);
    mindiff=60.000*(hourdiff-nint(hourdiff))
    hourdiff=nint(hourdiff);
    rounddate=zerotime+datetype(0,0,convertdays,0,0)
    rounddate=rounddate+datetype(0,0,0,hourdiff,0)
    rounddate=rounddate+datetype(0,0,0,0,mindiff)
    call format_date(rounddate, 'yyyy', yearchar)
    call format_date(rounddate, 'ms', monthchar)
    call format_date(rounddate, 'ds', daychar)
    call format_date(rounddate, 'HS', hourchar)
    call format_date(rounddate, 'MS', minchar)
    read(yearchar,*) year
    read(monthchar,*) month
    !read(daychar,*) day
    read(hourchar,*) hour
    read(minchar,*) minute
    yeardatenum=datenum(year,0,0,0,0)
    day=floor(datenumin-yeardatenum)
end subroutine





end module matdate
