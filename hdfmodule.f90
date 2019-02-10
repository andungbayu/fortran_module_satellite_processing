module hdfmodule
implicit none


! ----------------------------global interface------------------------------


interface hdfread_1d
	module procedure hdfread_1d,hdfread_1ddim
end interface

interface hdfread
	module procedure hdfread_2d,hdfread_2ddim
end interface

interface hdfread_3d
	module procedure hdfread_3d,hdfread_3ddim
end interface

interface hdfread_3d_sub
    module procedure hdfread_3d_real,hdfread_3d_dble,hdfread_3d_int32
end interface

interface vdataread
    module procedure vdataread_1d,vdataread_0d
end interface

interface vdataread_int8 !check w matlab
    module procedure vdataread_1d_int8
end interface

interface vdataread_int16 !check w matlab
    module procedure vdataread_1d_int16
end interface

interface hdfread_attr
	module procedure hdfread_attr
end interface

interface hdfcreate
	module procedure hdfcreate3d_2var
end interface

interface hdfwrite
	module procedure writehdf3d
end interface

interface writehdf5d
	module procedure writehdf5d_real,writehdf5d_int
end interface

interface transpose3d
	module procedure transpose3d_dble,transpose3d_real,transpose3d_int16,transpose3d_int32
end interface





! ----------------------------global variables------------------------------

! public variables
integer,parameter,public::DFACC_CREATE=4
!integer,public::sfstart,sfend,sfcreate,sfendacc
integer,public::writestatus
integer,parameter,public::sds_int=24,sds_real=5,sds_dble=6
integer,public::sd_id,sds_id

! private variables
private transpose3d
integer,private,parameter::trmmlen=99,trmmwdth=99,trmmlev=99




! ==================================function=================================
contains




!--------------------------- load HDF subroutine ----------------------------



function hdfgetdim(filename,variable,ndim)
    ! variable
    character (len=*), intent(in) :: filename
    character (len=*), intent(in) :: variable
    integer, intent(in) ::  ndim
    integer :: hdfgetdim
    integer :: DFACC_READ
    integer :: sfstart, sfn2index, sfselect, sfginfo, sfrdata, sfendacc, sfend
    integer :: sd_id, sds_id, sds_index, getstatus
    integer :: n_rank, data_type
    integer :: n_attrs
    character :: name*(256)
    integer :: dim_sizes_dim(32)
    integer :: start(2), edges(2), stride(2)
    integer :: n_column,n_row
    parameter   (DFACC_READ = 1)

    ! initiating value
    hdfgetdim=-9999

    ! begin sds routine
    sd_id = sfstart(trim(filename), DFACC_READ)
    sds_index = sfn2index(sd_id, trim(variable))
    sds_id = sfselect(sd_id, sds_index)
    getstatus = sfginfo(sds_id, name, n_rank, dim_sizes_dim, data_type, n_attrs)

    ! define dimension
    hdfgetdim=dim_sizes_dim(ndim)

    !close sds connection
    getstatus = sfendacc(sds_id)
    getstatus = sfend(sd_id)
end function


function hdfread_1d(filename,variable)
    !-------------------------------------------------------------
    ! Function: read hdf for 1 dimensional data based on default dimension
    ! Example : lat=hdfread_1d('tes.hdf','Latitude')
    !-------------------------------------------------------------
    ! variable
    character (len=*), intent(in) :: filename
    character (len=*), intent(in) :: variable
    real*8 :: hdfread_1d(trmmlen)
    real*4 :: hdfread_float32(trmmlen)
    integer*2 :: hdfread_int16(trmmlen)
    integer*1 :: hdfread_int8(trmmlen)
    integer :: DFACC_READ
    integer :: sfstart, sfn2index, sfselect, sfginfo, sfrdata, sfendacc, sfend
    integer :: sd_id, sds_id, sds_index, getstatus
    integer :: n_rank, data_type
    integer :: n_attrs
    character :: name*(256)
    integer :: dim_sizes_dim(32)
    integer :: start, edges, stride
    integer :: n_column,n_row
    parameter   (DFACC_READ = 1)
    ! initiating value
    hdfread_1d=-9999
    ! begin sds routine
    sd_id = sfstart(trim(filename), DFACC_READ)
    sds_index = sfn2index(sd_id, trim(variable))
    sds_id = sfselect(sd_id, sds_index)
    getstatus = sfginfo(sds_id, name, n_rank, dim_sizes_dim, data_type, n_attrs)
    ! define dimension
    n_column=dim_sizes_dim(1);
    start = 0;
    edges = dim_sizes_dim(1);
    write(*,*) 'rank:',n_rank,'dimension(1):',dim_sizes_dim(1)
    stride = 1;
    write(*,*) 'sd_id:',sd_id,'sds_index:',sds_index,'sds_id:',sds_id, &
    'getstatus:',getstatus,'edges:',edges,'datatype:',data_type
    ! acquire data
    if (data_type==5) then
		hdfread_float32=-9999
        getstatus = sfrdata(sds_id, start, stride, edges, hdfread_float32)
        hdfread_1d=dble(hdfread_float32);
    elseif (data_type==22) then
		hdfread_int16=-9999
        getstatus = sfrdata(sds_id, start, stride, edges, hdfread_int16)
        hdfread_1d=dble(hdfread_int16);
    elseif (data_type==20) then
		hdfread_int8=-99
        getstatus = sfrdata(sds_id, start, stride, edges, hdfread_int8)
        hdfread_1d=dble(hdfread_int8);
    end if
    hdfread_1d(edges:trmmlen)=-9999
    !close sds connection
    getstatus = sfendacc(sds_id)
    getstatus = sfend(sd_id)
end function
!
!
function hdfread_1ddim(filename,variable,hdfdim)
    !-------------------------------------------------------------
    ! Function: read hdf with specified dimension
    ! Example : lat=hdfread_1d('tes.hdf','Latitude',[3300,208,2])
    !-------------------------------------------------------------
    ! variable
    character (len=*), intent(in) :: filename
    character (len=*), intent(in) :: variable
    integer, intent(in) ::  hdfdim(3)
    real*8 :: hdfread_1ddim(hdfdim(1))
    real*4 :: hdfread_float32(hdfdim(1))
    integer*2 :: hdfread_int16(hdfdim(1))
    integer*1 :: hdfread_int8(hdfdim(1))
    integer :: DFACC_READ
    integer :: sfstart, sfn2index, sfselect, sfginfo, sfrdata, sfendacc, sfend
    integer :: sd_id, sds_id, sds_index, getstatus
    integer :: n_rank, data_type
    integer :: n_attrs
    character :: name*(256)
    integer :: dim_sizes_dim(32)
    integer :: start, edges, stride
    integer :: n_column,n_row
    parameter   (DFACC_READ = 1)
    ! initiating value
    hdfread_1ddim=-9999
    ! begin sds routine
    sd_id = sfstart(trim(filename), DFACC_READ)
    sds_index = sfn2index(sd_id, trim(variable))
    sds_id = sfselect(sd_id, sds_index)
    getstatus = sfginfo(sds_id, name, n_rank, dim_sizes_dim, data_type, n_attrs)
    ! define dimension
    n_column=dim_sizes_dim(1);
    start = 0;
    edges = dim_sizes_dim(1);
    write(*,*) 'rank:',n_rank,'dimension(1):',dim_sizes_dim(1)
    stride = 1;
    write(*,*) 'sd_id:',sd_id,'sds_index:',sds_index,'sds_id:',sds_id, &
    'getstatus:',getstatus,'edges:',edges,'datatype:',data_type
    !
    if (dim_sizes_dim(1)>0) then
        ! acquire data
        if (data_type==5) then
            hdfread_float32=-9999
            getstatus = sfrdata(sds_id, start, stride, edges, hdfread_float32)
            hdfread_1ddim=dble(hdfread_float32);
        elseif (data_type==22) then
            hdfread_int16=-9999
            getstatus = sfrdata(sds_id, start, stride, edges, hdfread_int16)
            hdfread_1ddim=dble(hdfread_int16);
        elseif (data_type==20) then
            hdfread_int8=-99
            getstatus = sfrdata(sds_id, start, stride, edges, hdfread_int8)
            hdfread_1ddim=dble(hdfread_int8);
        elseif (data_type==6) then
            getstatus = sfrdata(sds_id, start, stride, edges, hdfread_1ddim)
        else
            hdfread_1ddim=-9999;
        end if

        if (edges<hdfdim(1)) then
            hdfread_1ddim(edges:hdfdim(1))=-9999
        end if
    end if
    !
    !close sds connection
    getstatus = sfendacc(sds_id)
    getstatus = sfend(sd_id)
end function
!
!
function hdfread_2d(filename,variable)
    ! variable
    character (len=*), intent(in) :: filename
    character (len=*), intent(in) :: variable
    real*8 :: hdfread_2d(trmmlen,trmmwdth)
    real*4:: hdfread_float32(trmmwdth,trmmlen)
    integer(2):: hdfread_int16(trmmwdth,trmmlen)
    integer:: hdfread_int8(trmmwdth,trmmlen)
    integer :: DFACC_READ
    integer :: sfstart, sfn2index, sfselect, sfginfo, sfrdata, sfendacc, sfend
    integer :: sd_id, sds_id, sds_index, getstatus
    integer :: n_rank, data_type
    integer :: n_attrs
    character :: name*(256)
    integer :: dim_sizes_dim(32)
    integer :: start(2), edges(2), stride(2)
    integer :: n_column,n_row
    parameter   (DFACC_READ = 1)
    ! initiating value
    hdfread_2d=-9999
    ! begin sds routine
    sd_id = sfstart(trim(filename), DFACC_READ)
    sds_index = sfn2index(sd_id, trim(variable))
    sds_id = sfselect(sd_id, sds_index)
    getstatus = sfginfo(sds_id, name, n_rank, dim_sizes_dim, data_type, n_attrs)
    ! define dimension
    n_column=dim_sizes_dim(1); n_row=dim_sizes_dim(2);
    start(1) = 0; start(2) = 0;
    edges(1) = dim_sizes_dim(1); edges(2) = dim_sizes_dim(2);
    write(*,*) 'rank:',n_rank,'dimension(1):',dim_sizes_dim(1),'dimension(2):',dim_sizes_dim(2)
    stride(1) = 1; stride(2) = 1;
    write(*,*) 'sd_id:',sd_id,'sds_index:',sds_index,'sds_id:',sds_id, &
    'getstatus:',getstatus,'edges:',edges,'datatype:',data_type
    ! acquire data
    if (data_type==5) then
		hdfread_float32=-9999
        getstatus = sfrdata(sds_id, start, stride, edges, hdfread_float32)
        hdfread_2d=transpose(dble(hdfread_float32))
    elseif (data_type==22) then
		hdfread_int16=-9999
        getstatus = sfrdata(sds_id, start, stride, edges, hdfread_int16)
        hdfread_2d=transpose(dble(hdfread_int16))
    elseif (data_type==20) then
		hdfread_int8=-9999
        getstatus = sfrdata(sds_id, start, stride, edges, hdfread_int8)
        hdfread_2d=transpose(dble(hdfread_int8))
    end if
    hdfread_2d(edges(2):trmmlen,:)=-9999
    !close sds connection
    getstatus = sfendacc(sds_id)
    getstatus = sfend(sd_id)
end function
!
!
function hdfread_2ddim(filename,variable,hdfdim)
    ! variable
    character (len=*), intent(in) :: filename
    character (len=*), intent(in) :: variable
    integer, intent(in) ::  hdfdim(3)
    real :: hdfread_2ddim(hdfdim(1),hdfdim(2))
    real*4:: hdfread_float32(hdfdim(2),hdfdim(1))
    integer(2):: hdfread_int16(hdfdim(2),hdfdim(1))
    integer(1) :: hdfread_int8(hdfdim(2),hdfdim(1))
    integer :: DFACC_READ
    integer :: sfstart, sfn2index, sfselect, sfginfo, sfrdata, sfendacc, sfend
    integer :: sd_id, sds_id, sds_index, getstatus
    integer :: n_rank, data_type
    integer :: n_attrs
    character :: name*(256)
    integer :: dim_sizes_dim(32)
    integer :: start(2), edges(2), stride(2)
    integer :: n_column,n_row
    parameter   (DFACC_READ = 1)
    ! initiating value
    hdfread_2ddim=-9999
    ! begin sds routine
    sd_id = sfstart(trim(filename), DFACC_READ)
    sds_index = sfn2index(sd_id, trim(variable))
    sds_id = sfselect(sd_id, sds_index)
    getstatus = sfginfo(sds_id, name, n_rank, dim_sizes_dim, data_type, n_attrs)
    ! define dimension
    n_column=dim_sizes_dim(1); n_row=dim_sizes_dim(2);
    start(1) = 0; start(2) = 0;
    edges(1) = dim_sizes_dim(1); edges(2) = dim_sizes_dim(2);
    write(*,*) 'rank:',n_rank,'dimension(1):',dim_sizes_dim(1),'dimension(2):',dim_sizes_dim(2)
    stride(1) = 1; stride(2) = 1;
    write(*,*) 'sd_id:',sd_id,'sds_index:',sds_index,'sds_id:',sds_id, &
    'getstatus:',getstatus,'edges:',edges,'datatype:',data_type

    if ((dim_sizes_dim(1)>0).AND.(dim_sizes_dim(2)>0)) then
        ! acquire data
        if (data_type==5) then
            hdfread_float32=-9999
            getstatus = sfrdata(sds_id, start, stride, edges, hdfread_float32)
            hdfread_2ddim=transpose((hdfread_float32))
        elseif (data_type==22) then
            hdfread_int16=-9999
            getstatus = sfrdata(sds_id, start, stride, edges, hdfread_int16)
            hdfread_2ddim=transpose(real(hdfread_int16))
        elseif (data_type==20) then
            hdfread_int8=-99
            getstatus = sfrdata(sds_id, start, stride, edges, hdfread_int8)
            hdfread_2ddim=transpose(real(hdfread_int8))
        end if

        if  (edges(2)<hdfdim(1)) then
            hdfread_2ddim(edges(2):hdfdim(1),:)=-9999
        end if
    end if
    !
    !close sds connection
    getstatus = sfendacc(sds_id)
    getstatus = sfend(sd_id)
end function
!
!
function hdfread_3d(filename,variable)
    ! variable
    character (len=*), intent(in) :: filename
    character (len=*), intent(in) :: variable
    integer*2 :: hdfread_3d(trmmlen,trmmwdth,trmmlev)
    integer*2:: hdfread_int16(trmmlev,trmmwdth,trmmlen)
    integer :: DFACC_READ
    integer :: sfstart, sfn2index, sfselect, sfginfo, sfrdata, sfendacc, sfend
    integer :: sd_id, sds_id, sds_index, getstatus
    integer :: n_rank, data_type
    integer :: n_attrs
    character :: name*(256)
    integer :: dim_sizes_dim(32)
    integer :: start(3), edges(3), stride(3)
    parameter   (DFACC_READ = 1)
    ! initiating value
    hdfread_3d=-9999
    ! begin sds routine
    sd_id = sfstart(trim(filename), DFACC_READ)
    sds_index = sfn2index(sd_id, trim(variable))
    sds_id = sfselect(sd_id, sds_index)
    getstatus = sfginfo(sds_id, name, n_rank, dim_sizes_dim, data_type, n_attrs)
    ! define dimension
    start(1) = 0; start(2) = 0;start(3) = 0;
    edges(1) = dim_sizes_dim(1); edges(2) = dim_sizes_dim(2);
    edges(3) = dim_sizes_dim(3);
    stride(1) = 1; stride(2) = 1;stride(3) = 1;
    write(*,*) 'rank:',n_rank,'dimension:',dim_sizes_dim(1:3)
    write(*,*) 'sd_id:',sd_id,'sds_index:',sds_index,'sds_id:',sds_id, &
    'getstatus:',getstatus,'edges:',edges,'datatype:',data_type
    ! acquire data
    if (data_type==22) then
		hdfread_int16=-9999
        getstatus = sfrdata(sds_id, start, stride, edges, hdfread_int16)
        hdfread_3d=transpose3d(hdfread_int16,trmmlev,trmmwdth,trmmlen);
        if (edges(3)<trmmlen) then
            hdfread_3d(edges(3):trmmlen,:,:)=-9999
        end if
    end if
    !close sds connection
    getstatus = sfendacc(sds_id)
    getstatus = sfend(sd_id)
end function
!
!
function hdfread_3ddim(filename,variable,hdfdim)
    ! variable
    character (len=*), intent(in) :: filename
    character (len=*), intent(in) :: variable
    integer :: hdfdim(3)
    integer*2 :: hdfread_3ddim(hdfdim(1),hdfdim(2),hdfdim(3))
    integer*2:: hdfread_int16(hdfdim(3),hdfdim(2),hdfdim(1))
    integer :: DFACC_READ
    integer :: sfstart, sfn2index, sfselect, sfginfo, sfrdata, sfendacc, sfend
    integer :: sd_id, sds_id, sds_index, getstatus
    integer :: n_rank, data_type
    integer :: n_attrs
    character :: name*(256)
    integer :: dim_sizes_dim(32)
    integer :: start(3), edges(3), stride(3)
    parameter   (DFACC_READ = 1)
    ! initiating value
    hdfread_3ddim=-9999
    ! begin sds routine
    sd_id = sfstart(trim(filename), DFACC_READ)
    sds_index = sfn2index(sd_id, trim(variable))
    sds_id = sfselect(sd_id, sds_index)
    getstatus = sfginfo(sds_id, name, n_rank, dim_sizes_dim, data_type, n_attrs)
    ! define dimension
    start(1) = 0; start(2) = 0;start(3) = 0;
    edges(1) = dim_sizes_dim(1); edges(2) = dim_sizes_dim(2);
    edges(3) = dim_sizes_dim(3);
    stride(1) = 1; stride(2) = 1;stride(3) = 1;
    write(*,*) 'rank:',n_rank,'dimension:',dim_sizes_dim(1:3)
    write(*,*) 'sd_id:',sd_id,'sds_index:',sds_index,'sds_id:',sds_id, &
    'getstatus:',getstatus,'edges:',edges,'datatype:',data_type
    !
    if ((dim_sizes_dim(1)>0).AND.(dim_sizes_dim(2)>0).AND.(dim_sizes_dim(3)>0)) then
    ! acquire data
    if (data_type==22) then
		hdfread_int16=-9999
        getstatus = sfrdata(sds_id, start, stride, edges, hdfread_int16)
        hdfread_3ddim=transpose3d(hdfread_int16,hdfdim(3),hdfdim(2),hdfdim(1));
        if (edges(3)<hdfdim(1)) then
            hdfread_3ddim(edges(3):hdfdim(1),:,:)=-9999
        end if
    end if
    end if
    !
    !close sds connection
    getstatus = sfendacc(sds_id)
    getstatus = sfend(sd_id)
end function



!------------------------------new hdfread subroutine -----------------------------------


subroutine hdfread_1d_int32(filename,variable,hdfdim,hdfout,debug,outstatus)
    !------------------------------------------------------------------------------------
    ! Function: read 1D hdf data with format integer32
    ! Example : call hdfread_1d_int32('tes.hdf','start',[1,2],outfile,debug,sds_stats)
    !------------------------------------------------------------------------------------
    ! variable
    character(len=*),intent(in)::filename
    character(len=*),intent(in)::variable
    integer,intent(in)::hdfdim
    integer*4::hdfout(hdfdim)
    integer::DFACC_READ
    integer::sfstart,sfn2index,sfselect,sfginfo,sfrdata,sfendacc,sfend
    integer::sd_id, sds_id, sds_index, getstatus
    integer::n_rank, data_type
    integer::n_attrs
    character::name*(256)
    integer::dim_sizes_dim(32)
    integer::start,edges,stride
    parameter (DFACC_READ = 1)
    integer::debug
    integer,intent(out)::outstatus

    ! initiating value
    outstatus=0
    hdfout=-9999

    ! begin sds routine
    if (debug>=1) write(*,*)''
    if (debug>=1) write(*,*)'------------------',trim(variable),'--------------------'

    ! identify filename sd
    sd_id=sfstart(trim(filename), DFACC_READ)
    if (debug>=1) write(*,*)'sd_id:',sd_id
    if (sd_id<0) return

    ! identify variable sds
    sds_index=sfn2index(sd_id, trim(variable))
    if (debug>=1) write(*,*)'sds_index:',sds_index
    if (sds_index<0) return

    ! identify variable id
    sds_id=sfselect(sd_id, sds_index)
    if (debug>=1) write(*,*)'sds_id:',sds_id
    if (sds_id<0) return

    ! obtain variable dimension
    getstatus=sfginfo(sds_id, name, n_rank, dim_sizes_dim, data_type, n_attrs)
    if (debug>=1) write(*,*)'getstatus:',getstatus

    ! define dimension
    start=0
    edges=dim_sizes_dim(1)
    stride=1
    if (debug>=1) then
        write(*,*)'rank:',n_rank
        write(*,*)'dimension:',dim_sizes_dim(1:2)
        write(*,*)'edges:',edges
        write(*,*)'datatype:',data_type
    end if

    ! read data
    if (data_type==24) then
        getstatus=sfrdata(sds_id,start,stride,edges,hdfout(1:edges))
        outstatus=1
    else
        write(*,*)'HDF FORMAT (REAL)  IS NOT VALID'
    end if

    !close sds connection
    getstatus = sfendacc(sds_id)
    getstatus = sfend(sd_id)
end subroutine


subroutine hdfread_1d_real(filename,variable,hdfdim,hdfout,debug,outstatus)
    !------------------------------------------------------------------------------------
    ! Function: read 1D hdf data with format integer32
    ! Example : call hdfread_1d_int32('tes.hdf','start',[1,2],outfile,debug,sds_stats)
    !------------------------------------------------------------------------------------
    ! variable
    character(len=*),intent(in)::filename
    character(len=*),intent(in)::variable
    integer,intent(in)::hdfdim
    real::hdfout(hdfdim)
    integer::DFACC_READ
    integer::sfstart,sfn2index,sfselect,sfginfo,sfrdata,sfendacc,sfend
    integer::sd_id, sds_id, sds_index, getstatus
    integer::n_rank, data_type
    integer::n_attrs
    character::name*(256)
    integer::dim_sizes_dim(32)
    integer::start,edges,stride
    parameter (DFACC_READ = 1)
    integer::debug
    integer,intent(out)::outstatus

    ! initiating value
    outstatus=0
    hdfout=-9999

    ! begin sds routine
    if (debug>=1) write(*,*)''
    if (debug>=1) write(*,*)'------------------',trim(variable),'--------------------'

    ! identify filename sd
    sd_id=sfstart(trim(filename), DFACC_READ)
    if (debug>=1) write(*,*)'sd_id:',sd_id
    if (sd_id<0) return

    ! identify variable sds
    sds_index=sfn2index(sd_id, trim(variable))
    if (debug>=1) write(*,*)'sds_index:',sds_index
    if (sds_index<0) return

    ! identify variable id
    sds_id=sfselect(sd_id, sds_index)
    if (debug>=1) write(*,*)'sds_id:',sds_id
    if (sds_id<0) return

    ! obtain variable dimension
    getstatus=sfginfo(sds_id, name, n_rank, dim_sizes_dim, data_type, n_attrs)
    if (debug>=1) write(*,*)'getstatus:',getstatus

    ! define dimension
    start=0
    edges=dim_sizes_dim(1)
    stride=1
    if (debug>=1) then
        write(*,*)'rank:',n_rank
        write(*,*)'dimension:',dim_sizes_dim(1:2)
        write(*,*)'edges:',edges
        write(*,*)'datatype:',data_type
    end if

    ! read data
    if (data_type==5) then
        getstatus=sfrdata(sds_id,start,stride,edges,hdfout(1:edges))
        outstatus=1
    else
        write(*,*)'HDF FORMAT (REAL)  IS NOT VALID'
    end if

    !close sds connection
    getstatus = sfendacc(sds_id)
    getstatus = sfend(sd_id)
end subroutine


subroutine hdfread_1d_dble(filename,variable,hdfdim,hdfout,debug,outstatus)
    !------------------------------------------------------------------------------------
    ! Function: read 1D hdf data with format integer32
    ! Example : call hdfread_1d_int32('tes.hdf','start',[1,2],outfile,debug,sds_stats)
    !------------------------------------------------------------------------------------
    ! variable
    character(len=*),intent(in)::filename
    character(len=*),intent(in)::variable
    integer,intent(in)::hdfdim
    real*8::hdfout(hdfdim)
    integer::DFACC_READ
    integer::sfstart,sfn2index,sfselect,sfginfo,sfrdata,sfendacc,sfend
    integer::sd_id, sds_id, sds_index, getstatus
    integer::n_rank, data_type
    integer::n_attrs
    character::name*(256)
    integer::dim_sizes_dim(32)
    integer::start,edges,stride
    parameter (DFACC_READ = 1)
    integer::debug
    integer,intent(out)::outstatus

    ! initiating value
    outstatus=0
    hdfout=-9999

    ! begin sds routine
    if (debug>=1) write(*,*)''
    if (debug>=1) write(*,*)'------------------',trim(variable),'--------------------'

    ! identify filename sd
    sd_id=sfstart(trim(filename), DFACC_READ)
    if (debug>=1) write(*,*)'sd_id:',sd_id
    if (sd_id<0) return

    ! identify variable sds
    sds_index=sfn2index(sd_id, trim(variable))
    if (debug>=1) write(*,*)'sds_index:',sds_index
    if (sds_index<0) return

    ! identify variable id
    sds_id=sfselect(sd_id, sds_index)
    if (debug>=1) write(*,*)'sds_id:',sds_id
    if (sds_id<0) return

    ! obtain variable dimension
    getstatus=sfginfo(sds_id, name, n_rank, dim_sizes_dim, data_type, n_attrs)
    if (debug>=1) write(*,*)'getstatus:',getstatus

    ! define dimension
    start=0
    edges=dim_sizes_dim(1)
    stride=1
    if (debug>=1) then
        write(*,*)'rank:',n_rank
        write(*,*)'dimension:',dim_sizes_dim(1:2)
        write(*,*)'edges:',edges
        write(*,*)'datatype:',data_type
    end if

    ! read data
    if (data_type==6) then
        getstatus=sfrdata(sds_id,start,stride,edges,hdfout(1:edges))
        outstatus=1
    else
        write(*,*)'HDF FORMAT (REAL)  IS NOT VALID'
    end if

    !close sds connection
    getstatus = sfendacc(sds_id)
    getstatus = sfend(sd_id)
end subroutine


subroutine hdfread_2d_int8(filename,variable,hdfdim,hdfout,debug,outstatus)
    !-------------------------------------------------------------
    ! Function: read 2D hdf data with format real
    ! Example : call hdfread_2d_real('tes.hdf','start',[1,2],outfile)
    !-------------------------------------------------------------
    ! variable
    character (len=*),intent(in)::filename
    character (len=*),intent(in)::variable
    integer::hdfdim(2)
    integer::debug
    integer*1::hdfout(hdfdim(1),hdfdim(2))
    integer::DFACC_READ
    integer::sfstart, sfn2index, sfselect, sfginfo, sfrdata, sfendacc, sfend
    integer::sd_id, sds_id, sds_index, getstatus
    integer::n_rank, data_type
    integer::n_attrs
    character::name*(256)
    integer::dim_sizes_dim(32)
    integer::start(2), edges(2), stride(2)
    parameter (DFACC_READ = 1)
    integer,intent(out)::outstatus

    ! initiating value
    outstatus=0
    hdfout=-99

    ! begin sds routine
    if (debug>=1) write(*,*)''
    if (debug>=1) write(*,*)'------------------',trim(variable),'--------------------'

    ! identify filename sd
    sd_id=sfstart(trim(filename), DFACC_READ)
    if (debug>=1) write(*,*)'sd_id:',sd_id
    if (sd_id<0) return

    ! identify variable sds
    sds_index=sfn2index(sd_id, trim(variable))
    if (debug>=1) write(*,*)'sds_index:',sds_index
    if (sds_index<0) return

    ! identify variable id
    sds_id=sfselect(sd_id, sds_index)
    if (debug>=1) write(*,*)'sds_id:',sds_id
    if (sds_id<0) return

    ! obtain variable dimension
    getstatus=sfginfo(sds_id, name, n_rank, dim_sizes_dim, data_type, n_attrs)
    if (debug>=1) write(*,*)'getstatus:',getstatus

    ! define dimension
    start(1)=0;start(2)=0;
    edges(1)=dim_sizes_dim(1);
    edges(2)=dim_sizes_dim(2);
    stride(1)=1;stride(2)=1;
    if (debug>=1) then
        write(*,*)'rank:',n_rank
        write(*,*)'dimension:',dim_sizes_dim(1:2)
        write(*,*)'edges:',edges
        write(*,*)'datatype:',data_type
    end if

    ! read data
    if (data_type==20) then
        getstatus=sfrdata(sds_id,start,stride,edges,hdfout(1:edges(1),1:edges(2)))
        outstatus=1
    else
        write(*,*)'HDF FORMAT (REAL)  IS NOT VALID'
    end if

    !close sds connection
    getstatus = sfendacc(sds_id)
    getstatus = sfend(sd_id)
end subroutine


subroutine hdfread_2d_int16(filename,variable,hdfdim,hdfout,debug,outstatus)
    !-------------------------------------------------------------
    ! Function: read 2D hdf data with format real
    ! Example : call hdfread_2d_real('tes.hdf','start',[1,2],outfile)
    !-------------------------------------------------------------
    ! variable
    character (len=*),intent(in)::filename
    character (len=*),intent(in)::variable
    integer::hdfdim(2)
    integer::debug
    integer*2::hdfout(hdfdim(1),hdfdim(2))
    integer::DFACC_READ
    integer::sfstart, sfn2index, sfselect, sfginfo, sfrdata, sfendacc, sfend
    integer::sd_id, sds_id, sds_index, getstatus
    integer::n_rank, data_type
    integer::n_attrs
    character::name*(256)
    integer::dim_sizes_dim(32)
    integer::start(2), edges(2), stride(2)
    parameter (DFACC_READ = 1)
    integer,intent(out)::outstatus

    ! initiating value
    outstatus=0
    hdfout=-9999

    ! begin sds routine
    if (debug>=1) write(*,*)''
    if (debug>=1) write(*,*)'------------------',trim(variable),'--------------------'

    ! identify filename sd
    sd_id=sfstart(trim(filename), DFACC_READ)
    if (debug>=1) write(*,*)'sd_id:',sd_id
    if (sd_id<0) return

    ! identify variable sds
    sds_index=sfn2index(sd_id, trim(variable))
    if (debug>=1) write(*,*)'sds_index:',sds_index
    if (sds_index<0) return

    ! identify variable id
    sds_id=sfselect(sd_id, sds_index)
    if (debug>=1) write(*,*)'sds_id:',sds_id
    if (sds_id<0) return

    ! obtain variable dimension
    getstatus=sfginfo(sds_id, name, n_rank, dim_sizes_dim, data_type, n_attrs)
    if (debug>=1) write(*,*)'getstatus:',getstatus

    ! define dimension
    start(1)=0;start(2)=0;
    edges(1)=dim_sizes_dim(1);
    edges(2)=dim_sizes_dim(2);
    stride(1)=1;stride(2)=1;
    if (debug>=1) then
        write(*,*)'rank:',n_rank
        write(*,*)'dimension:',dim_sizes_dim(1:2)
        write(*,*)'edges:',edges
        write(*,*)'datatype:',data_type
    end if

    ! read data
    if (data_type==22) then
        getstatus=sfrdata(sds_id,start,stride,edges,hdfout(1:edges(1),1:edges(2)))
        outstatus=1
    else
        write(*,*)'HDF FORMAT (REAL)  IS NOT VALID'
    end if

    !close sds connection
    getstatus = sfendacc(sds_id)
    getstatus = sfend(sd_id)
end subroutine


subroutine hdfread_2d_int32(filename,variable,hdfdim,hdfout,debug,outstatus)
    !-------------------------------------------------------------
    ! Function: read 2D hdf data with format real
    ! Example : call hdfread_2d_real('tes.hdf','start',[1,2],outfile)
    !-------------------------------------------------------------
    ! variable
    character (len=*),intent(in)::filename
    character (len=*),intent(in)::variable
    integer::hdfdim(2)
    integer::debug
    integer*4::hdfout(hdfdim(1),hdfdim(2))
    integer::DFACC_READ
    integer::sfstart, sfn2index, sfselect, sfginfo, sfrdata, sfendacc, sfend
    integer::sd_id, sds_id, sds_index, getstatus
    integer::n_rank, data_type
    integer::n_attrs
    character::name*(256)
    integer::dim_sizes_dim(32)
    integer::start(2), edges(2), stride(2)
    parameter (DFACC_READ = 1)
    integer,intent(out)::outstatus

    ! initiating value
    outstatus=0
    hdfout=-9999

    ! begin sds routine
    if (debug>=1) write(*,*)''
    if (debug>=1) write(*,*)'------------------',trim(variable),'--------------------'

    ! identify filename sd
    sd_id=sfstart(trim(filename), DFACC_READ)
    if (debug>=1) write(*,*)'sd_id:',sd_id
    if (sd_id<0) return

    ! identify variable sds
    sds_index=sfn2index(sd_id, trim(variable))
    if (debug>=1) write(*,*)'sds_index:',sds_index
    if (sds_index<0) return

    ! identify variable id
    sds_id=sfselect(sd_id, sds_index)
    if (debug>=1) write(*,*)'sds_id:',sds_id
    if (sds_id<0) return

    ! obtain variable dimension
    getstatus=sfginfo(sds_id, name, n_rank, dim_sizes_dim, data_type, n_attrs)
    if (debug>=1) write(*,*)'getstatus:',getstatus

    ! define dimension
    start(1)=0;start(2)=0;
    edges(1)=dim_sizes_dim(1);
    edges(2)=dim_sizes_dim(2);
    stride(1)=1;stride(2)=1;
    if (debug>=1) then
        write(*,*)'rank:',n_rank
        write(*,*)'dimension:',dim_sizes_dim(1:2)
        write(*,*)'edges:',edges
        write(*,*)'datatype:',data_type
    end if

    ! read data
    if (data_type==24) then
        getstatus=sfrdata(sds_id,start,stride,edges,hdfout(1:edges(1),1:edges(2)))
        outstatus=1
    else
        write(*,*)'HDF FORMAT (REAL)  IS NOT VALID'
    end if

    !close sds connection
    getstatus = sfendacc(sds_id)
    getstatus = sfend(sd_id)
end subroutine


subroutine hdfread_2d_real(filename,variable,hdfdim,hdfout,debug,outstatus)
    !-------------------------------------------------------------
    ! Function: read 2D hdf data with format real
    ! Example : call hdfread_2d_real('tes.hdf','start',[1,2],outfile)
    !-------------------------------------------------------------
    ! variable
    character (len=*),intent(in)::filename
    character (len=*),intent(in)::variable
    integer::hdfdim(2)
    integer::debug
    real::hdfout(hdfdim(1),hdfdim(2))
    integer::DFACC_READ
    integer::sfstart, sfn2index, sfselect, sfginfo, sfrdata, sfendacc, sfend
    integer::sd_id, sds_id, sds_index, getstatus
    integer::n_rank, data_type
    integer::n_attrs
    character::name*(256)
    integer::dim_sizes_dim(32)
    integer::start(2), edges(2), stride(2)
    parameter   (DFACC_READ = 1)
    integer,intent(out)::outstatus

    ! initiating value
    outstatus=0
    hdfout=-9999

    ! begin sds routine
    if (debug>=1) write(*,*)''
    if (debug>=1) write(*,*)'------------------',trim(variable),'--------------------'

    ! identify filename sd
    sd_id=sfstart(trim(filename), DFACC_READ)
    if (debug>=1) write(*,*)'sd_id:',sd_id
    if (sd_id<0) return

    ! identify variable sds
    sds_index=sfn2index(sd_id, trim(variable))
    if (debug>=1) write(*,*)'sds_index:',sds_index
    if (sds_index<0) return

    ! identify variable id
    sds_id=sfselect(sd_id, sds_index)
    if (debug>=1) write(*,*)'sds_id:',sds_id
    if (sds_id<0) return

    ! obtain variable dimension
    getstatus=sfginfo(sds_id, name, n_rank, dim_sizes_dim, data_type, n_attrs)
    if (debug>=1) write(*,*)'getstatus:',getstatus

    ! define dimension
    start(1)=0;start(2)=0;
    edges(1)=dim_sizes_dim(1);
    edges(2)=dim_sizes_dim(2);
    stride(1)=1;stride(2)=1;
    if (debug>=1) then
        write(*,*)'rank:',n_rank
        write(*,*)'dimension:',dim_sizes_dim(1:2)
        write(*,*)'edges:',edges
        write(*,*)'datatype:',data_type
    end if

    ! read data
    if (data_type==5) then
        getstatus=sfrdata(sds_id,start,stride,edges,hdfout(1:edges(1),1:edges(2)))
        outstatus=1
    else
        write(*,*)'HDF FORMAT (REAL)  IS NOT VALID'
    end if

    !close sds connection
    getstatus = sfendacc(sds_id)
    getstatus = sfend(sd_id)
end subroutine


subroutine hdfread_2d_dble(filename,variable,hdfdim,hdfout,debug,outstatus)
    !-------------------------------------------------------------
    ! Function: read 2D hdf data with format dble
    ! Example : call hdfread_2d_dble('tes.hdf','start',[1,2],outfile)
    !-------------------------------------------------------------
    ! variable
    character (len=*),intent(in)::filename
    character (len=*),intent(in)::variable
    integer::hdfdim(2)
    integer::debug
    real*8::hdfout(hdfdim(1),hdfdim(2))
    integer::DFACC_READ
    integer::sfstart, sfn2index, sfselect, sfginfo, sfrdata, sfendacc, sfend
    integer::sd_id, sds_id, sds_index, getstatus
    integer::n_rank, data_type
    integer::n_attrs
    character::name*(256)
    integer::dim_sizes_dim(32)
    integer::start(2), edges(2), stride(2)
    parameter   (DFACC_READ = 1)
    integer,intent(out)::outstatus

    ! initiating value
    outstatus=0
    hdfout=-9999

    ! begin sds routine
    if (debug>=1) write(*,*)''
    if (debug>=1) write(*,*)'------------------',trim(variable),'--------------------'

    ! identify filename sd
    sd_id=sfstart(trim(filename), DFACC_READ)
    if (debug>=1) write(*,*)'sd_id:',sd_id
    if (sd_id<0) return

    ! identify variable sds
    sds_index=sfn2index(sd_id, trim(variable))
    if (debug>=1) write(*,*)'sds_index:',sds_index
    if (sds_index<0) return

    ! identify variable id
    sds_id=sfselect(sd_id, sds_index)
    if (debug>=1) write(*,*)'sds_id:',sds_id
    if (sds_id<0) return

    ! obtain variable dimension
    getstatus=sfginfo(sds_id, name, n_rank, dim_sizes_dim, data_type, n_attrs)
    if (debug>=1) write(*,*)'getstatus:',getstatus

    ! define dimension
    start(1)=0;start(2)=0;
    edges(1)=dim_sizes_dim(1);
    edges(2)=dim_sizes_dim(2);
    stride(1)=1;stride(2)=1;
    if (debug>=1) then
        write(*,*)'rank:',n_rank
        write(*,*)'dimension:',dim_sizes_dim(1:2)
        write(*,*)'edges:',edges
        write(*,*)'datatype:',data_type
    end if

    ! read data
    if (data_type==6) then
        getstatus=sfrdata(sds_id,start,stride,edges,hdfout(1:edges(1),1:edges(2)))
        outstatus=1
    else
        write(*,*)'HDF FORMAT (DBLE)  IS NOT VALID'
    end if

    !close sds connection
    getstatus = sfendacc(sds_id)
    getstatus = sfend(sd_id)
end subroutine


subroutine hdfread_3d_int16(filename,variable,hdfdim,hdfout,debug,outstatus)
    !-------------------------------------------------------------
    ! Function: read 3D hdf data with format real
    ! Example : call hdfread_3d_int32('tes.hdf','start',[1,2,3],outfile)
    !-------------------------------------------------------------
    ! variable
    character (len=*),intent(in)::filename
    character (len=*),intent(in)::variable
    integer::hdfdim(3)
    integer::debug
    integer*2::hdfout(hdfdim(1),hdfdim(2),hdfdim(3))
    integer::DFACC_READ
    integer::sfstart, sfn2index, sfselect, sfginfo, sfrdata, sfendacc, sfend
    integer::sd_id, sds_id, sds_index, getstatus
    integer::n_rank, data_type
    integer::n_attrs
    character::name*(256)
    integer::dim_sizes_dim(32)
    integer::start(3), edges(3), stride(3)
    parameter   (DFACC_READ = 1)
    integer,intent(out)::outstatus

    ! initiating value
    outstatus=0
    hdfout=-9999

    ! begin sds routine
    if (debug>=1) write(*,*)''
    if (debug>=1) write(*,*)'------------------',trim(variable),'--------------------'

    ! identify filename sd
    sd_id=sfstart(trim(filename), DFACC_READ)
    if (debug>=1) write(*,*)'sd_id:',sd_id
    if (sd_id<0) return

    ! identify variable sds
    sds_index=sfn2index(sd_id, trim(variable))
    if (debug>=1) write(*,*)'sds_index:',sds_index
    if (sds_index<0) return

    ! identify variable id
    sds_id=sfselect(sd_id, sds_index)
    if (debug>=1) write(*,*)'sds_id:',sds_id
    if (sds_id<0) return

    ! obtain variable dimension
    getstatus=sfginfo(sds_id, name, n_rank, dim_sizes_dim, data_type, n_attrs)
    if (debug>=1) write(*,*)'getstatus:',getstatus

    ! define dimension
    start(1)=0;start(2)=0;start(3)=0;
    edges(1)=dim_sizes_dim(1);edges(2)=dim_sizes_dim(2);
    edges(3)=dim_sizes_dim(3);
    stride(1)=1;stride(2)=1;stride(3)=1;
    if (debug>=1) then
        write(*,*)'rank:',n_rank
        write(*,*)'dimension:',dim_sizes_dim(1:2)
        write(*,*)'edges:',edges
        write(*,*)'datatype:',data_type
    end if

    ! read data
    if (data_type==22) then
        getstatus=sfrdata(sds_id,start,stride,edges,hdfout(1:edges(1),1:edges(2),1:edges(3)))
        outstatus=1
    else
        write(*,*)'HDF FORMAT (INT32)  IS NOT VALID'
    end if

    !close sds connection
    getstatus = sfendacc(sds_id)
    getstatus = sfend(sd_id)
end subroutine


subroutine hdfread_3d_int32(filename,variable,hdfdim,hdfout,debug,outstatus)
    !-------------------------------------------------------------
    ! Function: read 3D hdf data with format real
    ! Example : call hdfread_3d_int32('tes.hdf','start',[1,2,3],outfile)
    !-------------------------------------------------------------
    ! variable
    character (len=*),intent(in)::filename
    character (len=*),intent(in)::variable
    integer::hdfdim(3)
    integer::debug
    integer*4::hdfout(hdfdim(1),hdfdim(2),hdfdim(3))
    integer::DFACC_READ
    integer::sfstart, sfn2index, sfselect, sfginfo, sfrdata, sfendacc, sfend
    integer::sd_id, sds_id, sds_index, getstatus
    integer::n_rank, data_type
    integer::n_attrs
    character::name*(256)
    integer::dim_sizes_dim(32)
    integer::start(3), edges(3), stride(3)
    parameter   (DFACC_READ = 1)
    integer,intent(out)::outstatus

    ! initiating value
    outstatus=0
    hdfout=-9999

    ! begin sds routine
    if (debug>=1) write(*,*)''
    if (debug>=1) write(*,*)'------------------',trim(variable),'--------------------'

    ! identify filename sd
    sd_id=sfstart(trim(filename), DFACC_READ)
    if (debug>=1) write(*,*)'sd_id:',sd_id
    if (sd_id<0) return

    ! identify variable sds
    sds_index=sfn2index(sd_id, trim(variable))
    if (debug>=1) write(*,*)'sds_index:',sds_index
    if (sds_index<0) return

    ! identify variable id
    sds_id=sfselect(sd_id, sds_index)
    if (debug>=1) write(*,*)'sds_id:',sds_id
    if (sds_id<0) return

    ! obtain variable dimension
    getstatus=sfginfo(sds_id, name, n_rank, dim_sizes_dim, data_type, n_attrs)
    if (debug>=1) write(*,*)'getstatus:',getstatus

    ! define dimension
    start(1)=0;start(2)=0;start(3)=0;
    edges(1)=dim_sizes_dim(1);edges(2)=dim_sizes_dim(2);
    edges(3)=dim_sizes_dim(3);
    stride(1)=1;stride(2)=1;stride(3)=1;
    if (debug>=1) then
        write(*,*)'rank:',n_rank
        write(*,*)'dimension:',dim_sizes_dim(1:2)
        write(*,*)'edges:',edges
        write(*,*)'datatype:',data_type
    end if

    ! read data
    if (data_type==24) then
        getstatus=sfrdata(sds_id,start,stride,edges,hdfout(1:edges(1),1:edges(2),1:edges(3)))
        outstatus=1
    else
        write(*,*)'HDF FORMAT (INT32)  IS NOT VALID'
    end if

    !close sds connection
    getstatus = sfendacc(sds_id)
    getstatus = sfend(sd_id)
end subroutine


subroutine hdfread_3d_real(filename,variable,hdfdim,hdfout,debug,outstatus)
    !-------------------------------------------------------------
    ! Function: read 3D hdf data with format real
    ! Example : call hdfread_3d_real('tes.hdf','start',[1,2,3],outfile)
    !-------------------------------------------------------------
    ! variable
    character (len=*),intent(in)::filename
    character (len=*),intent(in)::variable
    integer::hdfdim(3)
    integer::debug
    real::hdfout(hdfdim(1),hdfdim(2),hdfdim(3))
    integer::DFACC_READ
    integer::sfstart, sfn2index, sfselect, sfginfo, sfrdata, sfendacc, sfend
    integer::sd_id, sds_id, sds_index, getstatus
    integer::n_rank, data_type
    integer::n_attrs
    character::name*(256)
    integer::dim_sizes_dim(32)
    integer::start(3), edges(3), stride(3)
    parameter   (DFACC_READ = 1)
    integer,intent(out)::outstatus

    ! initiating value
    outstatus=0
    hdfout=-9999

    ! begin sds routine
    if (debug>=1) write(*,*)''
    if (debug>=1) write(*,*)'------------------',trim(variable),'--------------------'

    ! identify filename sd
    sd_id=sfstart(trim(filename), DFACC_READ)
    if (debug>=1) write(*,*)'sd_id:',sd_id
    if (sd_id<0) return

    ! identify variable sds
    sds_index=sfn2index(sd_id, trim(variable))
    if (debug>=1) write(*,*)'sds_index:',sds_index
    if (sds_index<0) return

    ! identify variable id
    sds_id=sfselect(sd_id, sds_index)
    if (debug>=1) write(*,*)'sds_id:',sds_id
    if (sds_id<0) return

    ! obtain variable dimension
    getstatus=sfginfo(sds_id, name, n_rank, dim_sizes_dim, data_type, n_attrs)
    if (debug>=1) write(*,*)'getstatus:',getstatus

    ! define dimension
    start(1)=0;start(2)=0;start(3)=0;
    edges(1)=dim_sizes_dim(1);
    edges(2)=dim_sizes_dim(2);
    edges(3)=dim_sizes_dim(3);
    stride(1)=1;stride(2)=1;stride(3)=1;
    if (debug>=1) then
        write(*,*)'rank:',n_rank
        write(*,*)'dimension:',dim_sizes_dim(1:2)
        write(*,*)'edges:',edges
        write(*,*)'datatype:',data_type
    end if

    ! read data
    if (data_type==5) then
        getstatus=sfrdata(sds_id,start,stride,edges,hdfout(1:edges(1),1:edges(2),1:edges(3)))
        outstatus=1
    else
        write(*,*)'HDF FORMAT (REAL)  IS NOT VALID'
    end if

    !close sds connection
    getstatus = sfendacc(sds_id)
    getstatus = sfend(sd_id)
end subroutine


subroutine hdfread_3d_dble(filename,variable,hdfdim,hdfout,debug,outstatus)
    !-------------------------------------------------------------
    ! Function: read 3D hdf data with format real
    ! Example : call hdfread_3d_dble('tes.hdf','start',[1,2,3],outfile)
    !-------------------------------------------------------------
    ! variable
    character (len=*),intent(in)::filename
    character (len=*),intent(in)::variable
    integer::hdfdim(3)
    integer::debug
    real*8::hdfout(hdfdim(1),hdfdim(2),hdfdim(3))
    integer::DFACC_READ
    integer::sfstart, sfn2index, sfselect, sfginfo, sfrdata, sfendacc, sfend
    integer::sd_id, sds_id, sds_index, getstatus
    integer::n_rank, data_type
    integer::n_attrs
    character::name*(256)
    integer::dim_sizes_dim(32)
    integer::start(3), edges(3), stride(3)
    parameter   (DFACC_READ = 1)
    integer,intent(out)::outstatus

    ! initiating value
    outstatus=0
    hdfout=-9999

    ! begin sds routine
    if (debug>=1) write(*,*)''
    if (debug>=1) write(*,*)'------------------',trim(variable),'--------------------'

    ! identify filename sd
    sd_id=sfstart(trim(filename), DFACC_READ)
    if (debug>=1) write(*,*)'sd_id:',sd_id
    if (sd_id<0) return

    ! identify variable sds
    sds_index=sfn2index(sd_id, trim(variable))
    if (debug>=1) write(*,*)'sds_index:',sds_index
    if (sds_index<0) return

    ! identify variable id
    sds_id=sfselect(sd_id, sds_index)
    if (debug>=1) write(*,*)'sds_id:',sds_id
    if (sds_id<0) return

    ! obtain variable dimension
    getstatus=sfginfo(sds_id, name, n_rank, dim_sizes_dim, data_type, n_attrs)
    if (debug>=1) write(*,*)'getstatus:',getstatus

    ! define dimension
    start(1)=0;start(2)=0;start(3)=0;
    edges(1)=dim_sizes_dim(1);
    edges(2)=dim_sizes_dim(2);
    edges(3)=dim_sizes_dim(3);
    stride(1)=1;stride(2)=1;stride(3)=1;
    if (debug>=1) then
        write(*,*)'rank:',n_rank
        write(*,*)'dimension:',dim_sizes_dim(1:2)
        write(*,*)'edges:',edges
        write(*,*)'datatype:',data_type
    end if

    ! read data
    if (data_type==6) then
        getstatus=sfrdata(sds_id,start,stride,edges,hdfout(1:edges(1),1:edges(2),1:edges(3)))
        outstatus=1
    else
        write(*,*)'HDF FORMAT (DBLE)  IS NOT VALID'
    end if

    !close sds connection
    getstatus = sfendacc(sds_id)
    getstatus = sfend(sd_id)
end subroutine


function hdfread_attr(filename,variable,charlen)
    !-------------------------------------------------------------
    ! Function: read hdf attribute in the main file
    ! Example : begintime=hdfread_1d('tes.hdf','start_time',14)
    !-------------------------------------------------------------
    ! variable
    character (len=*), intent(in) :: filename
    character (len=*), intent(in) :: variable
    integer, intent(in) :: charlen
    character (len=charlen):: hdfread_attr
    integer,parameter :: DFACC_READ=1
    integer*4 :: sd_id, istat
    integer :: attr_index
    integer sfstart, sffattr, sfselect, sfdimid
    integer sfrcatt, sfrnatt, sfendacc, sfend
    ! initiating value
    hdfread_attr='NaN'
    ! begin reading
    sd_id = sfstart(trim(filename), DFACC_READ)
    attr_index = sffattr(sd_id, trim(variable))
    istat = sfrcatt(sd_id, attr_index, hdfread_attr)
    !close sds connection
    istat = sfend(sd_id)
end function
!
!
function vdataread_1d(filename,variable,nrow,hdfdim)
    !-------------------------------------------------------------
    ! Function: read hdf Vdata with specified dimension
    ! Example : lat=vdataread_1d('tes.hdf','Latitude',38000,[3300,208,2])
    !-------------------------------------------------------------
    ! variable
    character (len=*) :: filename
    character (len=*) :: variable
    integer :: hdfdim(3)
    !
    character vdata_name*30, vdata_class*30, fields*60
    integer*4 :: file_id, vdata_ref, vdata_id, nrow
    integer*4 :: n_records, interlace, vdata_size
    real :: databuf(nrow)
    integer :: hopen, vsfgid, vsfatch, vsfinq, vsfgcls, vsffnd
    integer :: vsfsfld, vsfread, vsfdtch, hclose
    integer :: vsfnpak, vsfcpak, vfstart, vfend
    integer*4, parameter :: DFACC_READ=1, FULL_INTERLACE=1
    integer*4, parameter :: HDF_VSUNPACK=1, HDF_VSPACK=0
    integer :: istat, i
    real*8 :: vdataread_1d(nrow)
    !
    vdataread_1d=-9999
    !
    ! begin reading data information
    file_id=hopen(filename, DFACC_READ, 0)
    if (file_id>=1) then
        write(*,*) '---------------------'
        write(*,*) ''
        write(*,*)file_id
        write(*,*) ''
        write(*,*) '---------------------'
        ! Vset interface
        istat=vfstart(file_id)                       ! Initialize the Vset interface
        vdata_ref=vsffnd(file_id,variable)
        vdata_id=vsfatch(file_id, vdata_ref, 'r')  !Attach to the Vdata in read mode
        ! inquire data information
        istat=vsfinq(vdata_id,n_records,interlace,fields,vdata_size,vdata_name)
        if (n_records>0) then
        write(*,*) 'vdata_id    :',vdata_id
        write(*,*) 'vdata_ref   :',vdata_ref
        write(*,*) 'n_records   :',n_records
        write(*,*) 'interlace   :',interlace
        write(*,*) 'fields      :',fields
        write(*,*) 'vdata_size  :',vdata_size
        write(*,*) 'vdata_name  :',vdata_name
        write(*,*) ''
        ! reading data
        istat = vsfread(vdata_id,databuf,n_records,FULL_INTERLACE)
        databuf(n_records+1:nrow)=-9999
        vdataread_1d=databuf
    end if
    !
    !close connection
    istat = vsfdtch(vdata_id)
    istat = vfend(file_id)
    end if
    istat = hclose(file_id)

end function


function vdataread_0d(filename,variable)
    !-------------------------------------------------------------
    ! Function: read hdf Vdata with specified dimension
    ! Example : lat=vdataread_1d('tes.hdf','Latitude',38000,[3300,208,2])
    !-------------------------------------------------------------
    ! variable
    character (len=*) :: filename
    character (len=*) :: variable
    !
    character vdata_name*30, vdata_class*30, fields*60
    integer*4 :: file_id, vdata_ref, vdata_id, nrow
    integer*4 :: n_records, interlace, vdata_size
    real :: databuf
    integer :: hopen, vsfgid, vsfatch, vsfinq, vsfgcls, vsffnd
    integer :: vsfsfld, vsfread, vsfdtch, hclose
    integer :: vsfnpak, vsfcpak, vfstart, vfend
    integer*4, parameter :: DFACC_READ=1, FULL_INTERLACE=1
    integer*4, parameter :: HDF_VSUNPACK=1, HDF_VSPACK=0
    integer :: istat, i
    real*8 :: vdataread_0d
    !
    vdataread_0d=-9999
    !
    ! begin reading data information
    file_id=hopen(filename, DFACC_READ, 0)
    if (file_id>=1) then
    write(*,*) '---------------------'
    write(*,*) ''
    write(*,*)file_id
    write(*,*) ''
    write(*,*) '---------------------'
    ! Vset interface
    istat=vfstart(file_id)                       ! Initialize the Vset interface
    vdata_ref=vsffnd(file_id,variable)
    vdata_id=vsfatch(file_id, vdata_ref, 'r')  !Attach to the Vdata in read mode
    ! inquire data information
    istat=vsfinq(vdata_id,n_records,interlace,fields,vdata_size,vdata_name)
    if (n_records>0) then
    write(*,*) 'vdata_id    :',vdata_id
    write(*,*) 'vdata_ref   :',vdata_ref
    write(*,*) 'n_records   :',n_records
    write(*,*) 'interlace   :',interlace
    write(*,*) 'fields      :',fields
    write(*,*) 'vdata_size  :',vdata_size
    write(*,*) 'vdata_name  :',vdata_name
    write(*,*) ''
    ! reading data
    istat = vsfread(vdata_id,databuf,n_records,FULL_INTERLACE)
    vdataread_0d=databuf
    end if
    !
    !close connection
    istat = vsfdtch(vdata_id)
    istat = vfend(file_id)
    end if
    istat = hclose(file_id)

end function


function vdataread_1d_int8(filename,variable,nrow,hdfdim)
    !-------------------------------------------------------------
    ! Function: read hdf Vdata with specified dimension
    ! Example : lat=vdataread_1d('tes.hdf','Latitude',38000,[3300,208,2])
    !-------------------------------------------------------------
    ! variable
    character (len=*) :: filename
    character (len=*) :: variable
    integer :: hdfdim(3)
    !
    character vdata_name*30, vdata_class*30, fields*60
    integer*4 :: file_id, vdata_ref, vdata_id, nrow
    integer*4 :: n_records, interlace, vdata_size
    integer*1 :: databuf(nrow)
    integer :: hopen, vsfgid, vsfatch, vsfinq, vsfgcls, vsffnd
    integer :: vsfsfld, vsfread, vsfdtch, hclose
    integer :: vsfnpak, vsfcpak, vfstart, vfend
    integer*4, parameter :: DFACC_READ=1, FULL_INTERLACE=1
    integer*4, parameter :: HDF_VSUNPACK=1, HDF_VSPACK=0
    integer :: istat, i
    integer*1 :: vdataread_1d_int8(nrow)
    !
    vdataread_1d_int8=-99
    !
    ! begin reading data information
    file_id=hopen(filename, DFACC_READ, 0)
    if (file_id>=1) then
    write(*,*) '---------------------'
    write(*,*) ''
    write(*,*)file_id
    write(*,*) ''
    write(*,*) '---------------------'
    ! Vset interface
    istat=vfstart(file_id)                       ! Initialize the Vset interface
    vdata_ref=vsffnd(file_id,variable)
    vdata_id=vsfatch(file_id, vdata_ref, 'r')  !Attach to the Vdata in read mode
    ! inquire data information
    istat=vsfinq(vdata_id,n_records,interlace,fields,vdata_size,vdata_name)
    if (n_records>0) then
    write(*,*) 'vdata_id    :',vdata_id
    write(*,*) 'vdata_ref   :',vdata_ref
    write(*,*) 'n_records   :',n_records
    write(*,*) 'interlace   :',interlace
    write(*,*) 'fields      :',fields
    write(*,*) 'vdata_size  :',vdata_size
    write(*,*) 'vdata_name  :',vdata_name
    write(*,*) ''
    ! reading data
    istat = vsfread(vdata_id,databuf,n_records,FULL_INTERLACE)
    databuf(n_records+1:nrow)=-99
    vdataread_1d_int8=databuf
    end if
    !
    !close connection
    istat = vsfdtch(vdata_id)
    istat = vfend(file_id)
    end if
    istat = hclose(file_id)

end function



function vdataread_1d_int16(filename,variable,nrow,hdfdim)
    !-------------------------------------------------------------
    ! Function: read hdf Vdata with specified dimension
    ! Example : lat=vdataread_1d('tes.hdf','Latitude',38000,[3300,208,2])
    !-------------------------------------------------------------
    ! variable
    character (len=*) :: filename
    character (len=*) :: variable
    integer :: hdfdim(3)
    !
    character vdata_name*30, vdata_class*30, fields*60
    integer*4 :: file_id, vdata_ref, vdata_id, nrow
    integer*4 :: n_records, interlace, vdata_size
    integer*2 :: databuf(nrow)
    integer :: hopen, vsfgid, vsfatch, vsfinq, vsfgcls, vsffnd
    integer :: vsfsfld, vsfread, vsfdtch, hclose
    integer :: vsfnpak, vsfcpak, vfstart, vfend
    integer*4, parameter :: DFACC_READ=1, FULL_INTERLACE=1
    integer*4, parameter :: HDF_VSUNPACK=1, HDF_VSPACK=0
    integer :: istat, i
    integer*2 :: vdataread_1d_int16(nrow)
    !
    vdataread_1d_int16=-99
    !
    ! begin reading data information
    file_id=hopen(filename, DFACC_READ, 0)
    if (file_id>=1) then
    write(*,*) '---------------------'
    write(*,*) ''
    write(*,*)file_id
    write(*,*) ''
    write(*,*) '---------------------'
    ! Vset interface
    istat=vfstart(file_id)                       ! Initialize the Vset interface
    vdata_ref=vsffnd(file_id,variable)
    vdata_id=vsfatch(file_id, vdata_ref, 'r')  !Attach to the Vdata in read mode
    ! inquire data information
    istat=vsfinq(vdata_id,n_records,interlace,fields,vdata_size,vdata_name)
    if (n_records>0) then
    write(*,*) 'vdata_id    :',vdata_id
    write(*,*) 'vdata_ref   :',vdata_ref
    write(*,*) 'n_records   :',n_records
    write(*,*) 'interlace   :',interlace
    write(*,*) 'fields      :',fields
    write(*,*) 'vdata_size  :',vdata_size
    write(*,*) 'vdata_name  :',vdata_name
    write(*,*) ''
    ! reading data
    istat = vsfread(vdata_id,databuf,n_records,FULL_INTERLACE)
    databuf(n_records+1:nrow)=-99
    vdataread_1d_int16=databuf
    end if
    !
    !close connection
    istat = vsfdtch(vdata_id)
    istat = vfend(file_id)
    end if
    istat = hclose(file_id)

end function


subroutine vdataread1d_dble(filename,variable,nrow,outdata,outstatus)
    !-------------------------------------------------------------
    ! Function: read hdf Vdata with specified dimension
    ! Example : lat=vdataread_1d_dble('tes.hdf','Latitude',38000,[3300,208,2])
    !-------------------------------------------------------------

    ! variable
    character (len=*)::filename
    character (len=*)::variable
    character vdata_name*30,vdata_class*30,fields*60
    integer*4::file_id,vdata_ref,vdata_id,nrow
    integer*4::n_records,interlace,vdata_size
    real::databuf(nrow)
    integer::hopen,vsfgid,vsfatch,vsfinq,vsfgcls,vsffnd
    integer::vsfsfld,vsfread,vsfdtch,hclose
    integer::vsfnpak,vsfcpak,vfstart,vfend
    integer*4,parameter::DFACC_READ=1, FULL_INTERLACE=1
    integer*4,parameter::HDF_VSUNPACK=1, HDF_VSPACK=0
    integer::istat,i
    real*8::outdata(nrow)
    integer::outstatus

    ! initiate condition
    outstatus=0
    outdata=-9999

    ! begin reading data information
    file_id=hopen(filename, DFACC_READ, 0)

    ! check if fileID valid
    if (file_id>=1) then
        write(*,*) '---------------------'
        write(*,*) ''
        write(*,*)file_id
        write(*,*) ''
        write(*,*) '---------------------'

        ! Vset interface
        istat=vfstart(file_id)                       ! Initialize the Vset interface
        vdata_ref=vsffnd(file_id,variable)
        vdata_id=vsfatch(file_id, vdata_ref, 'r')  !Attach to the Vdata in read mode

        ! inquire data information
        istat=vsfinq(vdata_id,n_records,interlace,fields,vdata_size,vdata_name)

        ! display data info if valid
        if (n_records>0) then
            write(*,*) 'vdata_id    :',vdata_id
            write(*,*) 'vdata_ref   :',vdata_ref
            write(*,*) 'n_records   :',n_records
            write(*,*) 'interlace   :',interlace
            write(*,*) 'fields      :',fields
            write(*,*) 'vdata_size  :',vdata_size
            write(*,*) 'vdata_name  :',vdata_name
            write(*,*) ''

            ! reading data
            istat = vsfread(vdata_id,databuf,n_records,FULL_INTERLACE)
            databuf(n_records+1:nrow)=-9999
            outdata=databuf
            outstatus=1

            !close data connection
            istat = vsfdtch(vdata_id)
            istat = vfend(file_id)
        end if

        ! close file connection
        istat = hclose(file_id)

    end if
end subroutine



subroutine vdataread1d_int16(filename,variable,nrow,outdata,outstatus)
    !-------------------------------------------------------------
    ! Function: read hdf Vdata with specified dimension
    ! Example : lat=vdataread_1d('tes.hdf','Latitude',38000,[3300,208,2])
    !-------------------------------------------------------------
    ! variable
    character (len=*) :: filename
    character (len=*) :: variable
    integer :: hdfdim(3)
    !
    character vdata_name*30, vdata_class*30, fields*60
    integer*4 :: file_id, vdata_ref, vdata_id, nrow
    integer*4 :: n_records, interlace, vdata_size
    integer*2 :: databuf(nrow)
    integer :: hopen, vsfgid, vsfatch, vsfinq, vsfgcls, vsffnd
    integer :: vsfsfld, vsfread, vsfdtch, hclose
    integer :: vsfnpak, vsfcpak, vfstart, vfend
    integer*4, parameter :: DFACC_READ=1, FULL_INTERLACE=1
    integer*4, parameter :: HDF_VSUNPACK=1, HDF_VSPACK=0
    integer :: istat, i
    integer*2 :: outdata(nrow)
    integer::outstatus

    ! initiate output
    outdata=-99
    outstatus=0

    ! begin reading data information
    file_id=hopen(filename, DFACC_READ, 0)

    ! check if file exist
    if (file_id>=1) then

        ! notify status
        write(*,*) '---------------------'
        write(*,*) ''
        write(*,*)file_id
        write(*,*) ''
        write(*,*) '---------------------'

        ! Vset interface
        istat=vfstart(file_id)
        vdata_ref=vsffnd(file_id,variable)
        vdata_id=vsfatch(file_id, vdata_ref, 'r')

        ! inquire data information
        istat=vsfinq(vdata_id,n_records,interlace,fields,vdata_size,vdata_name)

        ! check if data valid
        if (n_records>0) then
            write(*,*) 'vdata_id    :',vdata_id
            write(*,*) 'vdata_ref   :',vdata_ref
            write(*,*) 'n_records   :',n_records
            write(*,*) 'interlace   :',interlace
            write(*,*) 'fields      :',fields
            write(*,*) 'vdata_size  :',vdata_size
            write(*,*) 'vdata_name  :',vdata_name
            write(*,*) ''

            ! reading data
            istat = vsfread(vdata_id,databuf,n_records,FULL_INTERLACE)
            databuf(n_records+1:nrow)=-99
            outdata=databuf
            outstatus=1
        end if

        !close connection
        istat = vsfdtch(vdata_id)
        istat = vfend(file_id)
    end if
    istat = hclose(file_id)

end subroutine



!---------------------- save HDF subroutine ------------------------
subroutine hdfcreate3d_2var(filename,var1,var2,rank,nlev,nrow,ncol)
    character (len=*), intent(in) :: filename
    integer :: nrow,ncol,nlev
    integer :: DFACC_CREATE, stddatatype
    parameter  (DFACC_CREATE=4, stddatatype=6)
    integer :: sfstart,sfend,sfcreate,sfendacc
    integer :: status
    integer :: rank
    integer :: sd_id,sds_id
    integer :: dim3d(rank)
    character (len=30) :: dataname
    character (len=20) :: var1,var2
    !
    ! create file
    sd_id = sfstart(filename, DFACC_CREATE)
    !
    ! write 1st data
    dataname=var1
    dim3d(1)=nlev;dim3d(2)=nrow;dim3d(3)=ncol;rank=3;
    sds_id = sfcreate(sd_id,trim(dataname),stddatatype,rank,dim3d)
    status = sfendacc(sds_id)
    !
    ! write 2nd data
    dataname=var2
    dim3d(1)=nlev;dim3d(2)=nrow;dim3d(3)=ncol;rank=3;
    sds_id = sfcreate(sd_id,trim(dataname),stddatatype,rank,dim3d)
    status = sfendacc(sds_id)
    !
    ! close file
    status = sfend(sd_id)
end subroutine
!
subroutine writehdf1d(filename,writename,datain,nrow,conversion)
	integer :: nrow
    character (len=*):: filename, writename
    real*8 :: datain(nrow)
    integer*2 ::datain_int16(nrow)
    integer  :: DFACC_WRITE
    parameter   (DFACC_WRITE=2)
    integer :: sfstart,sfcreate,sfendacc,sfend,sfn2index,sfselect,sfwdata
    integer :: sd_id,sds_id,sds_index
    integer :: dim1d,dim2d(2),dim3d(3),dim4d(4)
    integer :: status
    integer :: start1d,edges1d,stride1d
    integer :: start2d(2), edges2d(2), stride2d(2)
    integer :: start3d(3), edges3d(3), stride3d(3)
    integer :: start4d(4), edges4d(4), stride4d(4)
    integer,optional::conversion
    ! create sds inside sd
    sd_id = sfstart(trim(filename),DFACC_WRITE)
    sds_index = sfn2index(sd_id,trim(writename))
    sds_id  = sfselect(sd_id,sds_index)
    write(*,*) sds_index,sds_id
    ! define dimension
        start1d = 0
        edges1d = nrow
        stride1d = 1
        if (present(conversion)) then
            datain_int16=nint(datain)
            status = sfwdata(sds_id,start1d,stride1d,edges1d,datain_int16)
        else
            status = sfwdata(sds_id,start1d,stride1d,edges1d,datain)
        end if
    ! close connection
    status = sfendacc(sds_id)
    status = sfend(sd_id)
end subroutine


subroutine writehdf_1d_int(filename,writename,datain,nrow)
	integer :: nrow
    character (len=*):: filename, writename
    integer :: datain(nrow)
    integer,parameter  :: DFACC_WRITE=2
    integer :: sfstart,sfcreate,sfendacc,sfend,sfn2index,sfselect,sfwdata
    integer :: sd_id,sds_id,sds_index
    integer :: dim1d
    integer :: status
    integer :: start1d,edges1d,stride1d
    ! create sds inside sd
    sd_id = sfstart(trim(filename),DFACC_WRITE)
    sds_index = sfn2index(sd_id,trim(writename))
    sds_id  = sfselect(sd_id,sds_index)
    write(*,*) sds_index,sds_id
    ! define dimension
    start1d = 0
    edges1d = nrow
    stride1d = 1
    ! write data
    status = sfwdata(sds_id,start1d,stride1d,edges1d,datain)
    ! close connection
    status = sfendacc(sds_id)
    status = sfend(sd_id)
end subroutine


subroutine writehdf_1d_real(filename,writename,datain,nrow)
	integer :: nrow
    character (len=*):: filename, writename
    real :: datain(nrow)
    integer,parameter  :: DFACC_WRITE=2
    integer :: sfstart,sfcreate,sfendacc,sfend,sfn2index,sfselect,sfwdata
    integer :: sd_id,sds_id,sds_index
    integer :: dim1d
    integer :: status
    integer :: start1d,edges1d,stride1d
    ! create sds inside sd
    sd_id = sfstart(trim(filename),DFACC_WRITE)
    sds_index = sfn2index(sd_id,trim(writename))
    sds_id  = sfselect(sd_id,sds_index)
    write(*,*) sds_index,sds_id
    ! define dimension
    start1d = 0
    edges1d = nrow
    stride1d = 1
    ! write data
    status = sfwdata(sds_id,start1d,stride1d,edges1d,datain)
    ! close connection
    status = sfendacc(sds_id)
    status = sfend(sd_id)
end subroutine


subroutine writehdf_1d_dble(filename,writename,datain,nrow)
	integer :: nrow
    character (len=*):: filename, writename
    real*8 :: datain(nrow)
    integer,parameter  :: DFACC_WRITE=2
    integer :: sfstart,sfcreate,sfendacc,sfend,sfn2index,sfselect,sfwdata
    integer :: sd_id,sds_id,sds_index
    integer :: dim1d
    integer :: status
    integer :: start1d,edges1d,stride1d
    ! create sds inside sd
    sd_id = sfstart(trim(filename),DFACC_WRITE)
    sds_index = sfn2index(sd_id,trim(writename))
    sds_id  = sfselect(sd_id,sds_index)
    write(*,*) sds_index,sds_id
    ! define dimension
    start1d = 0
    edges1d = nrow
    stride1d = 1
    ! write data
    status = sfwdata(sds_id,start1d,stride1d,edges1d,datain)
    ! close connection
    status = sfendacc(sds_id)
    status = sfend(sd_id)
end subroutine


subroutine writehdf2d(filename,writename,datain,nrow,ncol,conversion)
	integer :: nrow,ncol
    character (len=*):: filename, writename
    real*8 :: datain(nrow,ncol)
    integer*2 ::datain_int16(nrow,ncol)
    integer  :: DFACC_WRITE
    parameter   (DFACC_WRITE=2)
    integer :: sfstart,sfcreate,sfendacc,sfend,sfn2index,sfselect,sfwdata
    integer :: sd_id,sds_id,sds_index
    integer :: dim2d(2)
    integer :: status
    integer :: start2d(2), edges2d(2), stride2d(2)
    integer,optional::conversion
    ! create sds inside sd
    sd_id = sfstart(trim(filename),DFACC_WRITE)
    sds_index = sfn2index(sd_id,trim(writename))
    sds_id  = sfselect(sd_id,sds_index)
    write(*,*) sds_index,sds_id
    ! define dimension
        start2d(1) = 0;start2d(2)=0
        edges2d(1) = nrow;edges2d(2)=ncol
        stride2d(1) = 1;stride2d(2)=1
        if (present(conversion)) then
            datain_int16=nint(datain)
            status = sfwdata(sds_id,start2d,stride2d,edges2d,datain_int16)
        else
            status = sfwdata(sds_id,start2d,stride2d,edges2d,datain)
        end if
    ! close connection
    status = sfendacc(sds_id)
    status = sfend(sd_id)
end subroutine


subroutine writehdf_2d_int(filename,writename,datain,nrow,ncol)
	integer :: nrow,ncol
    character (len=*):: filename, writename
    integer :: datain(nrow,ncol)
    integer*2 ::datain_int16(nrow,ncol)
    integer  :: DFACC_WRITE
    parameter   (DFACC_WRITE=2)
    integer :: sfstart,sfcreate,sfendacc,sfend,sfn2index,sfselect,sfwdata
    integer :: sd_id,sds_id,sds_index
    integer :: dim2d(2)
    integer :: status
    integer :: start2d(2), edges2d(2), stride2d(2)
    ! create sds inside sd
    sd_id = sfstart(trim(filename),DFACC_WRITE)
    sds_index = sfn2index(sd_id,trim(writename))
    sds_id  = sfselect(sd_id,sds_index)
    write(*,*) sds_index,sds_id
    ! define dimension
    start2d(1) = 0;start2d(2)=0
    edges2d(1) = nrow;edges2d(2)=ncol
    stride2d(1) = 1;stride2d(2)=1
    ! write data
    status = sfwdata(sds_id,start2d,stride2d,edges2d,datain)
    ! close connection
    status = sfendacc(sds_id)
    status = sfend(sd_id)
end subroutine


subroutine writehdf_2d_real(filename,writename,datain,nrow,ncol)
	integer :: nrow,ncol
    character (len=*):: filename, writename
    real :: datain(nrow,ncol)
    integer*2 ::datain_int16(nrow,ncol)
    integer  :: DFACC_WRITE
    parameter   (DFACC_WRITE=2)
    integer :: sfstart,sfcreate,sfendacc,sfend,sfn2index,sfselect,sfwdata
    integer :: sd_id,sds_id,sds_index
    integer :: dim2d(2)
    integer :: status
    integer :: start2d(2), edges2d(2), stride2d(2)
    ! create sds inside sd
    sd_id = sfstart(trim(filename),DFACC_WRITE)
    sds_index = sfn2index(sd_id,trim(writename))
    sds_id  = sfselect(sd_id,sds_index)
    write(*,*) sds_index,sds_id
    ! define dimension
    start2d(1) = 0;start2d(2)=0
    edges2d(1) = nrow;edges2d(2)=ncol
    stride2d(1) = 1;stride2d(2)=1
    ! write data
    status = sfwdata(sds_id,start2d,stride2d,edges2d,datain)
    ! close connection
    status = sfendacc(sds_id)
    status = sfend(sd_id)
end subroutine


subroutine writehdf_2d_dble(filename,writename,datain,nrow,ncol)
	integer :: nrow,ncol
    character (len=*):: filename, writename
    real*8 :: datain(nrow,ncol)
    integer*2 ::datain_int16(nrow,ncol)
    integer  :: DFACC_WRITE
    parameter   (DFACC_WRITE=2)
    integer :: sfstart,sfcreate,sfendacc,sfend,sfn2index,sfselect,sfwdata
    integer :: sd_id,sds_id,sds_index
    integer :: dim2d(2)
    integer :: status
    integer :: start2d(2), edges2d(2), stride2d(2)
    ! create sds inside sd
    sd_id = sfstart(trim(filename),DFACC_WRITE)
    sds_index = sfn2index(sd_id,trim(writename))
    sds_id  = sfselect(sd_id,sds_index)
    write(*,*) sds_index,sds_id
    ! define dimension
    start2d(1) = 0;start2d(2)=0
    edges2d(1) = nrow;edges2d(2)=ncol
    stride2d(1) = 1;stride2d(2)=1
    ! write data
    status = sfwdata(sds_id,start2d,stride2d,edges2d,datain)
    ! close connection
    status = sfendacc(sds_id)
    status = sfend(sd_id)
end subroutine


subroutine writehdf2d_char(filename,writename,datain)
    character (len=*)::filename, writename
    character(len=*)::datain(:,:)
    integer  :: DFACC_WRITE
    parameter   (DFACC_WRITE=2)
    integer :: sfstart,sfcreate,sfendacc,sfend,sfn2index,sfselect,sfwdata
    integer :: sd_id,sds_id,sds_index
    integer :: dim2d(2)
    integer :: status
    integer :: start2d(2),edges2d(2),stride2d(2)
    ! create sds inside sd
    sd_id=sfstart(trim(filename),DFACC_WRITE)
    sds_index=sfn2index(sd_id,trim(writename))
    sds_id=sfselect(sd_id,sds_index)
    write(*,*)sds_index,sds_id
    ! define dimension
    start2d=0
    edges2d(1)=size(datain,1)
    edges2d(2)=size(datain,2)
    stride2d=1
    ! write data
    status = sfwdata(sds_id,start2d,stride2d,edges2d,datain)
    ! close connection
    status = sfendacc(sds_id)
    status = sfend(sd_id)
end subroutine


subroutine writehdf3d(filename,writename,datain,nlev,nrow,ncol,conversion)
	integer :: nrow,ncol,nlev
    character (len=*):: filename, writename
    real*8 :: datain(nlev,nrow,ncol)
    integer*2 ::datain_int16(nlev,nrow,ncol)
    integer*4 ::datain_int32(nlev,nrow,ncol)
    integer  :: DFACC_WRITE
    parameter   (DFACC_WRITE=2)
    integer :: sfstart,sfcreate,sfendacc,sfend,sfn2index,sfselect,sfwdata
    integer :: sd_id,sds_id,sds_index
    integer :: dim2d(2),dim3d(3),dim4d(4)
    integer :: status
    integer :: start2d(2), edges2d(2), stride2d(2)
    integer :: start3d(3), edges3d(3), stride3d(3)
    integer :: start4d(4), edges4d(4), stride4d(4)
    integer,optional::conversion
    ! create sds inside sd
    sd_id = sfstart(trim(filename),DFACC_WRITE)
    sds_index = sfn2index(sd_id,trim(writename))
    sds_id  = sfselect(sd_id,sds_index)
    write(*,*) sds_index,sds_id
    ! define dimension
        start3d(1) = 0;start3d(2)=0;start3d(3)=0;
        edges3d(1) = nlev;edges3d(2)=nrow;edges3d(3)=ncol;
        stride3d(1) = 1;stride3d(2)=1;stride3d(3)=1;
        if (present(conversion)) then
            if (conversion==1) then
                datain_int16=nint(datain)
                status = sfwdata(sds_id,start3d,stride3d,edges3d,datain_int16)
            elseif (conversion==2) then
                datain_int32=nint(datain)
                status = sfwdata(sds_id,start3d,stride3d,edges3d,datain_int32)
            end if
        else
            status = sfwdata(sds_id,start3d,stride3d,edges3d,datain)
        end if
    ! close connection
    status = sfendacc(sds_id)
    status = sfend(sd_id)
end subroutine


subroutine writehdf3d_real(filename,writename,datain)
    character (len=*):: filename, writename
    real :: datain(:,:,:)
    integer  :: DFACC_WRITE
    parameter   (DFACC_WRITE=2)
    integer :: sfstart,sfcreate,sfendacc,sfend,sfn2index,sfselect,sfwdata
    integer :: sd_id,sds_id,sds_index
    integer :: dim2d(2),dim3d(3),dim4d(4)
    integer :: status
    integer :: start3d(3), edges3d(3), stride3d(3)
    ! create sds inside sd
    sd_id = sfstart(trim(filename),DFACC_WRITE)
    sds_index = sfn2index(sd_id,trim(writename))
    sds_id  = sfselect(sd_id,sds_index)
    write(*,*) sds_index,sds_id
    ! define dimension
    start3d=0
    edges3d(1)=size(datain,1)
    edges3d(2)=size(datain,2)
    edges3d(3)=size(datain,3)
    stride3d=1
    ! write data
    status = sfwdata(sds_id,start3d,stride3d,edges3d,datain)
    ! close connection
    status = sfendacc(sds_id)
    status = sfend(sd_id)
end subroutine


subroutine writehdf3d_dble(filename,writename,datain)
    character (len=*):: filename, writename
    real*8 :: datain(:,:,:)
    integer  :: DFACC_WRITE
    parameter   (DFACC_WRITE=2)
    integer :: sfstart,sfcreate,sfendacc,sfend,sfn2index,sfselect,sfwdata
    integer :: sd_id,sds_id,sds_index
    integer :: dim2d(2),dim3d(3),dim4d(4)
    integer :: status
    integer :: start3d(3), edges3d(3), stride3d(3)
    ! create sds inside sd
    sd_id = sfstart(trim(filename),DFACC_WRITE)
    sds_index = sfn2index(sd_id,trim(writename))
    sds_id  = sfselect(sd_id,sds_index)
    write(*,*) sds_index,sds_id
    ! define dimension
    start3d=0
    edges3d(1)=size(datain,1)
    edges3d(2)=size(datain,2)
    edges3d(3)=size(datain,3)
    stride3d=1
    ! write data
    status = sfwdata(sds_id,start3d,stride3d,edges3d,datain)
    ! close connection
    status = sfendacc(sds_id)
    status = sfend(sd_id)
end subroutine


subroutine writehdf3d_int(filename,writename,datain)
    character (len=*):: filename, writename
    integer :: datain(:,:,:)
    integer  :: DFACC_WRITE
    parameter   (DFACC_WRITE=2)
    integer :: sfstart,sfcreate,sfendacc,sfend,sfn2index,sfselect,sfwdata
    integer :: sd_id,sds_id,sds_index
    integer :: status
    integer :: start3d(3), edges3d(3), stride3d(3)

    ! create sds inside sd
    sd_id = sfstart(trim(filename),DFACC_WRITE)
    sds_index = sfn2index(sd_id,trim(writename))
    sds_id  = sfselect(sd_id,sds_index)
    write(*,*) sds_index,sds_id

    ! define dimension
    start3d=0
    edges3d(1)=size(datain,1)
    edges3d(2)=size(datain,2)
    edges3d(3)=size(datain,3)
    stride3d=1

    ! write data
    status = sfwdata(sds_id,start3d,stride3d,edges3d,datain)

    ! close connection
    status = sfendacc(sds_id)
    status = sfend(sd_id)
end subroutine


subroutine writehdf5d_real(filename,writename,datain)
    character(len=*)::filename,writename
    real::datain(:,:,:,:,:)
    integer,parameter::DFACC_WRITE=2
    integer::sfstart,sfcreate,sfendacc,sfend,sfn2index,sfselect,sfwdata
    integer::sd_id,sds_id,sds_index
    integer::dim5d(5)
    integer::status
    integer::start5d(5), edges5d(5), stride5d(5)

    ! create sds inside sd
    sd_id = sfstart(trim(filename),DFACC_WRITE)
    sds_index = sfn2index(sd_id,trim(writename))
    sds_id  = sfselect(sd_id,sds_index)
    write(*,*) sds_index,sds_id

    ! define dimension
    start5d=0
    stride5d=1
    edges5d(1)=size(datain,1)
    edges5d(2)=size(datain,2)
    edges5d(3)=size(datain,3)
    edges5d(4)=size(datain,4)
    edges5d(5)=size(datain,5)

    ! write data
    status = sfwdata(sds_id,start5d,stride5d,edges5d,datain)

    ! close connection
    status = sfendacc(sds_id)
    status = sfend(sd_id)
end subroutine


subroutine writehdf5d_int(filename,writename,datain)
    character(len=*)::filename,writename
    integer::datain(:,:,:,:,:)
    integer,parameter::DFACC_WRITE=2
    integer::sfstart,sfcreate,sfendacc,sfend,sfn2index,sfselect,sfwdata
    integer::sd_id,sds_id,sds_index
    integer::dim5d(5)
    integer::status
    integer::start5d(5), edges5d(5), stride5d(5)

    ! create sds inside sd
    sd_id = sfstart(trim(filename),DFACC_WRITE)
    sds_index = sfn2index(sd_id,trim(writename))
    sds_id  = sfselect(sd_id,sds_index)
    write(*,*) sds_index,sds_id

    ! define dimension
    start5d=0
    stride5d=1
    edges5d(1)=size(datain,1)
    edges5d(2)=size(datain,2)
    edges5d(3)=size(datain,3)
    edges5d(4)=size(datain,4)
    edges5d(5)=size(datain,5)

    ! write data
    status = sfwdata(sds_id,start5d,stride5d,edges5d,datain)

    ! close connection
    status = sfendacc(sds_id)
    status = sfend(sd_id)
end subroutine


subroutine writehdfattr(filename,attribute)
    character (len=*):: filename
    character (len=*):: attribute
    integer  :: DFACC_WRITE,DFNT_CHAR8
    parameter   (DFACC_WRITE=2,DFNT_CHAR8=4)
    integer :: sfstart,sfendacc,sfend,sfscatt
    integer :: sd_id
    integer :: status
    character (len=25) :: attr_name
    integer :: n_values
    ! identify sds
    sd_id = sfstart(trim(filename),DFACC_WRITE)
    ! define dimension
    attr_name='dimension'
    n_values=150
    status = sfscatt(sd_id,attr_name,DFNT_CHAR8,n_values,attribute)
    ! close connection
    status = sfend(sd_id)
end subroutine


function transpose3d_dble(datain,size_y,size_x,size_z);
    integer, intent(in) :: size_x,size_y,size_z
    integer :: i,j,k
    real*8 :: datain(size_y,size_x,size_z)
    real*8 :: transpose3d_dble(size_z,size_x,size_y)
    do i=1,size_y
	do j=1,size_x
	do k=1,size_z
        transpose3d_dble(k,j,i)=datain(i,j,k)
    end do
    end do
    end do
end function


function transpose3d_real(datain,size_y,size_x,size_z);
    integer, intent(in) :: size_x,size_y,size_z
    integer :: i,j,k
    real :: datain(size_y,size_x,size_z)
    real :: transpose3d_real(size_z,size_x,size_y)
    do i=1,size_y
	do j=1,size_x
	do k=1,size_z
        transpose3d_real(k,j,i)=datain(i,j,k)
    end do
    end do
    end do
end function


function transpose3d_int16(datain,size_y,size_x,size_z);
    integer, intent(in) :: size_x,size_y,size_z
    integer :: i,j,k
    integer*2 :: datain(size_y,size_x,size_z)
    integer*2 :: transpose3d_int16(size_z,size_x,size_y)
    do i=1,size_y
	do j=1,size_x
	do k=1,size_z
        transpose3d_int16(k,j,i)=datain(i,j,k)
    end do
    end do
    end do
end function


function transpose3d_int32(datain,size_y,size_x,size_z);
    integer, intent(in) :: size_x,size_y,size_z
    integer :: i,j,k
    integer*4 :: datain(size_y,size_x,size_z)
    integer*4 :: transpose3d_int32(size_z,size_x,size_y)
    do i=1,size_y
	do j=1,size_x
	do k=1,size_z
        transpose3d_int32(k,j,i)=datain(i,j,k)
    end do
    end do
    end do
end function
!
!
end module hdfmodule
