program st
 implicit none
  integer,parameter         :: r8b= kind(1.D200)
  integer,parameter         :: i4b= kind(2147483647)
character(70)::f,m
integer(i4b)::i
write(*,*)"*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*"
write(*,*)"*                * By: Rkan M Al-Salman *                      *"
write(*,*)"*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*"

call avr

!---------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------
contains

!---------------------------------------------------------------------------------------------
subroutine avr
  implicit none
  integer,parameter         :: r8b= kind(1.D200)
  integer,parameter         :: i4b= kind(2147483647)

character(70)::m1,datf
integer(i4b)::j,r,tmax,k,mm,dd
real(r8b), allocatable :: h(:,:),av(:)
!real(r8b):: 
r=2
200 format (I0.0,x,10(f7.3,x)) 
201 format (A,x,10(f7.3,x)) 

tmax=2500025-24

allocate(h(tmax,r),av(10))

do dd=1,11
  if (dd==1) k=0
  if (dd==2) k=48 
  if (dd==3) k=120
  if (dd==4) k=180
  if (dd==5) k=240 
  if (dd==6) k=300
  if (dd==7) k=360 
  if (dd==8) k=426
  if (dd==9) k=480
  if (dd==10) k=534
  if (dd==11) k=600
  if (dd==1) then
    write(m1,"('0/s/st.xvg')")
  else  
  write(m1,"(I0.0,'/s/st.xvg')")k
  endif
  write(*,*)m1

  open(20, file=m1, status='old')
do j=1,24
 read (20,*)
enddo
do j=1,tmax
 read (20,*) h(j,2),h(j,1)
enddo
CLOSE (20) 


datf="sten.txt"
open(52,file=datf,action="write")
av=0
do mm=1,10
do j=((mm-1)*250000)+1,mm*250000
av(mm)=av(mm)+h(j,1)
enddo     
av(mm)=av(mm)/((20.0*tmax)/10.0)
enddo

if (dd==1) then
write (52,201) "0",av(:) 
else
write (52,200)k,av(:) 
endif
write (*,*)k
enddo

CLOSE (52)

deallocate(h,av)
end subroutine avr
!.............................................................................

end program st
