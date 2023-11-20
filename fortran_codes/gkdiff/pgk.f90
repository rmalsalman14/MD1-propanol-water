program vis
 implicit none
  integer,parameter         :: r8b= kind(1.D200)
  integer,parameter         :: i4b= kind(2147483647)
character(70)::m
write(*,*)"*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*"
write(*,*)"*                * By: Rkan M Al-Salman *                      *"
write(*,*)"*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*"

call avr1

m="p_intgk.txt"

call intx(m)
!---------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------
contains
!.............................................................................
subroutine avr1
  implicit none
  integer,parameter         :: r8b= kind(1.D200)
  integer,parameter         :: i4b= kind(2147483647)

character(70)::m1,datf
integer(i4b)::i,r,tmax,j
real(r8b), allocatable :: h(:,:)
real(r8b) :: sumr,x1,x2


r=5
200 format (2(f14.4,x)) !r+2
tmax=25018-17

allocate(h(tmax,r))

do i=1,r
if (i<10) then
write(m1,"('pv',I1.1,'.xvg')")i
else
write(m1,"('pv',I2.2,'.xvg')")i
endif

open(i+20, file=m1, status='old')
do j=1,17
 read (i+20,*)
enddo

do j=1,tmax
 read (i+20,*) x1,x2
 h(j,i)=x2
enddo
CLOSE (i+20) 
enddo

datf="p_avrgk.txt"
open(52,file=datf,action="write")

do j=1,tmax
sumr=0
do i=1,r
sumr=sumr+h(j,i)
enddo
write (52,200) j*0.002,sumr/(r*1.) !,h(j,:)
enddo     
CLOSE (52)
deallocate(h)
end subroutine avr1

!---------------------------------------------------------------------------------------------
subroutine intx(dat1)
  implicit none
  integer,parameter         :: r8b= kind(1.D200)
  integer,parameter         :: i4b= kind(2147483647)
character(70),intent(in)::dat1
character(70)::file2
integer(i4b) ::kloop,n,r
real(r8b)::h,const,s

200 format (2(f8.4,x))

!write(*,*)"input inteval of acf in ps:"

r=25001
const=(1.0)/(0.003)
open(58,file=dat1,action="write")
do n=1,r,100
  kloop=n
  file2="p_avrgk.txt"
  s=((n*1.0)/(r*1.0))*100
  if (s-int(s)<0.00000001) write(*,*) "done"
  call hk(kloop,h,file2)
  write (58,200) (kloop-1)*2./1000.,h*const
enddo
CLOSE (58)
end subroutine intx
!---------------------------------------------------------------------------------------------
subroutine hk(n,sum,file1) 
    integer,parameter         :: r8b= kind(1.D200)
    integer,parameter         :: i4b= kind(2147483647)  
  real(r8b),intent(out)::sum
  character(70),intent(in)::file1
  integer(i4b),intent(in)::n
  integer(i4b) :: i
  real(r8b), allocatable :: x(:,:),h(:,:)

  allocate(x(N,2),h(N,2))
   
  open(17, file=file1, status='old')
  do i=1,n
      read (17,*) x(i,1),x(i,2) 
  enddo
  CLOSE (17)
 
  do i=1,n
     h(i,1)=x(i,1)
     h(i,2)=x(i,2)
  enddo  
  close(15)
     
  sum=0
  do i=1,n-1
         sum=sum+0.5*0.002*(h((i+1),2)+h(i,2))              
  end do
 deallocate(x,h)
 end subroutine
!---------------------------------------------------------------------------------------------

end program vis
