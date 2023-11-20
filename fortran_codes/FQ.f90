implicit none
integer,parameter         :: r8b= kind(1.D200)
integer,parameter         :: i4b= kind(2147483647)
character(70)::Hdk,file1
integer(i4b) ::kloop,n,j,i,nf,cc,n1,n2,ii,c,np,nw,jj
real(r8b)::k,f,roh,xb,cp,cw,xb1,xb2
real(r8b),parameter::PI = 3.141592653589793238462643d0 ,dr=0.01
real(r8b), allocatable :: Hd(:,:),fi(:),h(:)


write (*,*) "for 1-propanol-water system"
write (*,*) "****choos system:*************** "
write (*,*) "1. propanol+water  "
write (*,*) "2. pure water   "
write (*,*) "3. pure propanol  "
write (*,*) "******************************** "
read(*,*) c
write (*,*) c

open(11, file='b.txt', status='old')
read (11,*) n
read (11,*) xb,xb1,xb2


write (*,*) "insert x-box value: "
!read(*,*) xb
write (*,*) xb

write (*,*) "insert xvg total lines "
!read(*,*) n
write (*,*) n
n=n-25

if (c==1) then
    write (*,*) "insert # of propanol: "
    read(*,*) np
    write (*,*) np
    nw=600-np
elseif (c==2) then
    np=0
    nw=600
elseif (c==3) then
    np=600
    nw=0
endif
roh=((12.0*np)+(3.0*nw))/(1000*(xb**3))

cp=(np*1.0)/((12.0*np)+(3.0*nw))
cw=(nw*1.0)/((12.0*np)+(3.0*nw))

write (*,*) "*************start of calculatin --- wait---- ************ "
write (*,*) "#pro=",np,"#wat=",nw
write (*,*) "c_pro=",cp,"c_wat=",cw
write(*,*) "#density=",roh
!*************************************************************************************
if (c==1) cc=10
if (c==2) cc=2
if (c==3) cc=8
nf=0

do i=1,cc
nf=nf+i
enddo
allocate(Hd(1001,2),fi(cc),h(nf))

Hdk="Sx.txt"         
open(56,file=Hdk,action="write")

j=2

do kloop=1,1001
f=0
i=1
k=(kloop-1)*0.01 

if (cc==10) then
    fi(1)=cp*ffc(k)
    fi(2)=cp*3*ffh(k)
    fi(3)=cp*ffc(k)
    fi(4)=cp*2*ffh(k)
    fi(5)=cp*ffc(k)
    fi(6)=cp*2*ffh(k)
    fi(7)=cp*ffo(k)
    fi(8)=cp*ffh(k)
    fi(9)=cw*ffo(k)
    fi(10)=cw*2*ffh(k)
elseif (cc==2) then
    fi(1)=cw*ffo(k)
    fi(2)=cw*2*ffh(k)
elseif (cc==8) then
    fi(1)=cp*ffc(k)
    fi(2)=cp*3*ffh(k)
    fi(3)=cp*ffc(k)
    fi(4)=cp*2*ffh(k)
    fi(5)=cp*ffc(k)
    fi(6)=cp*2*ffh(k)
    fi(7)=cp*ffo(k)
    fi(8)=cp*ffh(k)
endif

do ii=1,cc
    do jj=1,cc
f=f+fi(ii)*fi(jj)
enddo
enddo
!write (*,*) f

do n1=1,cc
do n2=n1,cc
write(file1,"(I0.0,'-',I0.0,'.xvg')")n1,n2 
call hk(n,k,h(i),file1,j,roh)
if (n1==n2)then
h(i)=h(i)*fi(n1)*fi(n2)

else
h(i)=h(i)*fi(n1)*fi(n2)*2.0
endif 
i=i+1

enddo
enddo


Hd(kloop,1)=K
if (kloop==1) then
Hd(kloop,j)=0
else
do i=1,nf    
Hd(kloop,j)=Hd(kloop,j)+(h(i)/f)
enddo
endif 
enddo
 

120 format (2(F7.3,3x))
do kloop=2,1001
write (56,120) Hd(kloop,:)   
enddo 
write (*,*) "*************done ************ "

deallocate(Hd,fi,h)

contains
!-----------------------------------------------------------------------    
subroutine hk(n,k,sum,file1,j,roh)   
real(r8b),intent(out)::sum
character(70),intent(in)::file1
real(r8b),intent(in)::k,roh
integer(i4b),intent(in)::n,j
integer(i4b) :: i
real(r8b), allocatable :: x(:,:),g(:,:),h(:,:)
allocate(g(N,2),h(N,2))
allocate(x(N,2))
open(15, file=file1, status='old')
do i=1,25
read (15,*) 
enddo
do i=1,n
read (15,*) x(i,:)
enddo
close(15)

do i=1,n
g(i,1)=10.0*x(i,1)   
g(i,2)=x(i,J)  
h(i,1)=g(i,1)
h(i,2)=f1(k,g(i,1),g(i,2),roh)
enddo

sum=1
do i=1,n-1

sum=sum+0.5*(h((i+1),1)-h(i,1))*(h((i+1),2)+h(i,2))              
end do
deallocate(g,h)
deallocate(x)
end subroutine
!-------------------------------------------------------------------
real (r8b) function f1(k,r,g,roh)
real(r8b),intent(in)::k,r,g,roh

f1=sin(k*r)/(k)
f1=f1*(4*roh*pi*(r)*(g-1))

end function f1 
!------------------------------------------------------------------- 
real (r8b) function ffc(k1)
real(r8b),intent(in)::k1
real(r8b)::a1,a2,a3,a4,a5,c,k
k=k1/(4*pi)
a1= 2.657506*exp(-1.0*(14.780758)*(k**2))
a2= 1.078079*exp(-1.0*(0.776775)*(k**2))
a3= 1.490909*exp(-1.0*(42.086843)*(k**2))
a4= -4.241070*exp(-1.0*(-0.000294)*(k**2))
a5= 0.713791*exp(-1.0*(0.239535)*(k**2))
c=4.297983
ffc=a1+a2+a3+a4+a5+c
end function ffc    
!-------------------------------------------------------------------
real (r8b) function ffo(k1)
real(r8b),intent(in)::k1
real(r8b)::a1,a2,a3,a4,a5,c,k
k=k1/(4*pi)
a1= 2.960427*exp(-1.0*(14.182259)*(k**2))
a2= 2.508818*exp(-1.0*(5.936858)*(k**2))
a3= 0.637853*exp(-1.0*(0.112726)*(k**2))
a4= 0.722838*exp(-1.0*(34.958481)*(k**2))
a5= 1.142756*exp(-1.0*(0.390240)*(k**2))
c=0.027014
ffo=a1+a2+a3+a4+a5+c
end function ffo      
!-------------------------------------------------------------------
!-------------------------------------------------------------------
real (r8b) function ffh(k1)
real(r8b),intent(in)::k1
real(r8b)::a1,a2,a3,a4,a5,c,k
k=k1/(4*pi)
a1=0.489918*exp(-1.0*(20.6593)*(k**2))
a2=0.262003*exp(-1.0*(7.74039)*(k**2))
a3=0.196767*exp(-1.0*(49.5519)*(k**2))
a4=0.049879*exp(-1.0*(2.20159)*(k**2))
c=0.001305
a5=0


ffh=a1+a2+a3+a4+a5+c

end function ffh    
!-------------------------------------------------------------------

end