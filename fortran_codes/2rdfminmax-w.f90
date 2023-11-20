 implicit none
        integer,parameter         :: r8b= kind(1.D200)
        integer,parameter         :: i4b= kind(2147483647)
    character(70)::dat,dat1,file1
    integer(i4b) ::i,n,j,cc,n1,n2
    real(r8b)::sum,roh,roh1,aw,ll,z1,z2
    real(r8b),parameter::PI = 3.141592653589793238462643d0 
    real(r8b), allocatable :: x(:,:)

  
cc=77

n=1000
z1=0.0
z2=0.0
allocate(x(n,78))

120 format (2(F6.3,x))
121 format (2(F6.3,x))

dat1="max_Hw-op.txt"
open(58,file=dat1,action="write")
dat="min_Hw-op.txt"
open(59,file=dat,action="write")

file1="op-Hw.txt"
    open(15, file=file1, status='old')
  
    do i=1,n
      read (15,*) x(i,:)
    enddo
   close (15)

do i=1,n
    x(i,1)=10.0*x(i,1)
enddo


write (58,*) "max-pos max-value"
write (59,*) "max-pos cord#"

do i=2,78
    print*,i

    if ( i==2) roh =0.0337485165752692
    if ( i==3) roh =0.025918573941339
    if ( i==4) roh =0.0177293326413256
    if ( i==5) roh =0.0131206862174553
    if ( i==6) roh =0.00962546850245467
    if ( i==7) roh =0.00703240829887471
    if ( i==8) roh =0.00499373646796168
    if ( i==9) roh =0.00320419088505273
    if ( i==10) roh =0.00202901039572266
    if ( i==11) roh =0.00102633116815934
    if ( i==12) roh =0
    if ( i==13) roh =0.0335845708602419
    if ( i==14) roh =0.0258941844324472
    if ( i==15) roh =0.0176248825420466
    if ( i==16) roh =0.0130072923681785
    if ( i==17) roh =0.00952386763836796
    if ( i==18) roh =0.0069737463242506
    if ( i==19) roh =0.00496843985328576
    if ( i==20) roh =0.00319494350132073
    if ( i==21) roh =0.00199511529920539
    if ( i==22) roh =0.00101375428155098
    if ( i==23) roh =0
    if ( i==24) roh =0.0338351710877676
    if ( i==25) roh =0.0254589887029538
    if ( i==26) roh =0.0175003440726324
    if ( i==27) roh =0.0127418580287477
    if ( i==28) roh =0.00936967219159478
    if ( i==29) roh =0.00683558347457013
    if ( i==30) roh =0.00486134368995937
    if ( i==31) roh =0.00312063662516653
    if ( i==32) roh =0.00196475224009027
    if ( i==33) roh =0.000992377961793945
    if ( i==34) roh =0
    if ( i==35) roh =0.0333181283012931
    if ( i==36) roh =0.0253442952154778
    if ( i==37) roh =0.0173495848172107
    if ( i==38) roh =0.0127343501299163
    if ( i==39) roh =0.00926065089080866
    if ( i==40) roh =0.00683977100626967
    if ( i==41) roh =0.004800318572832
    if ( i==42) roh =0.00308474429976473
    if ( i==43) roh =0.00194211777667407
    if ( i==44) roh =0.000995669831333692
    if ( i==45) roh =0
    if ( i==46) roh =0.0330311138276769
    if ( i==47) roh =0.0254625497710153
    if ( i==48) roh =0.0171073226916105
    if ( i==49) roh =0.0126227893043928
    if ( i==50) roh =0.00924394063566064
    if ( i==51) roh =0.0066272561773488
    if ( i==52) roh =0.00477756157576985
    if ( i==53) roh =0.00306038106165955
    if ( i==54) roh =0.00192795049736684
    if ( i==55) roh =0.000972770945785476
    if ( i==56) roh =0
    if ( i==57) roh =0.0332225951452396
    if ( i==58) roh =0.0252181153250823
    if ( i==59) roh =0.0169635592577338
    if ( i==60) roh =0.0125088047855279
    if ( i==61) roh =0.00910022776399534
    if ( i==62) roh =0.00660604704390614
    if ( i==63) roh =0.00467385916115137
    if ( i==64) roh =0.00301001214312905
    if ( i==65) roh =0.00191130705991712
    if ( i==66) roh =0.000963994610195759
    if ( i==67) roh =0
    if ( i==68) roh =0.0330002219179505
    if ( i==69) roh =0.0251075538691347
    if ( i==70) roh =0.0168500040824491
    if ( i==71) roh =0.0123751806382518
    if ( i==72) roh =0.00904325891842539
    if ( i==73) roh =0.00655674481475569
    if ( i==74) roh =0.00466447844357861
    if ( i==75) roh =0.00300394824289426
    if ( i==76) roh =0.00189263160566987
    if ( i==77) roh =0.000955134972358946
    if ( i==78) roh =0
    


ll=0.0
do j=1,n
 ll=ll+x(j,i)
enddo

 if (ll<0.1) then
    write (58,120) z1,z2
    write (59,121) z1,z2

 else
do j=1,n

if(x(j,i).ne.0) then    
if(x(j,i)>=x(j-1,i)) then
if(x(j,i)>=x(j-2,i)) then
if(x(j,i)>x(j-3,i)) then
if(x(j,i)>=x(j+1,i)) then
if(x(j,i)>=x(j+2,i)) then
if(x(j,i)>x(j+3,i)) then
write (58,120) x(j,1),x(j,i)
EXIT
endif
endif
endif
endif
endif
endif
endif
enddo

        do j=1,n
        if(x(j,i).ne.0) then
        if(x(j,i)<=x(j-2,i)) then
        if(x(j,i)<x(j-3,i)) then
        if(x(j,i)<x(j-4,i)) then
        if(x(j,i)<x(j-5,i)) then    
        if(x(j,i)<=x(j+1,i)) then
        if(x(j,i)<=x(j+2,i)) then
            call intl(i,j,x,sum,roh)
            write (59,121) x(j,1),sum
        EXIT
        endif
        endif
        endif
        endif
        endif
        endif
        endif    
        enddo
    endif       
enddo

contains
!-----------------------------------------------------------------------    
subroutine intl(n1,n,h,sum,roh)   
 real(r8b),intent(out)::sum
 integer(i4b),intent(in)::n,n1
 real(r8b),intent(in)::roh
 integer(i4b) :: i
 real(r8b), allocatable,intent(in) :: h(:,:)
 real(r8b), allocatable :: h1(:)
 
allocate(h1(n))
do i=1,n
h1(i)=4*pi*roh*h(i,n1)*h(i,1)*h(i,1)
enddo

    sum=0.0
    do i=1,n-1
        sum=sum+0.5*0.01*(h1((i+1))+h1(i))              
    end do
end subroutine
!-------------------------------------------------------------------    
end

    
 