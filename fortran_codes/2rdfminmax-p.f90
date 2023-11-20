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

dat1="max_Hp-ow.txt"
open(58,file=dat1,action="write")
dat="min_Hp-ow.txt"
open(59,file=dat,action="write")

file1="ow-Hp.txt"
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

    if ( i==2) roh =0
    if ( i==3) roh =0.00225378903837731
    if ( i==4) roh =0.00443233316033139
    if ( i==5) roh =0.00562315123605229
    if ( i==6) roh =0.00641697900163644
    if ( i==7) roh =0.00703240829887471
    if ( i==8) roh =0.00749060470194252
    if ( i==9) roh =0.007844743201336
    if ( i==10) roh =0.00811604158289062
    if ( i==11) roh =0.00830395217874377
    if ( i==12) roh =0.00850646546333726
    if ( i==13) roh =0
    if ( i==14) roh =0.00225166821151715
    if ( i==15) roh =0.00440622063551164
    if ( i==16) roh =0.00557455387207652
    if ( i==17) roh =0.00634924509224531
    if ( i==18) roh =0.0069737463242506
    if ( i==19) roh =0.00745265977992864
    if ( i==20) roh =0.00782210305495766
    if ( i==21) roh =0.00798046119682155
    if ( i==22) roh =0.00820219373254882
    if ( i==23) roh =0.00829115607677195
    if ( i==24) roh =0
    if ( i==25) roh =0.00221382510460468
    if ( i==26) roh =0.00437508601815811
    if ( i==27) roh =0.00546079629803474
    if ( i==28) roh =0.00624644812772986
    if ( i==29) roh =0.00683558347457013
    if ( i==30) roh =0.00729201553493906
    if ( i==31) roh =0.00764017932368358
    if ( i==32) roh =0.00785900896036108
    if ( i==33) roh =0.00802923987269646
    if ( i==34) roh =0.00827154951378248
    if ( i==35) roh =0
    if ( i==36) roh =0.00220385175786764
    if ( i==37) roh =0.00433739620430268
    if ( i==38) roh =0.00545757862710697
    if ( i==39) roh =0.00617376726053911
    if ( i==40) roh =0.00683977100626967
    if ( i==41) roh =0.007200477859248
    if ( i==42) roh =0.00755230500976883
    if ( i==43) roh =0.00776847110669627
    if ( i==44) roh =0.00805587408988169
    if ( i==45) roh =0.00822608004358977
    if ( i==46) roh =0
    if ( i==47) roh =0.00221413476269698
    if ( i==48) roh =0.00427683067290264
    if ( i==49) roh =0.00540976684473978
    if ( i==50) roh =0.00616262709044042
    if ( i==51) roh =0.0066272561773488
    if ( i==52) roh =0.00716634236365477
    if ( i==53) roh =0.00749265708199408
    if ( i==54) roh =0.00771180198946737
    if ( i==55) roh =0.00787060128862794
    if ( i==56) roh =0.00801660276044085
    if ( i==57) roh =0
    if ( i==58) roh =0.00219287959348541
    if ( i==59) roh =0.00424088981443344
    if ( i==60) roh =0.00536091633665481
    if ( i==61) roh =0.00606681850933023
    if ( i==62) roh =0.00660604704390614
    if ( i==63) roh =0.00701078874172705
    if ( i==64) roh =0.00736934007455733
    if ( i==65) roh =0.00764522823966849
    if ( i==66) roh =0.00779959275522023
    if ( i==67) roh =0.00796110501731659
    if ( i==68) roh =0
    if ( i==69) roh =0.0021832655538378
    if ( i==70) roh =0.00421250102061227
    if ( i==71) roh =0.00530364884496506
    if ( i==72) roh =0.00602883927895026
    if ( i==73) roh =0.00655674481475569
    if ( i==74) roh =0.00699671766536791
    if ( i==75) roh =0.00735449397398251
    if ( i==76) roh =0.00757052642267949
    if ( i==77) roh =0.0077279102309042
    if ( i==78) roh =0.00793048165648536
    
    


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

    
 