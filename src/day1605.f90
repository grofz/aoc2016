module day1605_mod
    implicit none
    private
    public day1605, md5, int2str

    interface 
        character*32 function md5(string)
            implicit none
            character*(*) string
        end function
    end interface

    integer, parameter :: PASSSIZE=8

contains
    subroutine day1605(str)
        character(len=*), intent(in) :: str
        integer :: i
        character(len=128) :: hash
        character(len=PASSSIZE) :: pass1, pass2

        print *, '*'//md5('abc3231929')//'*'
        print *, '*'//md5('abc5017308')//'*'
        print *, '*'//md5('abc5278568')//'*'
        print *, 'Verify that first letters are: 1, 8, F'

        call crack(str, pass1, 1)
        call validate_pass(pass1, 1)
        call crack(str, pass2, 2)
        call validate_pass(pass2, 2)

    contains
        subroutine validate_pass(pass, method)
            integer, intent(in) :: method
            character(len=*), intent(in) :: pass
            logical :: isvalid
            isvalid = .false.
            select case(method)
            case(1)
                if (pass=='18F47A30' .or. pass=='F97C354D') isvalid = .true.
            case(2)
                if (pass=='05ACE8E3' .or. pass=='863DDE27') isvalid = .true.
            end select
            if (isvalid) then
                print '("Password accepted...")'
            else
                print '("Unknown password...")'
            end if
            print *
        end subroutine
    end subroutine day1605


    subroutine crack(key, pass, method)
        character(len=*), intent(in) :: key
        character(len=PASSSIZE), intent(out) :: pass
        integer, intent(in) :: method

        integer, parameter :: MAXRUN = huge(MAXRUN)
        integer :: i, cfound, order
        character(len=32) :: hash
        character(len=1), parameter :: NULL_CHAR='.'
        real :: t0, t1

        pass = repeat(NULL_CHAR, PASSSIZE)
        cfound = 0
        call cpu_time(t0)

        write(*,'(a,1x,i1)') 'Cracking password for: '//key//'  version: ',method
        write(*,'(a)', advance='no') 'aaa'
        do i = 0, MAXRUN
            if (mod(i,10000)==0 .or. cfound==PASSSIZE) &
            write(*,'(a,i9,3x,a,4x,a32)',advance='no') achar(13),i/10000,pass,hash 
            if (cfound == PASSSIZE) exit
            hash = md5(key//int2str(i))
            if (hash(1:5)/='00000') cycle
            !if (hash(1:5)/='     ') cycle

            select case(method)
            case(1)
                ! characters found in order
                cfound = cfound + 1
                if (hash(6:6)==' ') hash(6:6) = '0'
                pass(cfound:cfound) = hash(6:6)
            case(2)
                ! the order is defined by 6th character
                if (hash(6:6)==' ') hash(6:6) = '0'
                if (hash(7:7)==' ') hash(7:7) = '0'
                order = iachar(hash(6:6))-iachar('0')
                if (order < 0 .or. order > PASSSIZE-1) cycle
                if (pass(order+1:order+1) /= NULL_CHAR) cycle
                cfound = cfound + 1
                pass(order+1:order+1) = hash(7:7)
            end select
        end do
        call cpu_time(t1)
        write(*,*)
        write(*,'("Time taken ",f5.0," seconds")') t1-t0
        write(*,*) 
        write(*,'(a)') pass
    end subroutine crack


    function int2str(i) result(str)
        integer, intent(in) :: i
        character(len=:), allocatable :: str

        character(len=18) :: wstr
        integer :: n
        write(wstr,'(i0)') i
        n = len_trim(wstr)
        allocate(character(len=n) :: str)
        str = trim(wstr)
    end function int2str

end module day1605_mod



! MD5 hash
! https://community.intel.com/t5/Intel-Fortran-Compiler/MD5-Hash/td-p/801183
!
! Minor changes to remove non-standard features

character*32 function md5(string)
! ---------------------------------------------------------------------*
!     Programmierer    : VEZ2/Pieper                                   *
!     Version          : 1.0                                           *
!     letzte nderung  : 07.05.2010                                    *
!     Aufgabe          : Erzeugt aus einem String einen MD5 Hashwert   *
! **********************************************************************

implicit none

character*(*) string

! https://community.intel.com/t5/Intel-Fortran-Compiler/MD5-Hash-in-Fortran-revisit/td-p/1354452
! correction
character*((int((len(string)+8)/64)+1)*64) newString
! original
!character*((int(len(string)/64)+1)*64) newString
character*8 wtmp

integer(kind=4) j,n1,n2,n3,n4,umdrehen,pos
integer(kind=4) r(64),h0,h1,h2,h3,a,b,c,d,f,g,temp,w(16),leftrotate,i,intLen
integer(kind=4), save :: k(64)
logical, save :: first_time = .true.
integer(kind=8) hoch32
real(kind=8) sinus,absolut,real8i

r = [7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22, 5, &
     9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20, 4, 11, 16, 23, &
     4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23, 6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21]

if (first_time) then
    do i=1,64
        real8i = real(int8(i), kind=8)
        sinus = dsin(real8i)
        absolut = dabs(sinus)
        hoch32 = 2.**32.
        k(i) = int8(absolut * real(hoch32, kind=8))
    end do
    first_time = .false.
end if

h0 = int(Z'67452301', kind=4)
h1 = int(Z'EFCDAB89', kind=4)
h2 = int(Z'98BADCFE', kind=4)
h3 = int(Z'10325476', kind=4)

j = len(string)+1
newString(:j) = string // char(128)
i = mod(j, 64)
do while(i /= 56)
    j = j + 1
    newString(j:j) = char(0)
    i = mod(j, 64)
end do

intLen = len(string)*8
do i = 0,3
    temp = IAND(intLen, Z'FF')
    j = j + 1
    newString(j:j) = char(temp)
    intLen = shiftr(intLen, 8)
end do

do i = 1,4
    j = j + 1
    newString(j:j) = char(0)
end do

do i = 1,int(len(newString)/64)

    do j = 1,16
        pos = (j-1)*4+(i-1)*64
        n1 = ichar(newString(4+pos:4+pos))
        n2 = ichar(newString(3+pos:3+pos))
        n3 = ichar(newString(2+pos:2+pos))
        n4 = ichar(newString(1+pos:1+pos))
        
        write(wtmp,'(4(z2.2))') n1,n2,n3,n4
        read(wtmp,'(z8)') w(j)
    end do

    a = h0
    b = h1
    c = h2
    d = h3

    do j = 1,64
        if (j >= 1 .and. j <= 16) then
            f = IOR(IAND(b, c), IAND(NOT(b), d))
            g = j
        else if (j >= 17 .and. j <= 32) then
            f = IOR(IAND(d, b), IAND(NOT(d), c))
            g = mod(5*(j-1) + 1, 16) + 1
        else if (j >= 33 .and. j <= 48) then
            f = ieor(b, ieor(c, d))
            g = mod(3*(j-1) + 5, 16) + 1
        else if (j >= 49 .and. j <= 64) then
            f = ieor(c, IOR(b, NOT(d)))
            g = mod(7*(j-1), 16) + 1
        end if
        
        temp = d
        d = c
        c = b
        b = b + leftrotate((a + f + k(j) + w(g)) , r(j))
        a = temp
    end do

    h0 = h0 + a
    h1 = h1 + b
    h2 = h2 + c
    h3 = h3 + d
end do
h0 = umdrehen(h0)
h1 = umdrehen(h1)
h2 = umdrehen(h2)
h3 = umdrehen(h3)

! to change " " to "0"
!write(md5,'(4(z8))') h0,h1,h2,h3
write(md5,'(4(z8.8))') h0,h1,h2,h3
return

end function md5
!
!
! **********************************************************************
integer(kind=4) function leftrotate (x, c)
! ---------------------------------------------------------------------*
!     Programmierer    : VEZ2/Pieper                                   *
!     Version          : 1.0                                           *
!     letzte nderung  : 07.05.2010                                    *
!     Aufgabe          : Fhrt ein Leftrotate der Bits durch           *
! **********************************************************************

implicit none

integer(kind=4) x,c,result1,result2

result1 = shiftl(x,c)
result2 = shiftr(x, (32-c))

leftrotate = IOR(result1, result2)

return
end function leftrotate
!
!
! **********************************************************************
integer(kind=4) function umdrehen(zahl)
! ---------------------------------------------------------------------*
!     Programmierer    : VEZ2/Pieper                                   *
!     Version          : 1.0                                           *
!     letzte nderung  : 07.05.2010                                    *
!     Aufgabe          : Macht aus Big Endian -> Little Endian Bits    *
! **********************************************************************

implicit none

integer(kind=4) i,tmp,zahl

umdrehen = 0
do i = 1,4
    umdrehen = shiftl(umdrehen, 8)
    tmp = IAND(zahl, Z'FF') 
    umdrehen = umdrehen + tmp;
    zahl = shiftr(zahl, 8)
end do

return
end function umdrehen