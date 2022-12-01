!
! TODO
! Part 1 ok
! Part 2 needs faster algorithm, it is too slow at the moment
!
module day1619_mod
    implicit none

    type elefant_t
        integer :: a, b, d
    contains
        procedure :: n => elefant_number
        procedure :: oneround => elefant_oneround
    end type
    interface elefant_t
        module procedure elefant_new
    end interface

contains

    subroutine day1619(nelf)
        integer, intent(in) :: nelf
        type(elefant_t) :: circle
        integer :: ans1, ans2

        circle = elefant_t(nelf)
        do
            print *, circle%a, circle%b, circle%d, circle%n()
            if (circle%n()==1) exit
            call circle%oneround()
        end do
        ans1 = circle%a
        print '("Answer 19/1 ",i0,l2)', ans1, ans1==1841611 

        ! Part Two (O(N^2) not very efficient)
        !ans2 = part2_bruteforce(nelf)
        ans2 = part2_analytical(nelf)
        print *, 'Ans 2',ans2, ans2==1423634
    end subroutine


    function part2_analytical(nelf) result(ans)
        integer, intent(in) :: nelf
        integer :: ans

        integer ::  pow

        pow = 1
        do
            if (pow*3 > nelf) exit
            pow = pow*3
        end do

        ans = nelf-pow
        if (ans >= pow) then
            ans =  pow + 2*(ans-pow)
        end if
        if (ans==0) ans = nelf
    end function


    function part2_bruteforce(nelf) result(ans)
        integer, intent(in) :: nelf
        integer :: ans

        integer :: i, n, ielim
        integer, allocatable :: arr(:)

        allocate(arr(nelf))
        do i=1, nelf
            arr(i) = i
        end do
        n = nelf 
        i = 1
        do
            ielim = get_index_across(i, n)
            call eliminate(arr, n, ielim)
            if (ielim > i) i = i + 1
            if (i>n) i = 1
            if (n==1) exit
            !if (mod(n,10000)==0) print *, 'remain ',n, i
        end do
        ans = arr(1)
    end function


    function get_index_across(i, n) result(iacross)
        integer :: iacross
        integer, intent(in) :: i, n

        iacross = i + n/2
        if (iacross > n) iacross = iacross - n
    end function


    subroutine eliminate(arr, n, ielim)
        integer, intent(inout) :: arr(:)
        integer, intent(inout) :: n
        integer, intent(in) :: ielim

        arr(ielim:n-1) = arr(ielim+1:n)
        n = n - 1
    end subroutine


    pure type(elefant_t) function elefant_new(nelf) result(new)
        integer, intent(in) :: nelf
        new%a = 1
        new%b = nelf
        new%d = 1
    end function

    function elefant_number(this) result(n)
        integer :: n
        class(elefant_t), intent(in) :: this
        n = (this%b-this%a)/this%d + 1
    end function

    subroutine elefant_oneround(this)
        class(elefant_t), intent(inout) :: this
        
        if (mod(this%n(), 2)==0) then
            ! even number of elves
            ! last elf eliminated, first survives
            this%b = this%b - this%d
            this%d = this%d * 2
        else
            ! odd number of elves
            ! first elf eliminated, last survives
            this%d = this%d * 2
            this%a = this%a + this%d
        end if
    end subroutine
end module day1619_mod