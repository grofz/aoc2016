module day1618_mod
    use parse_mod, only : string_t, read_strings
    implicit none

    character(len=1), parameter :: CH_SAFE='.', CH_TRAP='^'
    type row_t
        character(len=1), allocatable :: r(:)
    contains
        procedure :: print => row_print
        procedure :: nsafe => row_nsafe
    end type
    interface row_t
        module procedure row_new, row_next
    end interface

contains
    subroutine day1618(file)
        character(len=*), intent(in) :: file

        type(string_t), allocatable :: lines(:)
        type(row_t) :: row

        integer :: i, ans1, ans2
        integer, parameter :: NROW1=40, NROW2=400000

        lines = read_strings(file)
        if (size(lines)/=1) error stop 'day18 - only one line expected'
        row = row_t(lines(1)%str)
        ans1 = row%nsafe()

        do i=2, NROW1
            row = row_t(row)
            ans1 = ans1 + row%nsafe()
        end do

        ans2 = ans1
        do i=NROW1+1, NROW2
            row = row_t(row)
            ans2 = ans2 + row%nsafe()
        end do

        print '("Answer 18/1 ",i0,l2)', ans1, ans1==1939
        print '("Answer 18/2 ",i0,l2)', ans2, ans2==19999535
    end subroutine


    type(row_t) function row_new(str) result(new)
        character(len=*), intent(in) :: str

        integer :: n, i

        ! Allocate with borders (assumed safe)
        n = len_trim(str)
        allocate(new%r(0:n+1))
        new%r(0) = CH_SAFE
        do i=1,n
            new%r(i) = str(i:i)
        end do
        new%r(n+1) = CH_SAFE
    end function


    type(row_t) function row_next(origin) result(next)
        class(row_t), intent(in) :: origin

        integer :: n, i
        logical :: l_istrap, r_istrap, c_istrap

        n = size(origin%r)-2
        allocate(next%r, source=origin%r)

        do i=1,n
            l_istrap = origin%r(i-1)==CH_TRAP
            r_istrap = origin%r(i+1)==CH_TRAP
            c_istrap = origin%r(i)==CH_TRAP
            next%r(i)=CH_TRAP
            if (l_istrap .and. c_istrap .and. .not. r_istrap) then
            else if (r_istrap .and. c_istrap .and. .not. l_istrap) then
            else if (l_istrap .and. .not. (c_istrap .or. r_istrap)) then
            else if (r_istrap .and. .not. (c_istrap .or. l_istrap)) then
            else
                next%r(i)=CH_SAFE
            end if
        end do
    end function


    subroutine row_print(this)
        class(row_t), intent(in) :: this
        integer :: i
        if (allocated(this%r)) then
            print '(*(a))', (this%r(i), i=1,size(this%r)-2)
        else
            print '(a)', 'EMPTY ROW'
        end if
    end subroutine row_print


    integer function row_nsafe(this) result(nsafe)
        class(row_t), intent(in) :: this
        integer :: j

        nsafe = 0
        do j=1,size(this%r)-2
            if (this%r(j)==CH_SAFE) nsafe = nsafe+1
        end do
    end function

end module day1618_mod