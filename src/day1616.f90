module day1616_mod
    implicit none
    private
    public day1616

    integer, parameter :: LK = 1

    type dragon_t
        logical(LK), allocatable :: bits(:)
    contains
        procedure :: to_string => dragon_to_string
        procedure :: chsum => dragon_chsum
        procedure :: trim => dragon_trim
        procedure :: double => dragon_double
        procedure :: grow => dragon_grow
    end type
    interface dragon_t
        module procedure dragon_from_string, dragon_from_logical
    end interface
contains

    subroutine day1616(istate, nlen1, nlen2)
        character(len=*), intent(in) :: istate
        integer, intent(in) :: nlen1, nlen2

        type(dragon_t) :: obj
        character(len=:), allocatable :: ans1, ans2

        ! Part 1
        obj = dragon_t(istate)
        print '(a,a)', 'Intitial input: ', obj%to_string()
        call obj%grow(nlen1)
        print '(a,a)', 'Grown: ', obj%to_string()
        ans1 = obj%chsum()
        print '("Answer 16/1 ",a,l2)', &
        &    ans1, ans1=='11101010111100010'
        print *

        ! Part 2
        obj = dragon_t(istate)
        call obj%grow(nlen2)
        ans2 = obj%chsum()
        print '("Answer 16/2 ",a,l2)', &
        &    ans2, ans2=='01001101001000101' 
        print *
    end subroutine


    pure type(dragon_t) function dragon_from_string(str) result(new)
        character(len=*), intent(in) :: str

        integer :: n, i
        n = len_trim(str)
        allocate(new%bits(n))
        do i=1, n
            select case(str(i:i))
            case('1')
                new%bits(i) = .true.
            case('0')
                new%bits(i) = .false.
            case default
                error stop 'dragon_from_string - unexpected character in string'
            end select
        end do
    end function dragon_from_string


    pure type(dragon_t) function dragon_from_logical(larr) result(new)
        logical(LK), intent(in) :: larr(:)

        integer :: n
        n = size(larr)
        allocate(new%bits(n))
        new%bits = larr
    end function dragon_from_logical


    pure function dragon_to_string(this) result(str)
        class(dragon_t), intent(in) :: this
        character(len=size(this%bits)) :: str

        integer :: n, i
        n = size(this%bits)
        do i=1, n
            str(i:i) = '0'
            if (this%bits(i)) str(i:i) = '1'
        end do
    end function


    pure function dragon_chsum(this) result(str)
        class(dragon_t), intent(in) :: this
        character(len=:), allocatable :: str

        integer :: n, i
        logical(LK), allocatable :: larr(:)
        type(dragon_t) :: this0

        this0 = this
        do
            n = size(this0%bits)
            if (mod(n,2)/=0) then
                allocate(character(len=n)::str)
                str = this0%to_string()
                exit
            end if
            allocate(larr(n/2))
            do i = 1, n/2
                if (this0%bits(2*i-1) .eqv. this0%bits(2*i)) then
                    larr(i) = .true.
                else
                    larr(i) = .false.
                end if
            end do
            call move_alloc(larr,this0%bits)
        end do
    end function


    pure subroutine dragon_trim(this, ntrim)
        class(dragon_t), intent(inout) :: this
        integer, intent(in) :: ntrim

        if (size(this%bits) < ntrim) error stop 'dragon_trim - not grown enough'
        select type(this)
        type is(dragon_t)
            this = dragon_t(this%bits(1:ntrim))
        class default
            error stop 'dragon_trim - polymorf not supported'
        end select
    end subroutine


    pure subroutine dragon_double(this)
        class(dragon_t), intent(inout) :: this

        logical(LK), allocatable :: b(:), double(:)
        integer :: n, ierr

        n = size(this%bits)
        allocate(b(n), double(2*n+1), stat=ierr)
        if (ierr/=0) error stop 'dragon_double - allocation error'
        b = this%bits(n:1:-1) ! copy of "bits" and reversed
        b = .not. b
        double(1:n) = this%bits
        double(n+1) = .false.
        double(n+2:) = b
        call move_alloc(double, this%bits)
    end subroutine


    subroutine dragon_grow(this, nlen)
        class(dragon_t), intent(inout) :: this
        integer, intent(in) :: nlen

        do 
            if (size(this%bits)>=nlen) exit
            call this%double()
            print *, 'size increased to ',size(this%bits)
        end do
        call this%trim(nlen)
        print *, 'final size ',size(this%bits)
    end subroutine

end module day1616_mod