module day1607_mod
    use parse_mod, only : string_t, read_strings
    implicit none
    private
    public day1607

    type ip7_t
        type(string_t), allocatable :: chunks(:)
    contains
        procedure :: tls => supports_tls
        procedure :: ssl => supports_ssl
    end type 
    interface ip7_t
        module procedure ip7_new
    end interface
contains

    subroutine day1607(file)
        character(len=*), intent(in) :: file

        type(string_t), allocatable :: lines(:)
        type(ip7_t) :: ip7
        integer :: i, ans1, ans2
        logical :: yes

        lines = read_strings(file)
        ans1 = 0
        ans2 = 0
        do i=1,size(lines)
            ip7 = ip7_t(lines(i)%str)
            yes = ip7%tls() !support_tls(ip7)
            if (yes) ans1 = ans1 + 1

            yes = ip7%ssl()
            if (yes) ans2 = ans2 + 1
        end do
        print '("Answer 7/1 ",i0,l2)', ans1, ans1==110
        print '("Answer 7/2 ",i0,l2)', ans2, ans2==242
    end subroutine



    type(ip7_t) function ip7_new(str) result(new)
        character(len=*), intent(in) :: str

        integer :: i0, il, ir

        allocate(new%chunks(0))
        i0 = 1
        do
            il = scan(str(i0:),'[')
            ir = scan(str(i0:), ']')
            if (il /= 0) then
                new%chunks = [new%chunks, &
                &   string_t(str(i0:i0-1+il-1)), &
                &   string_t(str(i0-1+il+1:i0-1+ir-1)) ]

                i0 = i0-1+ir+1
                if (i0 >= len_trim(str)) exit
            else
                new%chunks = [new%chunks, string_t(str(i0:))]
                exit
            end if
        end do
    end function



    logical function supports_ssl(this) result(support)
        class(ip7_t), intent(in) :: this

        integer :: i, j1, j2
        character(len=2), allocatable :: super(:), hyper(:)

        allocate(super(0), hyper(0))
        do i=1, size(this%chunks), 2
            if (i==size(this%chunks)) then
                call add_element(super, aba(this%chunks(i)%str))
            else
                call add_element(super, aba(this%chunks(i)%str))
                call add_element(hyper, aba(this%chunks(i+1)%str))
            end if
        end do
        support = .false.
        MAIN: do j1 = 1, size(super)
            do j2= 1, size(hyper)
                if ( super(j1)(1:1)==hyper(j2)(2:2) .and. &
                     super(j1)(2:2)==hyper(j2)(1:1)) then
                    support = .true.
                    exit MAIN
                end if
            end do
        end do MAIN
    end function supports_ssl



    subroutine add_element(arr, items)
        character(len=2), allocatable, intent(inout) :: arr(:)
        character(len=2), intent(in) :: items(:)

        integer :: nold, nnew
        character(len=2), allocatable :: wrk(:)

        nnew = size(items)
        nold = size(arr)
        if (nnew == 0) return
        allocate(wrk(nold+nnew))
        wrk(1:nold) = arr
        wrk(nold+1:nold+nnew) = items
        call move_alloc(wrk, arr)
    end subroutine



    logical function supports_tls(this) result(support)
        class(ip7_t), intent(in) :: this

        integer :: i
        logical :: is_where_should, isnot_where_shouldnot

        is_where_should = .false.
        isnot_where_shouldnot = .true.
        do i=1,size(this%chunks),2
            if (i==size(this%chunks)) then
                if (is_abba(this%chunks(i)%str)) is_where_should = .true.
            else
                if (is_abba(this%chunks(i)%str)) is_where_should = .true.
                if (is_abba(this%chunks(i+1)%str)) isnot_where_shouldnot = .false.
            end if
        end do
        support = is_where_should .and. isnot_where_shouldnot
    end function supports_tls



    logical function is_abba(str) result(is)
        character(len=*), intent(in) :: str

        integer :: i
        logical :: first, second, different
        
        is = .false.
        do i=1, len_trim(str)-3
            first = str(i:i)==str(i+3:i+3)
            second = str(i+1:i+1)==str(i+2:i+2)
            different = str(i:i) /= str(i+1:i+1)
            if (first .and. second .and. different) then
                is = .true.
                exit
            end if
        end do
    end function



    function aba(str) result(ab)
        character(len=*), intent(in) :: str
        character(len=2), allocatable :: ab(:)

        character(len=2) :: new(1)
        integer :: i
        logical :: first, different

        allocate(ab(0))
        do i=1,len_trim(str)-2
            first = str(i:i)==str(i+2:i+2)
            different = str(i:i) /= str(i+1:i+1)
            if (first .and. different) then
                new(1) = str(i:i+1)
                call add_element(ab, new)
            end if
        end do
    end function

end module day1607_mod