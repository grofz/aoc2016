module day1620_mod
    use iso_fortran_env, only : I8 => int64
    use parse_mod, only : string_t, read_strings, split
    private
    public day1620

    type interval_t
        integer(I8) :: a, b
    end type 
    interface interval_t
        module procedure interval_new
    end interface

contains

    subroutine day1620(file)
        character(len=*), intent(in) :: file

        type(string_t), allocatable :: lines(:)
        type(interval_t), allocatable :: ints(:)
        integer :: i, nints
        integer(I8) :: ans1, ip, ans2
        integer(I8), parameter :: MINIP=0_I8, MAXIP=4294967295_I8
        lines = read_strings(file)
        nints = 0
        do i=1,size(lines)
            call add_interval(ints, nints, interval_t(lines(i)%str))
        end do
        call interval_sort(ints, nints)

        ! this is sorted list of blacklisted ip's
        print '("Black listed addresses: ")'
        do i=1,nints
          print '(i0,"-",i0)', ints(i)%a,ints(i)%b
        end do
        print '("(",i0," lines)")', nints

        ! Answer 1, find first number that is not inside any interval
        ans1 = MINIP
        do i=1,nints
            if (ints(i)%a<= ans1) then
                ans1 = ints(i)%b+1
            else
                exit
            end if
        end do
        print '("Answer 20/1 ",i0,l2)', ans1, ans1==17348574

        ! Answer 2, count number of valid ip adresses
        ans2 = 0
        ip = MINIP
        i = 1
        do
            if (ip > MAXIP) exit
            if (i<=nints) then
                if (ints(i)%a==ip) then
                    ! ip is blacklisted, try just behind the interval
                    ip = ints(i)%b+1
                    i = i + 1
                else
                    ! ip is available, try next ip
                    ans2 = ans2 + 1
                    ip = ip + 1
                end if
            else
                ! no more blacklists, just count what remains
                ans2 = ans2 + 1
                ip = ip + 1
            end if
        end do
        print '("Answer 20/2 ",i0,l2)', ans2, ans2==104

    end subroutine day1620


    type(interval_t) function interval_new(str) result(new)
        character(len=*), intent(in) :: str

        type(string_t), allocatable :: s(:)

        call split(str,'-',s)
        if (size(s)/=2) error stop 'interval_new - invalid format'
        read(s(1)%str,*) new%a
        read(s(2)%str,*) new%b
    end function interval_new


    subroutine add_interval(arr, n, new_int)
        type(interval_t), allocatable, intent(inout) :: arr(:)
        integer, intent(inout) :: n
        type(interval_t), intent(in) :: new_int

        type(interval_t), allocatable :: wrk(:)
        type(interval_t) :: wrk_int, union_int
        integer :: i, mode
        logical :: ignore_new

        ! make sure there is space in the array of intervals
        if (.not. allocated(arr)) then
            allocate(arr(100))
            n = 0
        end if
        if (size(arr) == n) then
            ! array is full
            allocate(wrk(2*n))
            wrk(1:n) = arr
            call move_alloc(wrk, arr)
        end if

        i = 1
        wrk_int = new_int
        ignore_new = .false.
        do
            if (i > n) exit
            union_int = interval_union(arr(i), wrk_int, mode)
            select case(mode)
            case(1)
            ! new interval is fully inside already present interval
            ! ... ignore new interval, exit
                ignore_new = .true.
                exit

            case(2)
            ! new interval does not interact with present interval
            ! ... continue
                i = i + 1

            case(3)
            ! new interval fully or partialy overlaps with already present interval
            ! ... make union of these intervals
            ! ... remove old interval
            ! ... continue with the intersection as the new interval
                wrk_int = union_int
                arr(i:n-1) = arr(i+1:n)  
                n = n - 1
                i = i ! do not update index
            end select
        end do

        ! add the working interval (if not discarded)
        if (.not. ignore_new) then
            n = n + 1
            arr(n) = wrk_int
        end if
    end subroutine add_interval


    function interval_union(ione, itwo, mode) result(iunion)
        type(interval_t) :: iunion
        type(interval_t), intent(in) :: ione, itwo
        integer, intent(out) :: mode

        ! if "itwo" fully inside "ione"
        ! ... iunion is null interval (mode=1)
        if (ione%a <= itwo%a .and. ione%b >= itwo%b) then
            mode = 1
            iunion%a = 0
            iunion%b = 0

        ! if "itwo" and "ione" do not interact
        ! ... iunion is copy of "itwo" (mode=2)
        else if (ione%b < itwo%a .or. itwo%b < ione%a) then
            mode = 2
            iunion = itwo

        ! if "itwo" partially or fully overlaps
        ! ... iunion is a union of both intervals (mode=3)
        else if (ione%b >= itwo%a .or. itwo%b >= ione%a) then
            mode = 3
            iunion%a = min(ione%a, itwo%a)
            iunion%b = max(ione%b, itwo%b)
        else 
            error stop 'interval_union - unexpected branch'
        end if
    end function interval_union


    subroutine interval_sort(arr, n)
        type(interval_t), intent(inout) :: arr(:)
        integer, intent(in) :: n

        integer :: i, mode
        logical :: was_swap
        type(interval_t) :: tmp

        do
            was_swap = .false.
            do i=1,n-1
                if (arr(i)%a > arr(i+1)%a) then
                    tmp = arr(i)
                    arr(i) = arr(i+1)
                    arr(i+1) = tmp
                    was_swap = .true.
                end if

                ! check intervals do not interact
                tmp = interval_union(arr(i),arr(i+1),mode)
                if (mode /= 2) error stop 'interval_sort - something wrong with intervals'
            end do
            if (.not. was_swap) exit
        end do
    end subroutine

end module day1620_mod