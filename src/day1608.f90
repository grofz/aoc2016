module day1608_mod
    use parse_mod, only : string_t, read_strings
    implicit none
    private
    public day1608

    integer, parameter :: XSIZE=50, YSIZE=6
    character(len=1), parameter :: CH_ON='#', CH_OFF='.'

    type :: lscreen_t
        character(len=1) :: ds(0:XSIZE-1, 0:YSIZE-1) = CH_OFF
    contains
        procedure :: print => lscreen_print
        procedure :: rect => lscreen_rect
        procedure :: rotate_row => lscreen_rotrow
        procedure :: rotate_column => lscreen_rotcol
        procedure :: command => lscreen_command
    end type

contains

    subroutine day1608(file)
        character(len=*), intent(in) :: file

        type(lscreen_t) :: screen
        type(string_t), allocatable :: lines(:)
        integer :: i, ans1

        lines = read_strings(file)
        do i=1, size(lines)
            call screen%command(lines(i)%str)
        end do
        ans1 = count(screen%ds==CH_ON)
        print '("Answer 8/1 ",i0,l2)', ans1, ans1==115
        print '("Answer 8/2")'
        call screen%print()
    end subroutine day1608



    subroutine lscreen_print(this)
        class(lscreen_t), intent(in) :: this

        integer :: i, j
        do j=lbound(this%ds,2), ubound(this%ds,2)
            write(*,'(*(a,1x))') (this%ds(i,j), i=lbound(this%ds,1),ubound(this%ds,1))
        end do
        print *
    end subroutine



    subroutine lscreen_rect(this, nx, ny)
        class(lscreen_t), intent(inout) :: this
        integer, intent(in) :: nx, ny
        this%ds(0:nx-1, 0:ny-1) = CH_ON
    end subroutine



    subroutine lscreen_rotrow(this, nr, m)
        class(lscreen_t), intent(inout) :: this
        integer, intent(in) :: nr, m
        this%ds(:,nr) = rotate_vector(this%ds(:,nr), m)
    end subroutine



    subroutine lscreen_rotcol(this, nc, m)
        class(lscreen_t), intent(inout) :: this
        integer, intent(in) :: nc, m
        this%ds(nc,:) = rotate_vector(this%ds(nc,:), m)
    end subroutine



    function rotate_vector(arrin, m) result(arrout)
        character(len=1), intent(in) :: arrin(:)
        integer, intent(in) :: m
        character(len=1) :: arrout(size(arrin,1))

        integer :: n, m0
        n = size(arrin,1)
        if (m >= 0) then
            m0 = mod(m, n)
        else ! make positive rotation from a negative instruction
            m0 = mod(-m, n)
            m0 = n - m0
        end if
        arrout(m0+1:n) = arrin(1:n-m0)
        arrout(1:m0) = arrin(n-m0+1:n)
    end function



    subroutine lscreen_command(this, str)
        class(lscreen_t), intent(inout) :: this
        character(len=*), intent(in) :: str

        integer :: ispace, ix, i, ib, m, n

        ispace = scan(str,' ')
        if (ispace==0) error stop 'lscreen_command - invalid format'

        if (str(:ispace-1)=='rect') then
            ix = scan(str(ispace+1:),'x')
            if (ix==0) error stop 'lscreen_command - rect format wrong'
            read(str(ispace+1:ispace-1+ix), *) m
            read(str(ispace+ix+1:), *) n
            call this%rect(m,n)

        else if (str(:ispace-1)== 'rotate') then
            i = scan(str(ispace+1:),'=')
            ib = scan(str(ispace+1:),'b')
            if (i==0 .or. ib==0) error stop 'lscreen command - rotate format wrong'
            if (str(ispace+1:ispace+i)=='row y=') then
                read(str(ispace+i+1:ispace+ib-1), *) m
                read(str(ispace+ib+3:), *) n
                call this%rotate_row(m, n)
            else if (str(ispace+1:ispace+i)=='column x=') then
                read(str(ispace+i+1:ispace+ib-1), *) m
                read(str(ispace+ib+3:), *) n
                call this%rotate_column(m, n)
            else
                error stop 'lscreen_command - uknown command 2'
            end if
        else
            error stop 'lscreen_command - uknown command'
        end if
    end subroutine

end module day1608_mod