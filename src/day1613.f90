module day1613_mod
    implicit none

    integer, parameter :: MAXBITS = 31
    integer, parameter :: DIRS(2,4) = reshape([-1, 0, 0, 1, 1, 0, 0, -1],[2,4])

    type maze_t
        integer, allocatable :: mh(:,:)
        integer :: number
        integer :: target(2)
        integer :: start(2) = [1,1]
    contains
        procedure :: isopen => maze_isopen
        procedure :: print => maze_print, printmh => maze_printmh
        procedure :: route => maze_route
    end type
    interface maze_t
        module procedure maze_new
    end interface maze_t
contains

    subroutine day1613(numbers)
        integer, intent(in) :: numbers(3)

        type(maze_t) :: maze
        integer :: ans1, ans2

        maze = maze_t(numbers)
        call maze%print()
        call maze%route()
        call maze%printmh()
        ans1 = maze%mh(maze%target(1), maze%target(2))
        ans2 = count(maze%mh>=0 .and. maze%mh<=50)
        print '("Answer 13/1 ",i0,l2)', ans1, ans1==86
        print '("Answer 13/2 ",i0,l2)', ans2, ans2==1
    end subroutine day1613


    type(maze_t) function maze_new(numbers) result(new)
        integer, intent(in) :: numbers(3)

        integer :: mxlim(2)

        mxlim = 2*numbers(2:3)
        allocate(new%mh(0:mxlim(1),0:mxlim(2)))
        new%mh = -1
        new%number = numbers(1)
        new%target = numbers(2:3)
    end function maze_new


    logical function maze_isopen(this, pos) result(isopen)
        class(maze_t), intent(in) :: this
        integer, intent(in) :: pos(2)

        integer :: val, nones

        if (any(pos<0)) then
            isopen = .false.
            return
        else if (pos(1)>size(this%mh,1) .or. pos(2)>size(this%mh,2)) then
            error stop 'maze_isopen - raaching limits of allocation'
        end if

        associate(x=>pos(1), y=>pos(2))
            val = x*x + 3*x + 2*x*y + y + y*y
            val = val + this%number
            nones = count(int2bin(val))
        end associate
        select case(mod(nones,2))
        case(0)
            isopen = .true.
        case(1)
            isopen = .false.
        end select
    end function maze_isopen


    function int2bin(int) result(bin)
        integer, intent(in) :: int
        logical :: bin(MAXBITS)

        integer :: n, i, j, p

        bin = .false.
        n = int
        do i=MAXBITS-1,0,-1
            j = MAXBITS-i
            p = n/(2**i)
            n = n - p*(2**i)
            if (p==1) then
                bin(j) = .true.
            else if (p /= 0) then
                error stop 'int2bin - number is too large for MAXBITS'
            end if
        end do
    end function int2bin


    subroutine maze_print(this)
        class(maze_t), intent(in) :: this

        integer :: i, j
        do j=0, size(this%mh,2)-1
            do i=0, size(this%mh,1)-1
                if (this%isopen([i,j])) then
                    write(*,'(a1)',advance='no') '.'
                else
                    write(*,'(a1)',advance='no') '#'
                end if
            end do
            write(*,*)
        end do
    end subroutine maze_print


    subroutine maze_printmh(this)
        class(maze_t), intent(in) :: this
!
! Print distance from the start.
! "-1" - unknown value, "-2" - not accessible
!
        integer :: i, j
        do j=0, size(this%mh,2)-1
            print '(*(i3))', (val([i,j]),i=0,size(this%mh,1)-1)
            if (mod(j,10)==0) print *
        end do
    contains
        integer function val(pos)
            integer, intent(in) :: pos(2)
            if (this%isopen(pos)) then
                val = this%mh(pos(1),pos(2))
            else
                val = -2
            end if
        end function
    end subroutine maze_printmh


    subroutine maze_route(this)
        class(maze_t), intent(inout) :: this
    !
    ! Simplified search without a priority queue
    ! - add unexplored positions at the end of queue
    ! - explore position in the same order they were added
    !
        integer, allocatable :: queue(:,:)
        integer :: curpos(2), i, n, k, n0

        n = 0
        curpos = this%start
        call addpos(queue,n,curpos)
        this%mh(curpos(1),curpos(2)) = 0
        i = n
        MAINLOOP: do
            if (i > n) then
                ! end of queue, but we are still in the loop...
                call this%printmh()
                error stop 'maze_route - could not find route to target'
            end if
            curpos = queue(:,i)

            ! target has been reached
            if (all(curpos==this%target)) exit MAINLOOP

            ! add all open neighbors to the queue if not already there
            do k=1,4
                associate(newpos=>curpos+DIRS(:,k))
                if (this%isopen(newpos)) then
                    n0 = n
                    call addpos(queue,n,newpos)
                    if (n/=n0) this%mh(newpos(1),newpos(2)) = this%mh(curpos(1),curpos(2)) + 1
                end if
                end associate
            end do
            i = i + 1
        end do MAINLOOP
!       print *, 'maze route ',this%mh(curpos(1),curpos(2))
    end subroutine maze_route


    subroutine addpos(arr, n, new)
        integer, parameter :: NCOLS=2
        integer, intent(inout), allocatable :: arr(:,:)
        integer, intent(inout) :: n
        integer, intent(in) :: new(NCOLS)
    !
    ! Add vector to the array, unless the same vector is
    ! already there. Expand array if necessary.
    !
        integer, allocatable :: tmp(:,:)
        integer :: j

        if (.not. allocated(arr)) allocate(arr(NCOLS,10))
        if (size(arr,2)==n) then
            ! expand array
            allocate(tmp(NCOLS,2*size(arr,2)))
            tmp(:,1:size(arr,2)) = arr
            call move_alloc(tmp, arr)
        end if
        ! search for existing item in array
        do j=n,1,-1
            if (all(new==arr(:,j))) exit
        end do
        if (j==0) then
            ! item not in array: add it at the end
            n = n+1
            arr(:,n) = new
        end if
    end subroutine addpos

end module day1613_mod