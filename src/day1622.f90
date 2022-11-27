!TODO list
! - remove repeating code in {hole|data}_{firstngb|nextngb}
module day1622_mod
    use parse_mod, only : split_nonempty, read_strings, string_t
    use djikstra_mod
    implicit none

    integer, parameter :: TYPE_FREE=1, TYPE_FIXED=2
    integer, parameter :: DIRS(2,4) = reshape([-1, 0, 0, 1, 1, 0, 0, -1], [2,4])

    type node_t
        integer :: pos(2)
        integer :: size, used, avail
        integer :: type = TYPE_FIXED
    end type
    interface node_t
        module procedure node_new
    end interface

    type grid_t
        integer, allocatable :: g(:,:) ! node-type
        integer, allocatable :: disksize(:,:) 
        integer :: hole(2), data(2)
    contains
        procedure :: print => grid_print
    end type
    interface grid_t
        module procedure grid_new
    end interface

    type, extends(djikstra_node_at) :: hole_t
        integer :: hole(2)
        integer :: data(2)
        type(grid_t), pointer :: grid => null()
        integer :: flag=-1
        integer :: target(2)
    contains
        procedure :: firstngb => hole_firstngb
        procedure :: nextngb => hole_nextngb
        procedure :: isequal => hole_isequal
        procedure :: isnull => hole_isnull
        procedure :: istarget => hole_istarget
    end type

    type, extends(hole_t) :: data_t
    contains
        procedure :: firstngb => data_firstngb
        procedure :: nextngb => data_nextngb
        procedure :: isequal => data_isequal
        procedure :: istarget => data_istarget
    end type

contains

    subroutine day1622(file)
        character(len=*), intent(in) :: file

        type(node_t), allocatable :: nodes(:)
        type(grid_t), target :: grid
        type(data_t) :: data_start
        type(djikstra_node_ptr), allocatable :: dwrk(:)
        integer :: i, j, n, ans1, ans2

        nodes = read_nodes(file)
        n = size(nodes)

        ! Part 1
        ans1 = 0
        do i=1, n
            if (nodes(i)%used == 0) then
                nodes(i)%type = TYPE_FREE
                cycle
            end if
            do j=1, n
                if (i==j) cycle
                if (nodes(j)%avail < nodes(i)%used) cycle
                ans1 = ans1 + 1
                nodes(i)%type = TYPE_FREE
            end do
        end do
        print '("Answer 22/1 ",i0,l2)', ans1, ans1==941

        ! Part 2
        grid = grid_t(nodes)
        call grid%print()

        data_start%grid => grid
        data_start%hole = grid%hole
        data_start%data = grid%data
        data_start%target = [0,0]
        call djikstra_search(dwrk, data_start, ans2)
        print '("Djikstra explored number of states ",i0)',size(dwrk)
        print '("Answer 22/2 ",i0,l2)', ans2, ans2==249
    end subroutine day1622


    type(grid_t) function grid_new(nodes) result(new)
        type(node_t), intent(in) :: nodes(:)

        integer :: i, maxx, maxy, minx, miny

        maxx=0
        maxy=0
        minx=0
        miny=0
        new%hole = -1
        do i=1, size(nodes)
            if (nodes(i)%pos(1)>maxx) maxx = nodes(i)%pos(1)
            if (nodes(i)%pos(2)>maxy) maxy = nodes(i)%pos(2)
            if (nodes(i)%used==0) then
                if (new%hole(1)/=-1) error stop 'grid_new - more than one hole is not expected'
                new%hole=nodes(i)%pos
            end if
        end do
        new%data = [maxx, 0]

        allocate(new%g(minx-1:maxx+1, miny-1:maxy+1))
        allocate(new%disksize(minx-1:maxx+1, miny-1:maxy+1))
        new%g = TYPE_FIXED
        new%disksize = 0
        do i=1, size(nodes)
            new%g(nodes(i)%pos(1), nodes(i)%pos(2)) = nodes(i)%type
            new%disksize(nodes(i)%pos(1), nodes(i)%pos(2)) = nodes(i)%size
        end do
    end function grid_new


    subroutine grid_print(this)
        class(grid_t), intent(in) :: this

        integer :: i0, i1, j0, j1, i, j
        character(len=1) :: ch
        i0 = lbound(this%g, 1)
        j0 = lbound(this%g, 2)
        i1 = ubound(this%g, 1)
        j1 = ubound(this%g, 2)
        do j=j0,j1
            do i=i0,i1
                if (all(this%hole==[i,j])) then
                    ch = '_'
                else if (all(this%data==[i,j])) then
                    ch = 'D'
                else if (this%g(i,j)==TYPE_FIXED) then
                    ch = '#'
                else if (this%g(i,j)==TYPE_FREE) then
                    ch = '.'
                else
                    error stop 'grid_print - uknown branch'
                end if
                write(*,'(a1,1x)',advance='no') ch
            end do
            write(*,*)
        end do
        write(*,*)
    end subroutine grid_print


    type(node_t) function node_new(str) result(new)
        character(len=*), intent(in) :: str

        type(string_t), allocatable :: tokens(:)
        integer :: ix0, ix1, iy0

        call split_nonempty(str, ' ', tokens)
        if (size(tokens)/=5) error stop 'node_new - five words expected'

        ! parse "x" and "y" node positions
        ix0 = scan(tokens(1)%str, 'x')
        iy0 = scan(tokens(1)%str, 'y')
        ix1 = scan(tokens(1)%str, '-', back=.true.)
        if (ix0==0 .or. iy0==0 .or. ix1==0) &
        &   error stop 'node_new - expected delimiters not found'
        new%pos(1) = str_to_number(tokens(1)%str(ix0+1:ix1-1))
        new%pos(2) = str_to_number(tokens(1)%str(iy0+1:))

        ! parse "size", "used" and "avail"
        new%size = str_to_number(tokens(2)%str)
        new%used = str_to_number(tokens(3)%str)
        new%avail = str_to_number(tokens(4)%str)

        ! check that "size = used + avail"
        if (new%size /= new%used + new%avail) &
        &   error stop 'node_new - values do not add up'
    end function node_new


    function read_nodes(file) result(nodes)
        character(len=*), intent(in) :: file
        type(node_t), allocatable :: nodes(:)

        type(string_t), allocatable :: lines(:)
        integer :: i, n
        integer, parameter :: HEADER_LINES=2

        lines = read_strings(file)
        n = size(lines)-HEADER_LINES
        allocate(nodes(n))
        do i=HEADER_LINES+1, HEADER_LINES+n
            nodes(i-HEADER_LINES) = node_t(lines(i)%str)
        end do
    end function read_nodes


    integer function str_to_number(str) result(val)
        character(len=*), intent(in) :: str

        character(len=*), parameter :: SUFFICES='T%'
        integer :: ios, i
        i = scan(str, SUFFICES)
        if (i/=0) then
            read(str(:i-1),*,iostat=ios) val
        else
            read(str,*,iostat=ios) val
        end if
        if (ios /= 0) error stop 'str_to_number - error reading value'
    end function str_to_number


    subroutine hole_firstngb(node, node_ngb, distance)
        class(hole_t), intent(in) :: node
        class(djikstra_node_at), intent(out), allocatable :: node_ngb
        integer, intent(out) :: distance

        integer :: i, pos(2)
        allocate(node_ngb, source=node)
        select type(node_ngb)
        class is (hole_t)
            do i = 1, 4
                pos = node%hole + DIRS(:,i)
                if (node%grid%g(pos(1),pos(2))/=TYPE_FREE) cycle
                if (all(node%data==pos)) cycle
                ! position is allowed
                node_ngb%hole = pos
                node_ngb%flag = i
                exit
            end do
            ! return null state if nowhere to go
            if (i==4+1) node_ngb%flag = -1
        class default
            error stop 'hole_firstngb - unsupported type'
        end select
        distance = 1
    end subroutine


    subroutine hole_nextngb(node, node_ngb1, node_ngb2, distance)
        class(hole_t), intent(in) :: node
        class(djikstra_node_at), intent(in) :: node_ngb1
        class(djikstra_node_at), intent(out), allocatable :: node_ngb2
        integer, intent(out) :: distance

        integer :: i, pos(2), i0

        select type(node_ngb1)
        class is (hole_t)
            i0 = node_ngb1 % flag
        class default
            error stop 'hole_nextngb - unsupported type'
        end select

        allocate(node_ngb2, source=node)
        select type(node_ngb2)
        class is (hole_t)
            if (i0 < 1) error stop 'hole_nextngb - previous ngb invalid'
            do i = i0+1, 4
                pos = node%hole + DIRS(:,i)
                if (node%grid%g(pos(1),pos(2))/=TYPE_FREE) cycle
                if (all(node%data==pos)) cycle
                ! position is allowed
                node_ngb2%hole = pos
                node_ngb2%flag = i
                exit
            end do
            ! return null state if nowhere to go
            if (i==4+1) node_ngb2%flag = -1
        class default
            error stop 'hole_nextngb - unsupported type arg2'
        end select
        distance = 1
    end subroutine

    logical function hole_isequal(anode, bnode)
        class(hole_t), intent(in) :: anode
        class(djikstra_node_at), intent(in) :: bnode

        select type(bnode)
        class is (hole_t)
            hole_isequal=all(anode%hole==bnode%hole)
        class default
            error stop 'hole_isequal - second arg type not supported'
        end select
    end function

    logical function hole_isnull(node)
        class(hole_t), intent(in) :: node
        hole_isnull = node%flag==-1
    end function

    logical function hole_istarget(node)
        class(hole_t), intent(in) :: node
        hole_istarget = all(node%hole==node%target)
    end function



    logical function data_isequal(anode, bnode)
        class(data_t), intent(in) :: anode
        class(djikstra_node_at), intent(in) :: bnode

        select type(bnode)
        class is (data_t)
            data_isequal=all(anode%hole==bnode%hole) .and. &
            &            all(anode%data==bnode%data)
        class default
            error stop 'data_isequal - second arg type not supported'
        end select
    end function

    subroutine data_firstngb(node, node_ngb, distance)
        class(data_t), intent(in) :: node
        class(djikstra_node_at), intent(out), allocatable :: node_ngb
        integer, intent(out) :: distance

        type(djikstra_node_ptr), allocatable :: djwrk(:)
        type(hole_t) :: hole_start
        integer :: i, pos(2)

        allocate(node_ngb, source=node)
        select type(node_ngb)
        class is (data_t)
            do i = 1, 4
                pos = node%data + DIRS(:,i)
                if (node%grid%g(pos(1),pos(2))/=TYPE_FREE) cycle
                !if (all(node%data==pos)) cycle
                ! position is allowed
                node_ngb%data = pos
                node_ngb%flag = i
                exit
            end do
            if (i==4+1) then
                ! return null state if nowhere to go
                node_ngb%flag = -1
                distance = huge(distance)
            else
                ! how many steps to move a hole
                hole_start%grid => node%grid
                hole_start%hole = node%hole
                hole_start%data = node%data
                hole_start%target = node_ngb%data
                call djikstra_search(djwrk, hole_start, distance)
                distance = distance + 1
                node_ngb%hole = node%data
            end if

        class default
            error stop 'data_firstngb - unsupported type'
        end select
    end subroutine

    subroutine data_nextngb(node, node_ngb1, node_ngb2, distance)
        class(data_t), intent(in) :: node
        class(djikstra_node_at), intent(in) :: node_ngb1
        class(djikstra_node_at), intent(out), allocatable :: node_ngb2
        integer, intent(out) :: distance

        type(djikstra_node_ptr), allocatable :: djwrk(:)
        type(hole_t) :: hole_start
        integer :: i, pos(2), i0

        select type(node_ngb1)
        class is (data_t)
            i0 = node_ngb1 % flag
        class default
            error stop 'data_nextngb - unsupported type'
        end select

        allocate(node_ngb2, source=node)
        select type(node_ngb2)
        class is (data_t)
            if (i0 < 1) error stop 'data_nextngb - previous ngb invalid'
            do i = i0+1, 4
                pos = node%data + DIRS(:,i)
                if (node%grid%g(pos(1),pos(2))/=TYPE_FREE) cycle
                !if (all(node%data==pos)) cycle
                ! position is allowed
                node_ngb2%data = pos
                node_ngb2%flag = i
                exit
            end do
            ! return null state if nowhere to go
            if (i==4+1) then
                node_ngb2%flag = -1
                distance = huge(distance)
            else
                ! how many steps to move a hole
                hole_start%grid => node%grid
                hole_start%hole = node%hole
                hole_start%data = node%data
                hole_start%target = node_ngb2%data
                call djikstra_search(djwrk, hole_start, distance)
                distance = distance + 1
                node_ngb2%hole = node%data
            end if
        class default
            error stop 'hole_nextngb - unsupported type arg2'
        end select
    end subroutine

    logical function data_istarget(node)
        class(data_t), intent(in) :: node
        data_istarget = all(node%data==node%target)
    end function

end module day1622_mod