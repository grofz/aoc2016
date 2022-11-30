module day1617_mod
    use day1605_mod, only : md5
    use djikstra_mod, only : djikstra_search, djikstra_node_at, djikstra_node_ptr
    use parse_mod, only : string_t
    implicit none

    integer, parameter :: MAXX=4, MAXY=4
    integer, parameter :: DIR(2,4) = reshape([0,-1, 0,1, -1,0, 1,0], [2,4])
    character(len=1), parameter :: LDIR(4) = ['U', 'D', 'L', 'R']

    type, extends(djikstra_node_at) :: state_t
        integer :: pos(2)
        type(string_t) :: path
        type(string_t) :: salt
        character(len=32) :: hash
    contains
        procedure :: nextngb => state_nextngb
        procedure :: isequal => state_isequal
        procedure :: istarget => state_istarget
    end type
    interface state_t
        module procedure state_new
    end interface
contains

    subroutine day1617(salt)
        character(len=*), intent(in) :: salt

        type(state_t) :: s
        type(djikstra_node_ptr), allocatable :: dwrk(:)
        type(string_t) :: ans1, path_for_ans2
        integer :: nlen, i, ans2

        s = state_t(salt)

        ! Part 1
        call djikstra_search(dwrk, s, nlen)
        print '("Shortest path ",i0)', nlen
        do i=1,size(dwrk)
            select type(st=>dwrk(i)%ptr)
            class is(state_t)
                if (st%istarget()) ans1=string_t(st%path%str)
            class default
                error stop 'day 17 - invalid type'
            end select
        end do
        print '("Answer 17/1 ",a,l2)', ans1%str, ans1%str=='RDRDUDLRDR'

        ! Part 2
        call find_longest_path(s, path_for_ans2)
        print '("Longest path ",a)',path_for_ans2%str
        ans2 = len_trim(path_for_ans2%str)
        print '("Answer 17/2 ",i0,l2)', ans2, ans2==386
    end subroutine day1617


    recursive subroutine find_longest_path(node, path)
        type(state_t), intent(in) :: node
        type(string_t), intent(out) :: path

        class(djikstra_node_at), allocatable :: ngb
        type(string_t) :: path0
        integer :: flag, i, itmp

        ! At the vault -> non-recursive answer
        if (node%istarget()) then
            path = string_t(node%path%str)
            return
        end if

        ! If dead-end reached, empty path will be returned
        path = string_t('')

        ! Recursively explore all next steps
        flag = 0
        do
            call node%nextngb(flag, ngb, itmp)
            if (flag==0) exit
            select type(ngb)
            type is (state_t)
                call find_longest_path(ngb, path0)
            class default
                error stop 'invalid type'
            end select

            ! Keep the longest path found so far
            if (len_trim(path0%str) > len_trim(path%str)) then
                path = string_t(path0%str)
            end if
        end do
    end subroutine


    type(state_t) function state_new(salt) result(new)
        character(len=*), intent(in) :: salt

        new%pos = [1,1]
        new%path = string_t('')
        new%salt = string_t(salt)
        new%hash = md5(new%salt%str)
    end function


    logical function state_isequal(anode, bnode) result(isequal)
        class(state_t), intent(in) :: anode
        class(djikstra_node_at), intent(in) :: bnode

        select type(bnode)
        class is (state_t)
            isequal = anode%path%str==bnode%path%str
        class default
            error stop 'state_isequal - unknown type'
        end select
    end function


    logical function state_istarget(node) result(istarget)
        class(state_t), intent(in) :: node
        istarget = all(node%pos==[MAXX, MAXY])
    end function


    subroutine state_nextngb(node, flag, node_ngb, distance)
        class(state_t), intent(in) :: node
        integer, intent(inout) :: flag
        class(djikstra_node_at), allocatable, intent(out) :: node_ngb
        integer, intent(out) :: distance

        integer :: i0, i, pos(2)

        distance = 1
        if (flag==0) then
            ! first neighbour
            i0 = 0
        else if (flag > 0 .and. flag <= 4) then
            ! second, third, fourth, no-more neighbours
            i0 = flag
        else
            error stop 'state_nextngb - invalid flag received'
        end if

        allocate(node_ngb, source=node)
        select type(node_ngb)
        class is (state_t)
        do i = i0+1, 4
            ! skip closed doors or out-of the board moves
            if (.not. isopen(node%hash(i:i))) cycle
            pos = node%pos + DIR(:,i)
            if (any(pos<1) .or. any(pos>[MAXX,MAXY])) cycle

            ! it is possible to move in the current direction
            node_ngb%pos = pos
            node_ngb%path = string_t(node%path%str//LDIR(i))
            node_ngb%hash = md5(node_ngb%salt%str//node_ngb%path%str)
            flag = i
            exit
        end do
        class default
            error stop 'state_nextngb - invalid type'
        end select

        ! indicate null-state if nowhere to go
        if (i==4+1) flag = 0
    end subroutine


    logical function isopen(ch)
        character(len=1), intent(in) :: ch
        select case(ch)
        case('B', 'C', 'D', 'E', 'F')
            isopen = .true.
        case('A', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9')
            isopen = .false.
        case default
            error stop 'isopen - invalid character'
        end select
    end function

end module day1617_mod