module day1624_mod
  use permutation_mod, only : permutation_generator_t
  use parse_mod, only : read_pattern
  use djikstra_mod, only : djikstra_node_at, djikstra_node_ptr, djikstra_search
  implicit none
  private
  public day1624

  type, extends(djikstra_node_at) :: state_t
    character(len=1), pointer :: board(:,:) => null()
    integer, pointer :: pois(:,:) => null()
    integer :: curpos(2)
  contains
    procedure :: nextngb => state_nextngb
    procedure :: isequal => state_isequal
    procedure :: istarget => state_istarget
  end type

  integer, parameter :: DIRS(2,4) = reshape([-1,0, 0,1, 1,0, 0,-1],[2,4])
  character(len=1), parameter :: CH_WALL = '#'

contains
  subroutine day1624(file)
    character(len=*), intent(in) :: file

    character(len=1), allocatable, target :: board(:,:)
    integer, allocatable, target :: pois(:,:)
    integer, allocatable :: dm(:,:), arr(:)
    type(permutation_generator_t) :: generator
    integer :: i, totd, iprev, ans1, ans2
    logical :: isok

    board = read_pattern(file)
    call extract_positions(board, pois)
    call distance_map(board, pois, dm)
    print '("Distance map of HVAC:")'
    do i=lbound(dm,1), ubound(dm,1)
      print '(i1," | ",*(i3,1x))', i, dm(i,:)
    end do

    ! Generate all permutations of POIS nodes (except "0")
    arr = [(i, i=1, ubound(dm,1))]
    call generator%init(arr)
    ans1 = huge(ans1)
    ans2 = huge(ans2)
    PERMUTATIONS: do
      call generator%next(arr,isok)
      if (.not. isok) exit

      ! calculate route distance
      iprev = 0
      totd = 0
      do i=1,size(arr)
        totd = totd + dm(iprev,arr(i))
        iprev = arr(i)
      end do
      if (totd < ans1) ans1 = totd

      ! add distance to the starting position
      totd = totd + dm(iprev, 0)
      if (totd < ans2) ans2 = totd
    end do PERMUTATIONS

    print '("Shortest distance (24/1) ",i0,l2)', ans1, ans1==448
    print '("Shortest with returning to start (24/2) ",i0,l2)', ans2, ans2==672
  end subroutine day1624


  subroutine extract_positions(board, pos)
    character(len=1), intent(in) :: board(:,:)
    integer, allocatable, intent(out) :: pos(:,:)

    integer :: maxv, i, wrk(2,0:9)

    maxv = -1
    do i=0,9
      wrk(:,i) = findloc(board,achar(iachar('0')+i))
      if (.not. all(wrk(:,i)==0)) maxv = i
    end do
    allocate(pos(2,0:maxv))
    pos = wrk(:,0:maxv)
  end subroutine extract_positions


  subroutine print_pattern(arr)
    character(len=1), intent(in) :: arr(:,:)

    integer :: j, i
    do j=1,size(arr,2)
      write(*,'(*(a))') (arr(i,j), i=1,size(arr,1))
    end do
  end subroutine 


  subroutine state_nextngb(node, flag, node_ngb, distance)
    class(state_t), intent(in) :: node
    integer, intent(inout) :: flag
    class(djikstra_node_at), allocatable, intent(out) :: node_ngb
    integer, intent(out) :: distance

    integer :: i

    distance = 1
    allocate(node_ngb, source=node)
    do i = flag+1, 4
      select type(node_ngb)
      class is (state_t)
        node_ngb%curpos = node%curpos + DIRS(:,i)
        ! discard impossible moves (out of board or into wall)
        if (any(node_ngb%curpos<1)) cycle
        if (node_ngb%curpos(1)>size(node_ngb%board,1)) cycle
        if (node_ngb%curpos(2)>size(node_ngb%board,2)) cycle
        if (node_ngb%board(node_ngb%curpos(1),node_ngb%curpos(2))==CH_WALL) cycle
        ! move is valid
        exit
      end select
    end do
    if (i==4+1) then
      flag = 0
    else
      flag = i
    end if
  end subroutine state_nextngb


  logical function state_isequal(anode, bnode) result(isequal)
    class(state_t), intent(in) :: anode
    class(djikstra_node_at), intent(in) :: bnode
    select type(bnode)
    class is (state_t)
      isequal = all(anode%curpos==bnode%curpos)
    class default
      error stop 'state_isequal - invalid class'
    end select
  end function


  logical function state_istarget(node)
    class(state_t), intent(in) :: node
    ! searching whole domain, there is no target
    state_istarget = .false.
  end function


  subroutine distance_map(board, pois, dm)
    character(len=1), intent(in), target :: board(:,:)
    integer, intent(in), target :: pois(:,0:)
    integer, allocatable, intent(out) :: dm(:,:)
!
! Compute distance matrix between all pairs of points-of-interest
!
    type(state_t) :: start
    type(djikstra_node_ptr), allocatable :: dnodes(:)
    integer :: ibeg, iend, i, inode, j, d
    integer, parameter :: NULLVAL = -9

    ibeg = lbound(pois, dim=2)
    iend = ubound(pois, dim=2)
    allocate(dm(ibeg:iend, ibeg:iend))
    dm = NULLVAL
    start%board => board
    start%pois => pois

    DJIKSTRA: do i = ibeg, iend
      if (allocated(dnodes)) deallocate(dnodes)
      start%curpos = pois(:,i)
      call djikstra_search(dnodes, start, d)
      do inode = 1, size(dnodes)
        select type (a=>dnodes(inode)%ptr)
        class is(state_t)
          do j = ibeg, iend
            if (all(pois(:,j)==a%curpos)) then
              if (dm(i,j)/=NULLVAL) error stop 'distance_map - overwrite not expected'
              dm(i,j) = a%d
            end if
          end do
        end select
      end do
    end do DJIKSTRA
  end subroutine
end module day1624_mod