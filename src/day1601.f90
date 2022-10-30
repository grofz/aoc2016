module day1601_mod
  use parse_mod, only : read_strings, string_t, split
  implicit none
  private
  public day1601

  type state_t
    integer :: ori = 0 ! 0=north, 1=right, 2=south, 3=left
    integer :: x = 0, y = 0
    integer :: lim0(2) = 0
    integer :: lim1(2) = 0
  contains
    procedure move
  end type

  integer, parameter :: DEL(2,0:3) = reshape([0,1, 1,0, 0,-1, -1,0], [2,4])

contains
  subroutine day1601(file)
    character(len=*), intent(in) :: file
    type(string_t), allocatable :: lines(:), moves(:)
    integer :: i
    type(state_t) :: me, me2
    integer, allocatable :: vis(:,:)
    logical :: hq_found

    lines = read_strings(file)
    if (size(lines)/=1) error stop 'day1601: input more than one line'
    call split(lines(1)%str,',',moves)
    do i=1,size(moves)
      write(*,'(i0,1x,i0,1x,i0)',advance='no') me%ori, me%x, me%y 
      write(*,'(1x,a5,1x)',advance='no') moves(i)%str
      call me%move(moves(i)%str)
      write(*,'(i0,1x,i0,1x,i0)',advance='yes') me%ori, me%x, me%y
    end do

    print '("Answer 1: ",i0)', abs(me%x)+abs(me%y)

    allocate(vis(me%lim0(1):me%lim1(1), me%lim0(2):me%lim1(2)))
    vis = 0
    vis(me2%x,me2%y) = 1
    do i=1,size(moves)
      call me2%move(moves(i)%str, vis, hq_found)
      if (hq_found) exit
    end do
    print '("Answer 2: ",i0)', abs(me2%x)+abs(me2%y)

  end subroutine


  subroutine move(this, movement, vis, hq_found)
    !use iso_fortran_env, only :
    class(state_t), intent(inout) :: this
    character(len=*), intent(in) :: movement
    integer, intent(inout), optional, allocatable :: vis(:,:)
    logical, intent(out), optional :: hq_found
    character(len=:), allocatable :: movement0
    integer :: distance, ios, i

    movement0 = trim(adjustl(movement))
    read(movement0(2:),*,iostat=ios) distance
    if (ios/=0) error stop 'mode - reading error'
    select case(movement0(1:1))
    case('L','l')
      this % ori = mod(this%ori+4-1, 4)
    case('R','r')
      this % ori = mod(this%ori+1, 4)
    case default
      error stop 'move - invalid command'
    end select
    if (.not. present(vis)) then
      this%x = this%x + distance*DEL(1,this%ori)
      this%y = this%y + distance*DEL(2,this%ori)
      if (this%x > this%lim1(1)) this%lim1(1) = this%x
      if (this%x < this%lim0(1)) this%lim0(1) = this%x
      if (this%y > this%lim1(2)) this%lim1(2) = this%y
      if (this%y < this%lim0(2)) this%lim0(2) = this%y

    else 
      if (.not. present(hq_found)) error stop '(move: hq_found must be present'
      hq_found = .false.
      do i=1, distance
        this%x = this%x + DEL(1,this%ori)
        this%y = this%y + DEL(2,this%ori)
        if (vis(this%x,this%y)/=0) then
          hq_found = .true.
          exit
        else
          vis(this%x,this%y) = 1
        end if
      end do
    end if        
  end subroutine
end module