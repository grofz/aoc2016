module day1610_mod
  use parse_mod, only : string_t, read_strings, split
  implicit none
  private

  public :: day1610
  integer, parameter :: NULL_VAL = -1

  type address_t
    logical :: is_output = .false.
    integer :: addr = NULL_VAL
  contains
    procedure :: to_str => address_to_str
  end type address_t
  interface address_t
    module procedure address_new
  end interface

  type value_t
    integer :: id = NULL_VAL
    type(address_t) :: origin = address_t(.true.,NULL_VAL)
    type(address_t), allocatable :: path(:)
  contains
      procedure :: add_to_path => value_add_to_path
      procedure :: is_complete => value_is_complete
  end type
  interface value_t
    module procedure value_new
  end interface

  type bot_t
    integer :: id = NULL_VAL
    type(address_t) :: lowval, highval
    integer :: nvals = 0
    integer :: vals(2) = NULL_VAL
  contains
    procedure :: bot_receive
    procedure :: bot_send
  end type
  interface bot_t
      module procedure bot_new
  end interface

  integer, parameter :: MAX_VALS = 1000, MAX_BOTS = 1000
  type(value_t) :: vals(0:MAX_VALS)
  type(bot_t) :: bots(0:MAX_BOTS)
  integer :: box_output(0:2) = NULL_VAL
  integer :: nvals, nbots
  integer, parameter :: VAL_ASKED(*) = [17, 61]

contains
  subroutine day1610(file)
    character(len=*), intent(in) :: file
    type(string_t), allocatable :: lines(:)
    type(string_t), allocatable :: curline(:)
    integer :: i
    logical :: all_complete
    type(bot_t) :: curbot
    type(value_t) :: curval
    integer, allocatable :: result1(:)
    
    lines = read_strings(file)
    nvals = 0
    nbots = 0
    do i = 1, size(lines)
      call split(lines(i)%str,' ',curline)
      select case(size(curline,1))
      case(6)
        curval = value_t(curline)
        if (curval%id < 0 .or. curval%id > MAX_VALS) error stop 'day1610: too many values or invalid id'
        if (vals(curval%id)%id /= NULL_VAL) error stop 'day1610: duplicit instruction for a value'
        vals(curval%id) = curval
        nvals = nvals + 1
      case(12)
        curbot = bot_t(curline)
        if (curbot%id < 0 .or. curbot%id > MAX_BOTS) error stop 'day1610: too many bots or invalid id'
        if (bots(curbot%id)%id /= NULL_VAL) error stop 'day1610: duplicit instuction for a bot'
        bots(curbot%id) = curbot
        nbots = nbots + 1
      case default
        error stop 'day1610: invalid line'
      end select
    end do

    ! Print what was read
    do i=0,MAX_VALS
      if (vals(i)%id==NULL_VAL) cycle
      print '("Value ",i0," goes ",a)', vals(i)%id, vals(i)%origin%to_str()
    end do
    print *
    do i=0,MAX_BOTS
      if (bots(i)%id==NULL_VAL) cycle
      print '("Bot ",i0," lower ",a," and higher ",a)', bots(i)%id, bots(i)%lowval%to_str(), bots(i)%highval%to_str()
    end do
    print *, 'End of input'
    print *
    
    ! Main loop
    do 
      print *, 'Transfer'
      call transfer()     
      call print_paths(all_complete, .true.)
      if (all_complete) exit
    end do
    call print_paths(all_complete, .false.)
    print *, 'RESULTS:'
    result1 = intersection(vals(VAL_ASKED(1))%path, vals(VAL_ASKED(2))%path)
    print '("Answer 1: ", *(i0,1x))', result1
    print *, box_output
    print '("Answer 2: ", i0)', box_output(0)*box_output(1)*box_output(2)
    
  end subroutine day1610



  subroutine transfer()
    integer :: i, j, tmpvals(2), idbot
    type(address_t) :: addr(2)

    do i=0,MAX_VALS
      if (vals(i)%id==NULL_VAL) cycle
      idbot = vals(i)%origin%addr
      call bots(idbot)%bot_receive(vals(i)%id)
    end do

    do i=0,MAX_BOTS
      if (bots(i)%id==NULL_VAL) cycle
      if (bots(i)%nvals/=2) cycle

      ! bot has two values, transfer
      call bots(i)%bot_send(addr,tmpvals)
      do j=1,2
        if (addr(j)%is_output) then
          call vals(tmpvals(j))%add_to_path(addr(j))
          print '("Value ",i0," reached output box ",i0)', tmpvals(j),addr(j)%addr
          if (addr(j)%addr <= 2) then
            ! Save what went to output boxes 0, 1, 2
            associate(ia=>addr(j)%addr, ival=>tmpvals(j))
              if (box_output(ia)==NULL_VAL) then
                box_output(ia)=ival
              else if (box_output(ia)/=ival) then
                error stop 'Error: more values into same box'
              end if
            end associate
          end if
        else
          call vals(tmpvals(j))%add_to_path(addr(j))
          call bots(addr(j)%addr)%bot_receive(tmpvals(j))
        end if
      end do
    end do

  end subroutine transfer


  subroutine print_paths(all_complete, be_quiet)
    integer :: i, j
    logical, intent(out) :: all_complete
    logical, intent(in)  :: be_quiet
    all_complete = .true.
    do i=0,MAX_VALS
      if (vals(i)%id==NULL_VAL) cycle            
      if (.not. vals(i)%is_complete()) all_complete=.false.
      if (be_quiet) cycle
      print '("Value ",i3," complete?",l2)',vals(i)%id, vals(i)%is_complete()
      print '(*(i0,:,", "))', (vals(i)%path(j)%addr,j=1,size(vals(i)%path,1))      
    end do
  end subroutine print_paths


  function address_new(text) result(new)
    type(address_t) :: new
    type(string_t), intent(in) :: text(:)

    if (size(text)/=3) error stop 'address_new: invalid tokens'
    if (text(1)%str /= 'to') error stop 'address_new: first token must be TO'
    select case(text(2)%str)
    case('bot')
      new%is_output = .false.
    case('output')
      new%is_output = .true.
    case default
      error stop 'address_new: second token not BOT our OUTPUT'     
    end select
    new%addr = text(3)%to_int()
  end function


  function value_new(text) result(new)
    type(value_t) :: new
    type(string_t), intent(in) :: text(:)

    if (size(text)/=6) error stop 'value_new: invalid number of tokens'
    if (text(1)%str /= "value") error stop 'value_new: first token must be VALUE'
    new%id = text(2)%to_int()
    if (text(3)%str /= 'goes') error stop 'value_new: third token expected GOES'
    new%origin = address_t(text(4:6))
    allocate(new%path(0))
  end function


  function bot_new(text) result(new)
    type(bot_t) :: new
    type(string_t), intent(in) :: text(:)

    if (size(text)/=12) error stop 'bot_new: invalid number of tokens'
    if (text(1)%str /= 'bot') error stop 'bot_new: first token expected BOT'
    new%id = text(2)%to_int()
    if (text(3)%str /= 'gives' .and. text(4)%str/='low') error stop 'bot_new: gives low expected'
    new%lowval = address_t(text(5:7))
    if (text(8)%str /= 'and' .and. text(9)%str/='high') error stop 'bot_new: and high expected'
    new%highval = address_t(text(10:12))
  end function


  function address_to_str(this) result(str)
    class(address_t), intent(in) :: this
    character(len=:), allocatable :: str
    character(len=10) :: strtmp, strtmp2
    if (this%is_output) then
      strtmp = 'to output'
    else 
      strtmp = 'to bot'
    end if
    write(strtmp2,'(i0)') this%addr
    str = trim(strtmp)//' '//trim(strtmp2)
  end function


  subroutine bot_receive(this, val)
    class(bot_t), intent(inout) :: this
    integer, intent(in) :: val

    select case(this%nvals)
    case(2)
      if (val==this%vals(1) .or. val==this%vals(2)) then
        !print '("bot ",i0," already has ",i0)',this%id,val
      else
        error stop 'bot_receive: bot is full'
      end if
    case(1)
      if (this%vals(1) > val) then
        this%vals(2) = this%vals(1)
        this%vals(1) = val
        this%nvals = 2
      else if (this%vals(1) == val) then
        !print '("bot ",i0," already has ",i0)',this%id,val
      else
        this%vals(2) = val
        this%nvals = 2
      end if
      
    case(0)
      this%vals(1) = val
      this%nvals = 1
    end select    
  end subroutine


  subroutine bot_send(this,addr,sendvals)
    class(bot_t), intent(inout) :: this
    type(address_t), intent(out) :: addr(2)
    integer, intent(out) :: sendvals(2)
    if (this%nvals /= 2) error stop 'bot_send: bot not ready'
    addr(1) = this%lowval
    addr(2) = this%highval
    sendvals = this%vals
    this%nvals = 0
  end subroutine


  subroutine value_add_to_path(this,addr)
    class(value_t), intent(inout) :: this
    type(address_t), intent(in) :: addr

    integer :: i, nfound
    
    ! add address to the path (if not already there)
    if (.not. allocated(this%path)) allocate(this%path(0))
    nfound = 0
    do i=1,size(this%path)
      if (this%path(i)%addr==addr%addr .and. &
          (this%path(i)%is_output .eqv. addr%is_output)) nfound = nfound+1
    end do
    if (nfound==0) then
      this%path = [this%path, addr]
    else if (nfound==1) then
      !print '("Value ",i0," address ",a," already in path")',this%id, addr%to_str()
    else 
      print *, 'ERRROR FRO VALUE ',this%id, nfound, size(this%path)
      print *, (this%path(i)%addr,this%path(i)%is_output,i=1,size(this%path))
      print *, addr%addr, addr%is_output
      error stop 'value_add_to_path: more than one occurence'
    end if
  end subroutine
  
  
  logical function value_is_complete(this) result(is_complete)
    class(value_t), intent(in) :: this
    integer :: n
    is_complete = .false.
    n = 0
    if (allocated(this%path)) n = size(this%path)
    if (n>0) is_complete = this%path(n)%is_output
  end function


  function intersection(a, b) result (res)
    integer, allocatable :: res(:)
    type(address_t), intent(in) :: a(:), b(:)
    integer :: i1, i2, n1, n2

    allocate(res(0))
    n1 = size(a)
    n2 = size(b)
    do i1=1,n1
      if (a(i1)%is_output) cycle
      do i2=1,n2
        if (b(i2)%is_output) cycle
        if (a(i1)%addr==b(i2)%addr) res = [res, a(i1)%addr]
      end do
    end do
  end function


end module
