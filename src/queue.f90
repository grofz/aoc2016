
  module queue_mod
    use dll_mod
    implicit none
    private

    type, public :: queue_t
      type(node_t), pointer :: head => null()
      type(node_t), pointer :: tail => null()
      integer :: n = 0
      integer :: nmax = 0
    contains
      procedure :: enqueue => queue_enqueue
      procedure :: dequeue => queue_dequeue
      procedure :: front => queue_front
      procedure :: rear => queue_rear
      procedure :: isempty => queue_isempty
      procedure :: isfull => queue_isfull
      procedure :: size => queue_size
      procedure, private, pass(this) :: queue_copy
      generic :: assignment(=) => queue_copy
     !final :: queue_removeall
    end type

    interface queue_t
      module procedure queue_new
    end interface

  contains

    ! 1. Constructor, return a new queue of a maximum size "maxsize"
    function queue_new(maxsize) result(newq)
      type(queue_t) :: newq
      integer, intent(in) :: maxsize

      newq%nmax = maxsize
    end function


    ! 2. Add item "val" at the rear of the queue
    ! "ierr" should return "0" if operation successfull and non-zero
    ! if item could not be added (queue is full)
    subroutine queue_enqueue(this, val, ierr)
      class(queue_t), intent(inout) :: this
      integer, intent(in) :: val
      integer, intent(out), optional :: ierr

      integer :: ierr0
      type(node_t), pointer :: added

      if (this%n == this%nmax) then
        ierr0 = -1 ! queue is full
      else 
        ierr0 = 0
        added => dll_newnode(val)
        if (this%n==0) then
          ! first node in the queue
          this%head => added
        else
          ! add behind tail-node and move tail
          call dll_addnodebehind(this%tail, added)
        end if
        this%tail => added
        this%n = this%n + 1
      end if
      if (present(ierr)) ierr = ierr0
    end subroutine


    ! 3. Remove and return item "val" from the front of the queue
    ! "ierr" should return "0" if operation successfull and non-zero
    ! if item could not be returned (queue is empty)
    subroutine queue_dequeue(this, val, ierr)
      class(queue_t), intent(inout) :: this
      integer, intent(out) :: val
      integer, intent(out), optional :: ierr

      integer :: ierr0
      type(node_t), pointer :: removed

      if (this%n == 0) then
        ierr0 = -2 ! queue is empty
      else
        ierr0 = 0
        removed => this%head
        val = this%head%val
        call dll_removenode(removed, this%head)
        this%n = this%n - 1
        if (this%n == 0) this%tail => null()
      end if
      if (present(ierr)) ierr = ierr0
    end subroutine


    ! 4. Show item at the front of the queue.
    ! If queue is empty, "ierr" should return a non-zero value.
    function queue_front(this, ierr) result(val)
      integer :: val
      class(queue_t), intent(in) :: this
      integer, intent(out), optional :: ierr

      integer :: ierr0

      if (this%n==0) then
        ierr0 = -2 ! queue is empty
      else
        val = this%head%val
        ierr0 = 0
      end if
      if (present(ierr)) ierr = ierr0
    end function


    ! 5. Show item at the rear of the queue.
    ! If queue is empty, "ierr" should return a non-zero value.
    function queue_rear(this, ierr) result(val)
      integer :: val
      class(queue_t), intent(in) :: this
      integer, intent(out), optional :: ierr

      integer :: ierr0

      if (this%n==0) then
        ierr0 = -2 ! queue is empty
      else
        val = this%tail%val
        ierr0 = 0
      end if
      if (present(ierr)) ierr = ierr0
    end function


    ! 6. Return .true. if queue is empty
    function queue_isempty(this) result(isempty)
      logical :: isempty
      class(queue_t), intent(in) :: this
      isempty = this%n==0
    end function


    ! 7. Return .true. if queue is full (contains "maxsize" number of items)
    function queue_isfull(this) result(isfull)
      logical :: isfull
      class(queue_t), intent(in) :: this
      isfull = this%n==this%nmax
    end function


    ! 8. Return a number of items in the queue
    function queue_size(this) result(qsize)
      integer :: qsize
      class(queue_t), intent(in) :: this
      qsize = this%n
    end function


    ! 9. On return, "newq" contains a copy of queue "this"
    subroutine queue_copy(newq, this)
      type(queue_t), intent(inout) :: newq
      class(queue_t), intent(in) :: this

      newq%n = this%n
      newq%nmax = this%nmax
      newq%head => dll_copy(this%head)
      ! set tail pointer
      newq%tail => newq%head
      do
        ! if queue is empty
        if (.not. associated(newq%tail)) exit
        ! if tail pointer is set at the last node
        if (.not. associated(newq%tail%next)) exit
        ! move tail pointer otherwise
        newq%tail => newq%tail%next
      end do
    end subroutine


    ! 10. Free memory occupied by the queue
    subroutine queue_removeall(this)
      type(queue_t), intent(inout) :: this
      
      call dll_removeall(this%head)
      this%tail=>null()
      this%n=0
      this%nmax=0
    end subroutine

  end module queue_mod

