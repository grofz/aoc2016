module day1602_mod
    use parse_mod, only : read_strings, string_t
    implicit none
    private
    public day1602

    type key_t
        integer :: x = 2
        integer :: y = 2
    contains
        procedure :: pos_to_key, pos_valid
        procedure :: move_pos
    end type

    character(len=1), parameter :: NULL_CH = '#'
    type, extends(key_t) :: newkey_t
        character(len=1) :: map(5,5) = NULL_CH
    contains
        procedure :: pos_to_key => new_pos_to_key
        procedure :: pos_valid => new_pos_valid
    end type 
    interface newkey_t
        module procedure newkey_new
    end interface

contains
    subroutine day1602(file)
        character(len=*), intent(in) :: file
        type(string_t), allocatable :: lines(:)
        integer :: i, j
        type(key_t) :: me
        type(newkey_t) :: you

        lines = read_strings(file)
        you = newkey_t()
        do i=1,size(lines)
            !print '(a)', lines(i)%str
            !print *
            do j=1,len(lines(i)%str)
                call me%move_pos(lines(i)%str(j:j))
                call you%move_pos(lines(i)%str(j:j))
            end do
            print '("Position : ",a,"   ",a)', me%pos_to_key(), you%pos_to_key()
        end do
    end subroutine

    logical function pos_valid(this) result(valid)
        class(key_t), intent(in) :: this
        valid = .true.
        if (this%x < 1 .or. this%x > 3 .or. this%y < 1 .or. this%y >3) valid = .false.
    end function

    logical function new_pos_valid(this) result(valid)
        class(newkey_t), intent(in) :: this
        valid = .true.
        if (this%x < 1 .or. this%x > 5 .or. this%y < 1 .or. this%y >5) valid = .false.
        if (valid) then
            if (this%map(this%x, this%y)==NULL_CH) valid = .false.
        end if
    end function        

    character(len=1) function pos_to_key(this) result(keych)
        class(key_t), intent(in) :: this
        integer :: key
        if (.not. this%pos_valid())  error stop 'invalid position'
        key = 3*(this%y-1) + this%x
        write(keych,'(i1)') key
    end function

    character(len=1) function new_pos_to_key(this) result(key)
        class(newkey_t), intent(in) :: this
        if (.not. this%pos_valid()) error stop 'invalid position2'
        key = this%map(this%x,this%y)
    end function

    subroutine move_pos(this, move)
        class(key_t), intent(inout) :: this
        character(len=1), intent(in) :: move

        class(key_t), allocatable :: a
        
        ! only valid moves allowed
        allocate(a, source=this)
        select case(move)
        case('L')
            a%x = a%x - 1
        case('R')
            a%x = a%x + 1
        case('D')
            a%y = a%y + 1
        case('U')
            a%y = a%y - 1
        case default
            error stop 'move_pos: invalid instruction'
        end select
        if (a%pos_valid()) then
            this % x = a%x
            this % y = a%y
        end if
    end subroutine

    function newkey_new() result(new)
        type(newkey_t) :: new
        new%x = 1
        new%y = 3
        new%map(1,3) = '5'
        new%map(2,2:4) = ['2', '6', 'A']
        new%map(3,1:5) = ['1', '3', '7', 'B', 'D']
        new%map(4,2:4) = ['4', '8', 'C']
        new%map(5,3) = '9'
    end function

end module