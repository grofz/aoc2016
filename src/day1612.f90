module day1612_mod
    use parse_mod, only : string_t, read_strings, split
    implicit none

    type instruction_t
        integer :: opcode
        logical :: absmode(2)
        integer :: args(2)
    end type
    interface instruction_t
        module procedure instruction_new
    end interface

    type assembunny_t
        type(instruction_t), allocatable :: list(:)
        integer :: regs(4)=0
        integer :: ip=1
    contains
        procedure :: step => assembunny_step
        procedure :: run => assembunny_run
    end type
    interface assembunny_t
        module procedure assembunny_new
    end interface

    integer, parameter :: OP_CPY=1, OP_INC=2, OP_DEC=3, OP_JNZ=4
    character(len=3), parameter :: OP_CHARS(4) = ['cpy','inc','dec','jnz']

contains

    subroutine day1612(file)
        character(len=*), intent(in) :: file

        type(assembunny_t) :: monorail
        integer :: ans1, ans2

        ! Part 1
        monorail = assembunny_t(file)
        call monorail%run()
        ans1 = monorail%regs(1)
        print '("IP=",i0,"  A-D=",4(i0,1x))', monorail%ip, monorail%regs
        print '("Answer 12/1 ",i0,l2)', ans1, ans1==318003

        ! Part 2
        monorail = assembunny_t(file)
        monorail%regs(3)=1
        call monorail%run()
        ans2 = monorail%regs(1)
        print '("IP=",i0,"  A-D=",4(i0,1x))', monorail%ip, monorail%regs
        print '("Answer 12/2 ",i0,l2)', ans2, ans2==9227657

    end subroutine



    type(assembunny_t) function assembunny_new(file) result(new)
        character(len=*), intent(in) :: file

        type(string_t), allocatable :: lines(:)
        integer :: i

        lines = read_strings(file)
        allocate(new%list(size(lines)))
        do i=1, size(lines)
            new%list(i) = instruction_t(lines(i)%str)
        end do
        new%regs = 0
        new%ip = 1
        print '("Assembunny instructions read ",i0)', size(new%list)
    end function assembunny_new



    type(instruction_t) function instruction_new(str) result(new)
        character(len=*), intent(in) :: str

        integer :: i, ios
        type(string_t), allocatable :: words(:)

        call split(str,' ',words)
        if (size(words)/=2 .and. size(words)/=3) &
        &   error stop 'instruction_new - 2 or 3 tokens expected'

        ! process one or two arguments
        do i=2,size(words)
            new%absmode(i-1)=.false.
            select case(words(i)%str(1:1))
            case('a')
                new%args(i-1)=1
            case('b')
                new%args(i-1)=2
            case('c')
                new%args(i-1)=3
            case('d')
                new%args(i-1)=4
            case default
                new%absmode(i-1)=.true.
                read(words(i)%str,*,iostat=ios) new%args(i-1)
                if (ios/=0) error stop 'instruction_new - cannot read integer'
            end select
        end do

        ! assign opcode
        do i=1,size(OP_CHARS)
            if (words(1)%str==OP_CHARS(i)) exit
        end do
        if (i==size(OP_CHARS)+1) error stop 'instruction_new - uknown opcode'
        new%opcode = i

        ! verify correct amount of arguments
        select case(new%opcode)
        case(OP_CPY, OP_JNZ)
            if (size(words)/=3) error stop 'instruction_new - two arguments expected'
        case(OP_INC, OP_DEC)
            if (size(words)/=2) error stop 'instruction_new - just one argument expected'
        end select
    end function instruction_new



    subroutine assembunny_step(this)
        class(assembunny_t), intent(inout) :: this

        integer :: val, val2
        logical :: isjump

        if (this%ip < 1 .or. this%ip > size(this%list)) then
            print '("Computer halts. Registers contain ",4(i0,1x))', this%regs
            this%ip = 0
            return
        end if

        associate(ins=>this%list(this%ip))
        select case(ins%opcode)
        case(OP_CPY)
            if (ins%absmode(1)) then
                val = ins%args(1)
            else
                val = this%regs(ins%args(1))
            end if
            if (ins%absmode(2)) error stop 'step - cpy second argument must reference a register'
            this%regs(ins%args(2)) = val
        case(OP_INC)
            if (ins%absmode(1)) error stop 'step - inc argument must reference a register'
            this%regs(ins%args(1)) = this%regs(ins%args(1)) + 1
        case(OP_DEC)
            if (ins%absmode(1)) error stop 'step - inc argument must reference a register'
            this%regs(ins%args(1)) = this%regs(ins%args(1)) - 1
        case(OP_JNZ)
            if (ins%absmode(1)) then
                val = ins%args(1)
            else
                val = this%regs(ins%args(1))
            end if

            isjump = .true.
            if (val==0) isjump=.false.
            if (isjump) then
                if (.not. ins%absmode(2)) error stop 'step - jnz second argument must reference to a value'
                val2 = ins%args(2)
                this%ip = this%ip + val2 - 1
            end if
        case default
            error stop 'step - uknown opcode'
        end select
        end associate
        this%ip = this%ip + 1
    end subroutine assembunny_step



    subroutine assembunny_run(this)
        class(assembunny_t), intent(inout) :: this

        do
            if (this%ip<1 .or. this%ip>size(this%list)) exit
            call this%step()
           !print '("IP=",i0,"  A-D=",4(i0,1x))', this%ip, this%regs
        end do
    end subroutine assembunny_run

end module day1612_mod