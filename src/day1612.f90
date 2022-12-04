module day1612_mod
    use parse_mod, only : string_t, read_strings, split
    implicit none

    type instruction_t
        integer :: opcode
        logical :: absmode(2)
        integer :: args(2)
    contains
        procedure :: print => instruction_print
    end type
    interface instruction_t
        module procedure instruction_new
    end interface

    type assembunny_t
        type(instruction_t), allocatable :: list(:)
        integer :: regs(4)=0
        integer :: ip=1
        logical :: isdebug = .false.
        integer, allocatable :: bp(:)
    contains
        procedure :: step => assembunny_step
        procedure :: run => assembunny_run
    end type
    interface assembunny_t
        module procedure assembunny_new
    end interface

    integer, parameter :: OP_CPY=1, OP_INC=2, OP_DEC=3, OP_JNZ=4, OP_TGL=5, OP_OUT=6
    character(len=3), parameter :: OP_CHARS(6) = ['cpy','inc','dec','jnz','tgl','out']

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
        case(OP_CPY, OP_JNZ) ! two-argument instructions
            if (size(words)/=3) error stop 'instruction_new - two arguments expected'
        case(OP_INC, OP_DEC, OP_TGL, OP_OUT) ! one-argument instructions
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
        if (this%isdebug) call this%list(this%ip)%print(this%ip)

        associate(ins=>this%list(this%ip))
        select case(ins%opcode)
        case(OP_CPY)
            if (ins%absmode(1)) then
                val = ins%args(1)
            else
                val = this%regs(ins%args(1))
            end if
            if (ins%absmode(2)) then
                !error stop 'step - cpy second argument must reference a register'
                print *, 'cpy ignored as the second argument is not a register'
            else
                this%regs(ins%args(2)) = val
            end if
        case(OP_INC)
            if (ins%absmode(1)) error stop 'step - inc argument must reference a register'
            this%regs(ins%args(1)) = this%regs(ins%args(1)) + 1
        case(OP_DEC)
            if (ins%absmode(1)) error stop 'step - inc argument must reference a register'
            this%regs(ins%args(1)) = this%regs(ins%args(1)) - 1
        case(OP_OUT)
            if (ins%absmode(1)) then
                val = ins%args(1)
            else
                val = this%regs(ins%args(1))
            end if
            print '("Signal output ",i0,5x,4(i0,1x))', val, this%regs
        case(OP_JNZ)
            if (ins%absmode(1)) then
                val = ins%args(1)
            else
                val = this%regs(ins%args(1))
            end if

            isjump = .true.
            if (val==0) isjump=.false.
            if (isjump) then
                if (ins%absmode(2)) then
                    val2 = ins%args(2)
                else
                    val2 = this%regs(ins%args(2))
                end if
                this%ip = this%ip + val2 - 1
           end if
        
        case(OP_TGL)
            if (ins%absmode(1)) then
                val = ins%args(1)
            else
                val = this%regs(ins%args(1))
            end if
            val2 = this%ip+val
            if (val2 >= 1 .and. val2 <= size(this%list)) then
                associate(op=>this%list(val2)%opcode)
                    if (op==OP_JNZ) then
                        op = OP_CPY
                    else if (op==OP_CPY) then
                        op = OP_JNZ
                    else if (op==OP_INC) then
                        op = OP_DEC
                    else ! OP_DEC, OP_TGL
                        op = OP_INC
                    end if
                end associate
            else
                print *, 'tgl has no effect as it points out of instruction range'
            end if
        case default
            error stop 'step - uknown opcode'
        end select
        end associate
        this%ip = this%ip + 1
        if (this%isdebug) print '(4(i9,1x))', this%regs
    end subroutine assembunny_step



    subroutine assembunny_run(this)
        class(assembunny_t), intent(inout) :: this

        do
            if (this%ip<1 .or. this%ip>size(this%list)) exit
            if (allocated(this%bp)) then
                if (findloc(this%bp, this%ip, dim=1)/=0) then
                    print '("BP=",i0,"  A-D=",4(i0,1x))', this%ip, this%regs
                    exit
                end if
            end if
            call this%step()
           !print '("IP=",i0,"  A-D=",4(i0,1x))', this%ip, this%regs
        end do
    end subroutine assembunny_run



    subroutine instruction_print(this, ip)
        class(instruction_t), intent(in) :: this
        integer, intent(in) :: ip

        character(len=20) :: cargs(2)
        integer :: i

        do i=1,2
            if (this%absmode(i)) then
                write(cargs(i),'(i0)') this%args(i)
            else
                write(cargs(i),'(a)') achar(iachar('a')-1+this%args(i))
            end if
        end do
        if (this%opcode==OP_CPY .or. this%opcode==OP_JNZ) then
            write(*,'("[",i0,"] ",a,1x,a,1x,a)') &
            &  ip, OP_CHARS(this%opcode), trim(cargs(1)), trim(cargs(2))
        else
            write(*,'("[",i0,"] ",a,1x,a)') &
            &  ip, OP_CHARS(this%opcode), trim(cargs(1))
        end if

    end subroutine


end module day1612_mod