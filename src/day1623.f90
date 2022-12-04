module day1623_mod
    use day1612_mod, only : assembunny_t
    implicit none

contains

    subroutine day1623(file)
        character(len=*), intent(in) :: file

        type(assembunny_t) :: keypad
        integer :: ans1, ans2
        integer, parameter :: INP_TO_A(2) = [7, 12]

        ! Part One
        keypad = assembunny_t(file)
        keypad%regs(1) = INP_TO_A(1)
        !keypad%isdebug = .true.
        call keypad%run()
        print *, keypad%regs
        ans1 = keypad%regs(1)
        print '("Answer 23/1 ",i0,l2)', ans1, ans1==11748

        ! Part Two
        keypad = assembunny_t(file)
        keypad%regs(1) = INP_TO_A(2)
        !keypad%isdebug = .true.
        call keypad%run()
        ans2 = keypad%regs(1)
        print '("Answer 23/2 ",i0,l2)', ans2, ans2==479008308
    end subroutine day1623
end module day1623_mod