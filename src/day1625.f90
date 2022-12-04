! Notes: Reverse engineering the code
!
! At instruction [9], the value in register D defines the output pattern.
! The pattern is the value in a binary format. The minimum value to produce
! the required pattern is 2 ("10"). 
!
! However, before reaching instruction [9] there is a double loop increasing
! the initial value in register A --- 643*4 = 2572 times. The value in
! register D is therefore A_init  + 2572.
!
! 2572 in binary is "1010 0000 1100", the closest higher number with desired
! pattern is:       "1010 1010 1010" = 2730.
!
! We have to put to the register A the value: 2730 - 2572 = 158
! 
module day1625_mod
    use day1612_mod, only : assembunny_t
    implicit none
    private
    public day1625

contains
    subroutine day1625(file)
        character(len=*), intent(in) :: file

        type(assembunny_t) :: tower
        integer :: regD

        ! Find the required input
        tower = assembunny_t(file)
        tower%bp = [9] ! se tbreak point
        call tower%run()
        regD = tower%regs(4)

        print *, 'convert this number to binary: ',regD
        print *, 'find closest 101010... binary pattern.'
        print *, 'convert this pattern do decimal => X (2730)'
        print *, 'the answer to be put into A is:'
        print *, ' X (2730) - ',regD,' = 158'

        return

        ! Test the clock (infinite loop)
        tower = assembunny_t(file)
        tower%regs(1)= 158
        call tower%run()
    end subroutine day1625

end module day1625_mod