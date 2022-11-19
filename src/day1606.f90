module day1606_mod
    use parse_mod, only : read_strings, string_t
    implicit none
    private
    public day1606
contains
    subroutine day1606(file)
        character(len=*), intent(in) :: file

        type(string_t), allocatable :: lines(:)
        integer :: freq(iachar('z')-iachar('a')+1)
        character(len=:), allocatable :: message1, message2
        integer :: i, j, k, k1, k2

        lines = read_strings(file)
        message1 = ''
        message2 = ''
        do j=1,len_trim(lines(1)%str)
            freq = 0
            do i=1,size(lines)
                associate(ch=>lines(i)%str(j:j))
                k = iachar(ch)-iachar('a')+1
                freq(k)=freq(k)+1
                end associate
            end do
            k1 = maxloc(freq,1)
            k2 = minloc(freq,1,mask=freq>0)
            message1 = message1//achar(k1+iachar('a')-1)
            message2 = message2//achar(k2+iachar('a')-1)
        end do
        print '("Answer 6/1 ",a,l2)', message1, message1=='umcvzsmw'
        print '("Answer 6/2 ",a,l2)', message2, message2=='rwqoacfz'
    end subroutine
end module day1606_mod