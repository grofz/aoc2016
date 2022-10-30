module day1603_mod
    use parse_mod, only : read_strings, string_t, split
    implicit none

    type :: triangle_t
        integer :: x(3)
    contains
        procedure :: valid
    end type
    interface triangle_t
        module procedure triangle_new
    end interface

contains
    subroutine day1603(file)
        character(len=*), intent(in) :: file
        type(triangle_t), allocatable :: triangles(:), coltrigs(:)
        type(string_t), allocatable :: lines(:), chop1(:), chop2(:), chop3(:)
        integer :: i, n

        lines = read_strings(file)
        allocate(triangles(size(lines,1)))
        allocate(coltrigs(size(lines,1)))        
        if (mod(size(lines,1),3)/=0) error stop 'lines must be divisible by three'
        
        ! read triangles row-wise (Part 1)
        n = 0
        do i=1,size(triangles)
            triangles(i) = triangle_t(lines(i)%str)
            print *, triangles(i)%valid(), triangles(i)%x, lines(i)%str
            if (triangles(i)%valid()) n = n+1
        end do
        print '("Answer 1 ",i0)', n

        ! read triangles column-wise (Part 2)
        n = 0
        do i=1,size(coltrigs)-2,3
            call split(lines(i+0)%str,' ',chop1)
            call split(lines(i+1)%str,' ',chop2)
            call split(lines(i+2)%str,' ',chop3)
            chop1 = sanitize_chop(chop1)
            chop2 = sanitize_chop(chop2)
            chop3 = sanitize_chop(chop3)
            if (size(chop1)/=3 .or. size(chop2)/=3 .or. size(chop3)/=3) error stop 'choping line error'
            coltrigs(i+0) = triangle_t(chop1(1)%str//' '//chop2(1)%str//' '//chop3(1)%str)
            coltrigs(i+1) = triangle_t(chop1(2)%str//' '//chop2(2)%str//' '//chop3(2)%str)
            coltrigs(i+2) = triangle_t(chop1(3)%str//' '//chop2(3)%str//' '//chop3(3)%str)            
        end do
        do i=1,size(coltrigs)
            if (coltrigs(i)%valid()) n = n+1
        end do
        print '("Answer 2 ",i0)', n

    end subroutine day1603


    function sanitize_chop(raw) result(res)
        type(string_t), intent(in) :: raw(:)
        type(string_t), allocatable :: res(:)
        integer :: i
        ! return array of strings with empty strings removed
        allocate(res(0))
        do i=1,size(raw)
            if (len_trim(raw(i)%str)==0) cycle
            res = [res, raw(i)]
        end do
    end function


    function triangle_new(ch) result(new)
        type(triangle_t) :: new
        character(len=*), intent(in) :: ch
        integer :: ios, itmp
        read(ch,*,iostat=ios) new%x
        if (ios/=0) error stop 'triangle_new I/O error'

        ! sort sides
        if (new%x(1) > new%x(2) .or. new%x(1) > new%x(3)) then        
            itmp = new%x(1)
            if (new%x(2) <= new%x(3)) then
                new%x(1) = new%x(2)
                new%x(2) = itmp
            else
                new%x(1) = new%x(3)
                new%x(3) = itmp
            end if
        end if
        if (new%x(2) > new%x(3)) then
            itmp = new%x(2)
            new%x(2) = new%x(3)
            new%x(3) = itmp
        end if               
    end function


    logical function valid(this)
        class(triangle_t), intent(in) :: this
        ! Sides must be ordered to work correctly
        valid = .false.
        if (this%x(1)+this%x(2)>this%x(3)) valid = .true.
    end function
end module