module day1604_mod
    use parse_mod, only : read_strings, string_t
    implicit none
    private
    public day1604

    type room_t
        type(string_t) :: name, chsum
        integer :: sid
    contains
        procedure :: isvalid => room_isvalid
        procedure :: decrypt => room_decrypt
    end type
    interface room_t
        module procedure room_new
    end interface room_t

contains

    subroutine day1604(file)
        character(len=*), intent(in) :: file

        type(string_t), allocatable :: lines(:)
        type(room_t), allocatable :: rooms(:)
        integer :: i, totsid, ans1, ans2

        ! Part 1
        lines = read_strings(file)
        allocate(rooms(size(lines)))
        totsid = 0
        do i=1,size(rooms)
            rooms(i) = room_t(lines(i)%str)
            if (rooms(i)%isvalid()) totsid = totsid + rooms(i)%sid
        end do
        ans1 = totsid

        ! Part 2
        ans2 = -1
        do i=1,size(rooms)
            if (.not. rooms(i)%isvalid()) cycle
            call rooms(i)%decrypt()
            print '(a,1x,i0)', rooms(i)%name%str, rooms(i)%sid
            if (rooms(i)%name%str=='northpole object storage') ans2=rooms(i)%sid
        end do
        print '("Answer 4/1 ",i0,l2)', ans1, ans1==361724
        print '("Answer 4/2 ",i0,l2)', ans2, ans2==482
    end subroutine day1604


    type(room_t) function room_new(str) result(new)
        character(len=*), intent(in) :: str

        integer :: l1, r1, r2
        l1 = scan(str,'[')
        r1 = scan(str,']')
        r2 = scan(str,'-',BACK=.true.)
        if (l1 == 0 .or. r1 == 0 .or. r2 == 0 ) &
        &   error stop 'room_new - invalid input'

        new%chsum = string_t(str(l1+1:r1-1))
        read(str(r2+1:l1-1),*) new%sid
        new%name = string_t(str(:r2-1))
    end function


    function frequency(str) result(freq)
        character(len=*), intent(in) :: str
        integer, parameter :: IMAX = iachar('z') - iachar('a') + 1
        integer :: freq(IMAX)

        integer :: j, n
        freq = 0
        do j=1,len_trim(str)
            if (str(j:j)=='-') cycle
            associate(i => iachar(str(j:j))-iachar('a')+1)
                if (i<1 .or. i>imax) then
                    print *, '%'//str(j:j)//'%'
                    error stop 'frequency - unexpected char in room name'
                end if
                freq(i) = freq(i) + 1
            end associate
        end do
    end function


    type(string_t) function calc_chsum(str) result(chsum)
        character(len=*), intent(in) :: str

        integer :: i, j
        integer, allocatable :: f(:)
        chsum = string_t('12345')
        f = frequency(str)
        do i=1,5
            j = maxloc(f, dim=1)
            chsum%str(i:i) = achar(iachar('a')-1+j)
            f(j) = -1
        end do
    end function


    logical function room_isvalid(this) result(isvalid)
        class(room_t), intent(in) :: this

        type(string_t) :: chsum
        chsum = calc_chsum(this%name%str)
        isvalid = this%chsum%str==chsum%str
    end function


    subroutine room_decrypt(this)
        class(room_t), intent(inout) :: this

        integer, parameter :: ALFNUM = iachar('z')-iachar('a') + 1
        integer :: i, a
        if (.not. this%isvalid()) return
        associate(name=>this%name%str)
        do i=1,len_trim(name)
            if (name(i:i)=='-') then
                name(i:i) = ' '
            else
                a = mod(this%sid + iachar(name(i:i)) - iachar('a') + 1, ALFNUM)
                if (a==0) a = ALFNUM
                name(i:i) = achar(a+iachar('a')-1)
            end if
        end do
        end associate
    end subroutine
end module day1604_mod

