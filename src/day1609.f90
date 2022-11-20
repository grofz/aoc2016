module day1609_mod
    use parse_mod, only : string_t, read_strings
    use iso_fortran_env, only : I8B => int64
    implicit none

    type, extends(string_t) :: cmp_t
    contains
        procedure :: decompress
        procedure :: len => cmp_len
    end type

contains

    subroutine day1609(file)
        character(len=*), intent(in) :: file

        type(string_t), allocatable :: lines(:)
        type(cmp_t), allocatable :: clines(:)
        integer :: n, i, ans1 
        integer(I8B) :: ans2

        lines = read_strings(file)
        n = size(lines)
        allocate(clines(n))
        ans1 = 0
        do i=1,n
            allocate(character(len=len_trim(lines(i)%str)) :: clines(i)%str)
            clines(i)%str = trim(lines(i)%str)
            call clines(i)%decompress()
            print *, clines(i)%len()
            ans1 = ans1 + clines(i)%len()
            print *
        end do
        print '("Answer 9/1 ",i0,l2)', ans1, ans1==110346

        ans2 = 0
        do i=1,n
            associate(tmp => decom_len(clines(i)%str))
                print *, i, tmp
                ans2 = ans2 + tmp
            end associate
        end do
        print '("Answer 9/2 ",i0,l2)', ans2, ans2==10774309173_I8B
    end subroutine



    recursive function decom_len(str) result(n)
        character(len=*), intent(in) :: str
        integer(i8B) :: n

        integer :: i, j, ims, ime, irep, nrep, nsize, res(2)

        i = 1
        n = 0
        do
            ims = scan(str(i:),'(')
            ime = scan(str(i:),')')
            ! "J" is the position of last character before marker
            if (ims==0 .and. ime==0) then
                j = len_trim(str)
            else if (ime-ims > 3 ) then
                j = i-1+(ims-1) 
            else
                error stop 'decomlen - funny position of brackets'
            end if

            ! length of the text before marker
            n = n + int(j-i+1, kind=I8B)

            ! add length of decompressed marked text
            if (ims /= 0) then
                res = decode_marker(str(i-1+ims+1:i-1+ime-1))
                nrep = res(2)
                nsize = res(1)
                n = n + nrep * decom_len(str(i-1+ime+1:i-1+ime+1+nsize-1))
                i = i-1+ime+1+nsize
            else
                exit
            end if
        end do
    end function




    subroutine decompress(this)
        class(cmp_t), intent(inout) :: this

        type(string_t) :: old, new
        integer :: i, j, ims, ime, irep, nrep, nsize, res(2)
        old = string_t(this%str)
        new = string_t('')
        i = 1

        do
            ims = scan(old%str(i:),'(')
            ime = scan(old%str(i:),')')
            if (ims==0 .and. ime==0) then
                j = len_trim(old%str)
            else if (ime-ims > 3 ) then
                j = i-1+(ims-1) ! last character before marker "(axb)"
            else
                error stop 'decompress - funny position of brackets'
            end if

            ! add text before marker
            new = string_t(new%str//old%str(i:j))
            res = 0
            if (ims/=0) res = decode_marker(old%str(i-1+ims+1:i-1+ime-1))
            nrep = res(2)
            nsize = res(1)
            do irep = 1, nrep
                new = string_t(new%str//old%str(i-1+ime+1:i-1+ime+1+nsize-1))
            end do

            if (ims==0) exit
            i = i-1+ime+1+nsize
        end do
        if (allocated(this%str)) deallocate(this%str)
        allocate(character(len=len_trim(new%str)) :: this%str)
        this%str = trim(new%str)
    end subroutine decompress



    function decode_marker(str) result(res)
        character(len=*), intent(in) :: str
        integer :: res(2)

        integer :: i
        i = scan(str,'x')
        if (i==0) error stop 'decode marker - no x found'
        read(str(:i-1), *) res(1)
        read(str(i+1:), *) res(2)
    end function



    integer function cmp_len(this) result(n)
        class(cmp_t), intent(in) :: this
        n = len_trim(this%str)
    end function
end module day1609_mod