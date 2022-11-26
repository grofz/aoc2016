module day1615_mod
    use parse_mod, only : string_t, read_strings
    implicit none

    type disc_t
        integer :: id     ! ball arrives at time "t_drop + id"
        integer :: n      ! position [0; n-1], hole at [0] 
        integer :: istart ! position at time = 0
    contains
        procedure :: position => disc_position
        procedure :: next_time
    end type
    interface disc_t
        module procedure disc_new
    end interface

contains

    subroutine day1615(file)
        character(len=*), intent(in) :: file

        type(string_t), allocatable :: lines(:)
        type(disc_t), allocatable :: discs(:), discs2(:)
        integer :: i, j, ndiscs, ans1, ans2

        ! read input, add an additional wheel for Part 2
        lines = read_strings(file)
        ndiscs = size(lines)
        allocate(discs(ndiscs))
        allocate(discs2(ndiscs+1))
        do i=1, ndiscs
            discs(i) = disc_t(lines(i)%str)
        end do
        discs2(1:ndiscs) = discs
        discs2(ndiscs+1) = disc_t(id=ndiscs+1, n=11, istart=0)

        print '(a)', 'Searching alignment for Part 1...'
        ans1 = search_alignment(discs)

        print '(a)', 'Searching alignment for Part 2...'
        ans2 = search_alignment(discs2)

        print '("Answer 15/1 ",i0,l2)', ans1, ans1==16824
        print '("Answer 15/2 ",i0,l2)', ans2, ans2==3543984
    end subroutine day1615


    integer function search_alignment(discs0) result(t)
        type(disc_t), intent(in) :: discs0(:)
!
! Task is simplified because all discs vary in size and all their sizes are primes
! I am not sure if the algorithm would work otherwise...
!
        type(disc_t), allocatable :: discs(:)
        integer :: ndiscs, i, aligned, dt

        ndiscs = size(discs0)
        allocate(discs(ndiscs))
        discs = discs0
        call sort_discs(discs)

        t = discs(ndiscs)%next_time(0)
        do
            ! display progress
            write(*,'(i9)',advance='no') t
            do i=1, ndiscs
              write(*,'(i2,1x)',advance='no') discs(i)%position(t+discs(i)%id)
            end do
            write(*,*)

            ! count aligned discs at the current time
            aligned = 0
            do i = ndiscs, 1, -1
                if (discs(i)%position(t+discs(i)%id)==0) then
                    aligned = aligned + 1
                else
                    exit
                end if
            end do
            if (aligned==ndiscs) exit

            ! try again at later time (t + dt)
            dt = 1
            do i = ndiscs, ndiscs-aligned+1, -1
                dt = dt * discs(i)%n
            end do
            t = t + dt
        end do
    end function search_alignment


    type(disc_t) function disc_new(str) result(new)
        character(len=*), intent(in) :: str

        integer :: i0, i1, i2, j2

        i0 = index(str,'Disc #')
        if (i0 /= 0) then
            i0 = i0 + len('Disc #')
            read(str(i0:),*) new%id 
        else
            error stop 'disc_new - error getting Disc#'
        end if

        i1 = index(str,'has ')
        if (i1 /= 0) then
            i1 = i1 + len('has ')
            read(str(i1:),*) new%n
        else
            error stop 'disc_new - error getting has positions'
        end if

        i2 = index(str,'it is at position ')
        j2 = scan(str(i2:),'.')
        if (i2 /= 0) then
            j2 = j2 + i2-1
            i2 = i2 + len('it is at position ')
            read(str(i2:j2-1),*) new%istart
        else
            error stop 'disc_new - error getting initial position'
        end if
    end function disc_new


    subroutine sort_discs(arr)
        type(disc_t), intent(inout) :: arr(:)
!
! Sort discs according the first time their slot is aligned
!
        type(disc_t) :: wrk
        integer :: i
        logical :: was_swap

        do
            was_swap = .false.
            do i=1,size(arr)-1
                !if (arr(i)%n > arr(i+1)%n) then
                if (arr(i)%next_time(0) > arr(i+1)%next_time(0)) then
                    wrk = arr(i)
                    arr(i) = arr(i+1)
                    arr(i+1) = wrk
                    was_swap = .true.
                end if
            end do
            if (.not. was_swap) exit
        end do
    end subroutine sort_discs


    function next_time(disc, t0) result (t)
        class(disc_t), intent(in) :: disc
        integer, intent(in) :: t0
        integer :: t, pos
!
! Next time from time "t0" a slot on the disc will be alligned
!
        ! position at time "t0"
        pos = disc%position(t0) 

        ! advance time by the distance to the slot "0"
        t = t0 + disc%n - pos

        ! shift time back due to disc position in the machine
        t = t - disc%id

        ! good postion repeats after "n" seconds
        ! make sure time returned is larger than the current time
        if (t<=t0) t = t + disc%n
    end function


    integer function disc_position(this, time) result(position)
        class(disc_t), intent(in) :: this
        integer, intent(in) :: time
!
! What is the slot position of the disc at time "t"
!
        position = mod(time, this%n)
        position = mod(position+this%istart, this%n)
    end function

end module day1615_mod