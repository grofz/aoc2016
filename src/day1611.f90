module day1611_mod
    implicit none

    type state_t
        integer, allocatable :: ip(:) ! position of item: odd chip, even generator
        integer :: ie ! position of elevator
        integer :: t  ! step (time)
        logical :: visited = .false.
    contains
        procedure :: allowed => state_allowed, print => state_print
        procedure, private :: state_gt_state, state_eq_state
        generic :: operator(>) => state_gt_state
        generic :: operator(==) => state_eq_state
    end type
    interface state_t
        module procedure state_new
    end interface

    integer, parameter :: MAXLEVEL = 4

contains
    
    subroutine day1611(problem)
        character(len=*), intent(in) :: problem
        type(state_t) :: init_state, final_state

        init_state = state_t(problem)
        print *, init_state % allowed()
        call init_state%print()
        print *
        call find_process(init_state, final_state)
        print *, 'final ',final_state%t
    end subroutine


    subroutine find_process(init_state, final_state)
        type(state_t), intent(in) :: init_state
        type(state_t), intent(out) :: final_state

        type(state_t) :: current 
        type(state_t) :: new
        type(state_t), allocatable :: arr(:)
        integer :: i, j, dir, nstates, mintime, idmin, last, z
        logical :: was_added, symmetry_found

        last = 0
        nstates = 0
        call add_state(arr, nstates, init_state, was_added)
        current = arr(1)
        MAINLOOP: do
            if (all(current%ip==MAXLEVEL) .and. current%ie==MAXLEVEL) exit MAINLOOP

            ! update state by moving one/two items up-down
            do i=1, size(current%ip)
            do j=i, size(current%ip) ! j >= i
                ! can not move item not on the current floor
                if (current%ip(i)/=current%ie .or. current%ip(j)/=current%ie) cycle
                do dir= 1,-1,-2
                    if (current%ie+dir < 1 .or. current%ie+dir > MAXLEVEL) cycle

                    ! do not move same chip and generator down from 4th floor
                    if (dir==-1 .and. mod(j,2)==0 .and. j-i==1) then
                        !print *, 'aaa'
                        if (current%ie==MAXLEVEL) cycle
                        cycle
                    end if

                    ! do not move up same chip and generator, if
                    ! another chip and generator are on the same floor
                    if (mod(j,2)==0 .and. j-i==1) then
                        symmetry_found = .false.
                        do z=i-2,1,-2
                            if (current%ip(z)==current%ie .and. &
                                current%ip(z+1)==current%ie) symmetry_found=.true.
                        end do
                        if (symmetry_found) then
                           !print *, 'AAA',i,j
                           !call current%print()
                            cycle
                        end if
                    end if

                    ! do not move chip if same state chip is on the same
                    ! floor
                    if (i==j .and. mod(i,2)/=0) then
                        symmetry_found = .false.
                        do z=i-2,1,-2
                            if (current%ip(z)==current%ie .and. &
                                 current%ip(z+1)==current%ip(i+1)) symmetry_found = .true.
                        end do
                        if (symmetry_found) cycle
                    end if

                    ! do not move generator if same state generator on same floor
                    if (i==j .and. mod(i,2)==0) then
                        symmetry_found = .false.
                        do z=i-2,2,-2
                            if (current%ip(z)==current%ie .and. &
                                 current%ip(z-1)==current%ip(i-1)) symmetry_found = .true.
                        end do
                        if (symmetry_found) cycle
                    end if

                    ! do not move two chips down
                    if (dir==-1 .and. mod(i,2)/=0 .and. mod(j,2)/=0 .and. i/=j) cycle

                    ! do not move generators down
                    !if (dir==-1 .and. mod(i,2)==0 .and. mod(j,2)==0) cycle
                    if (i/=j .and. dir==-1 .and. mod(i,2)==0 .and. mod(j,2)==0) cycle


                    new = current
                    new%ie = current%ie + dir
                    new%ip(i) = current%ip(i) + dir
                    if (j/=i) new%ip(j) = current%ip(j) + dir
                    ! skip not allowed configurations
                    if (.not. new%allowed()) cycle
                    new%t = current%t + 1
                    call add_state(arr, nstates, new, was_added)
                end do
            end do
            end do

            ! find new unvisited state/ mark current as visited
            mintime = huge(mintime)
            idmin = 0
            do i=1, nstates
                if (current==arr(i)) arr(i)%visited=.true.
                if (arr(i)%visited) cycle
                if (arr(i)%t < mintime) then
                    mintime = arr(i)%t
                    idmin = i
                endif
            end do
            if (idmin==0) error stop 'can not find next move'
            current = arr(idmin)

            j = 0
            do i=1, nstates
                if (arr(i)%visited) cycle
                j=j+1
                !call arr(i)%print()
            end do
           !print *,'lrft =',j 
           !call current%print()
            if (current%t > last) then
                last = current%t
                call current%print()
                print *, 'size ',nstates
            end if

        end do MAINLOOP
        final_state = current
    end subroutine


    type(state_t) function state_new(problem) result(new)
        character(len=*), intent(in) :: problem

        new % t = 0
        new % ie = 1
        select case(problem)
        case('test') ! H, Li
            allocate(new%ip(4))
            new%ip(1) = 1 !H chip
            new%ip(2) = 2
            new%ip(3) = 1 !Li chip
            new%ip(4) = 3
        case('real') ! Th, Pu, Sr, Pm, Ru
            allocate(new%ip(10))
            new%ip( 1) = 1 ! Th
            new%ip( 2) = 1
            new%ip( 3) = 2 ! Pu
            new%ip( 4) = 1
            new%ip( 5) = 2 ! Sr
            new%ip( 6) = 1
            new%ip( 7) = 3 ! Pm
            new%ip( 8) = 3 
            new%ip( 9) = 3! Ru
            new%ip(10) = 3
        case('big') ! Th, Pu, Sr, Pm, Ru + El Di
            allocate(new%ip(14))
            new%ip( 1) = 1 ! Th
            new%ip( 2) = 1
            new%ip( 3) = 2 ! Pu
            new%ip( 4) = 1
            new%ip( 5) = 2 ! Sr
            new%ip( 6) = 1
            new%ip( 7) = 3 ! Pm
            new%ip( 8) = 3 
            new%ip( 9) = 3! Ru
            new%ip(10) = 3
            new%ip(11) = 1! El
            new%ip(12) = 1
            new%ip(13) = 1! El
            new%ip(14) = 1
        case default
            error stop 'state_new - invalid problem'
        end select
    end function


    subroutine add_state(arr, n, new, was_added)
        type(state_t), intent(inout), allocatable :: arr(:)
        integer, intent(inout) :: n
        type(state_t), intent(in) :: new
        logical, intent(out) :: was_added

        integer :: i, nmax
        type(state_t), allocatable :: wrk(:)

        ! Add state "new" to the array of states (if not already there)
        ! Keep states in the array sorted

        ! Increase array size (if needed)
        if (.not. allocated(arr)) then
            allocate(arr(10))
        end if
        nmax = size(arr)
        if (n==nmax) then
            allocate(wrk(2*nmax))
            wrk(1:n) = arr
            call move_alloc(wrk, arr)
        end if

        ! Todo - I should implement binary search here...
        was_added = .true.
        do i=1,n
            if (arr(i)==new) then
                was_added = .false.
                exit
            else if (new > arr(i)) then
                cycle
            end if
            ! inserting new at position "i"
            arr(i+1:n+1) = arr(i:n)
            arr(i) = new
            exit
        end do
        if (i==n+1) arr(n+1) = new ! adding new at the end
        if (was_added) n = n + 1
    end subroutine


    logical function state_allowed(this) result(allowed)
        class(state_t), intent(in) :: this

        integer :: ilev, n, ngen, natoms, iatom

        natoms = size(this%ip)
        if (mod(natoms,2)/=0) error stop 'state allowed - ip array size must be even'
        natoms = natoms/2
        allowed = .true.

        ! If there is a unshielded chip (without generator),
        ! no other generator may be present at the same level.
        ! Chip - odd, Generator - even positions in "ip" array
        LEV: do ilev = 1, MAXLEVEL
            ! number of generators at the current level
            ngen = count(this%ip(2:2*natoms:2)==ilev)
            do iatom=1, natoms
                ! micro-chip not at the same level as the generator
                ! and other generators present at the current level
                if (this%ip(2*iatom-1)==ilev .and. &
                    this%ip(2*iatom-1)/=this%ip(2*iatom) .and. ngen /= 0) then
                    allowed = .false.
                    exit LEV
                end if
            end do
        end do LEV
    end function


    logical function state_gt_state(a, b) result (gt)
        class(state_t), intent(in) :: a, b
        
        integer :: i
        if (size(a%ip) /= size(b%ip)) error stop 'state_gt_state - different operand ip size'
        gt = .false.
        do i=1, size(a%ip)
            if (a%ip(i)> b%ip(i)) then
                gt = .true.
                exit
            elseif (a%ip(i)< b%ip(i)) then
                exit
            end if
        end do
    end function


    logical function state_eq_state(a, b) result(eq)
        class(state_t), intent(in) :: a, b
        
        integer :: i
        if (size(a%ip) /= size(b%ip)) error stop 'state_eq_state - different operand ip size'
        eq = all(a%ip==b%ip) .and. a%ie==b%ie
    end function


    subroutine state_print(this)
        class(state_t), intent(in) :: this
        print '(i0,"  EL-",i1,1x,*("["i1,"]{",i1,"}",:,2x))', &
        &   this%t, this%ie, this%ip
    end subroutine
end module day1611_mod