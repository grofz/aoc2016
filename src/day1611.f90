module day1611_mod
    implicit none

    type state_t
        integer, allocatable :: ip(:) ! position of item: odd chip, even generator
        integer :: ie ! position of elevator
        integer :: t  ! step (time)
        logical :: visited = .false.
        integer :: id=1, previd=0
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
        character(len=*), intent(in) :: problem(2)
        type(state_t) :: init_state(2), final_state(2)
        real :: t0, t1

        call cpu_time(t0)
        init_state(1) = state_t(problem(1))
        init_state(2) = state_t(problem(2))
        call find_process(init_state(1), final_state(1))
        print *
        call find_process(init_state(2), final_state(2))
        print *
        associate(ans1=>final_state(1)%t, ans2=>final_state(2)%t)
          print '("Answer 11/1 ",i0,l2)', ans1, ans1==31
          print '("Answer 11/2 ",i0,l2)', ans2, ans2==55
        end associate
        call cpu_time(t1)
        print '("Time taken ",g0)', t1-t0
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
        current%id = 1
        MAINLOOP: do
            ! every item has been moved to the top level
            if (all(current%ip==MAXLEVEL) .and. current%ie==MAXLEVEL) exit MAINLOOP

            ! update state by moving one/two items up/down
            do i=1, size(current%ip)
            do j=i, size(current%ip) ! j >= i
                ! can not move item not on the current floor
                if (current%ip(i)/=current%ie .or. current%ip(j)/=current%ie) cycle
                do dir= 1,-1,-2
                    ! can not move outside 1-4 floor range
                    if (current%ie+dir < 1 .or. current%ie+dir > MAXLEVEL) cycle

                    ! Heuristic rules (to speed-up)
                    ! 1. do not move the same chip and generator down
                    if (dir==-1 .and. mod(j,2)==0 .and. j-i==1) then
                        !print *, 'aaa'
                        cycle
                    end if

                    ! 2. do not move two chips down
                    if (dir==-1 .and. mod(i,2)/=0 .and. mod(j,2)/=0 .and. i/=j) cycle

                    ! 3. do not move two generators down
                    if (i/=j .and. dir==-1 .and. mod(i,2)==0 .and. mod(j,2)==0) cycle

                    ! try to move items
                    new = current
                    new%ie = current%ie + dir
                    new%ip(i) = current%ip(i) + dir
                    if (j/=i) new%ip(j) = current%ip(j) + dir

                    ! skip not allowed configurations
                    if (.not. new%allowed()) cycle

                    ! add new configuration at the end of an array
                    new%t = current%t + 1
                    new%previd = current%id
                    call add_state(arr, nstates, new, was_added)
                end do
            end do
            end do

            ! find new unvisited state/mark current as visited
            mintime = huge(mintime)
            idmin = 0
            do i=1, nstates
                if (arr(i)%visited) cycle
                if (current==arr(i)) then
                    arr(i)%visited=.true.
                    cycle
                end if
                if (arr(i)%t < mintime) then
                    mintime = arr(i)%t
                    idmin = i
                endif
            end do
            if (idmin==0) error stop 'can not find next move'
            current = arr(idmin)
            current%id = idmin

            ! print progress of the search
            if (current%t > last) then
                last = current%t
                call current%print()
                print *, 'number of states ',nstates
            end if

        end do MAINLOOP
        final_state = current

        ! Print the path
        print '("Solution found:")'
        i = final_state%id
        do
            call arr(i)%print()
            i = arr(i)%previd
            if (i==0) exit
        end do
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

        ! Search if new is present
        was_added = .true.
        do i=n,1,-1
            if (arr(i)==new) then
                was_added = .false.
                exit
            end if
        end do
        if (was_added) then
            n = n + 1
            arr(n) = new
        end if
    end subroutine


    logical function state_allowed(this) result(allowed)
        class(state_t), intent(in) :: this

        integer :: ilev, n, ngen, natoms, iatom

        natoms = size(this%ip)
        if (mod(natoms,2)/=0) &
        &   error stop 'state allowed - ip array size must be even'
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


    ! not needed
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
        
        integer :: ilev, j
        integer :: agen(MAXLEVEL), bgen(MAXLEVEL), ach(MAXLEVEL), bch(MAXLEVEL)
        integer :: aboth(MAXLEVEL), bboth(MAXLEVEL)

        if (size(a%ip) /= size(b%ip)) error stop 'state_eq_state - different operand ip size'
        ach = 0; bch = 0
        agen = 0; bgen = 0
        aboth = 0; bboth = 0
        do ilev=1,MAXLEVEL
            do j=1,size(a%ip)-1,2
                if (a%ip(j)==ilev .and. a%ip(j+1)/=ilev) then
                    ach(ilev) = ach(ilev)+1
                else if (a%ip(j)/=ilev .and. a%ip(j+1)==ilev) then
                    agen(ilev) = agen(ilev)+1
                else if (a%ip(j)==ilev .and. a%ip(j+1)==ilev) then
                    aboth(ilev) = aboth(ilev)+1
                end if
                if (b%ip(j)==ilev .and. b%ip(j+1)/=ilev) then
                    bch(ilev) = bch(ilev)+1
                else if (b%ip(j)/=ilev .and. b%ip(j+1)==ilev) then
                    bgen(ilev) = bgen(ilev)+1
                else if (b%ip(j)==ilev .and. b%ip(j+1)==ilev) then
                    bboth(ilev) = bboth(ilev)+1
                end if
            end do
        end do
        eq = all(ach==bch) .and. all(agen==bgen) .and. all(aboth==bboth) &
        &    .and. a%ie == b%ie
    end function


    subroutine state_print(this)
        class(state_t), intent(in) :: this
        print '(i3,"  EL-",i1,1x,*("["i1,"]{",i1,"}",:,2x))', &
        &   this%t, this%ie, this%ip
    end subroutine
end module day1611_mod
