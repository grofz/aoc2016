module day1614_mod
    use day1605_mod, only : md5, int2str
    use queue_mod, only : queue_t
    implicit none

    character(len=1), parameter :: NULL_CH = ' '

    type pass_t
        character(len=:), allocatable :: salt
        integer :: gencounter
        type(queue_t) :: keys
        type(queue_t) :: cand_keys(16)
    contains
        procedure :: genhash => pass_genhash
        procedure :: collect_keys => pass_collect_keys
    end type
    interface pass_t
        module procedure pass_new
    end interface pass_t
    
contains
    subroutine day1614(salt)
        character(len=*), intent(in) :: salt

        type(pass_t) :: pass, pass2
        integer :: ans1, ans2
        character(len=32) :: hash

        pass = pass_t(salt)
        call pass%collect_keys(1, ans1)
        print '("Answer 14/1 ",i0,l2)', ans1, ans1==35186

        pass2 = pass_t(salt)
        call pass2%collect_keys(2016, ans2)
        print '("Answer 14/2 ",i0,l2)', ans2, ans2==22429
    end subroutine


    subroutine pass_collect_keys(pass, nstretch, ans)
        class(pass_t), intent(inout) :: pass 
        integer, intent(in) :: nstretch
        integer, intent(out) :: ans

        character(len=32) :: hash
        integer :: i, ind, j
        integer, parameter :: KEYS_NEEDED=64

        ! Note:
        ! Seems working, but safer would be these changes:
        ! When last key added, then assess all non-expired candidates
        ! it is possible some of them were jumped by later keys.
        ! We should keep generating keys until all candidates expire
        ! (but not adding any new candidates)

        ans = 0
        MAINLOOP: do 
            call pass%genhash(nstretch, hash)
            !print *, pass%gencounter, hash

            ! Add all triplets to queue of candidate keys
            associate(ch=>find_repeat(hash, 3))
                if (ch /= NULL_CH) then
                    ind = ch_to_ind(ch)
                    call pass%cand_keys(ind)%enqueue(pass%gencounter)
                end if
            end associate

            ! If pentet is found
            ! - remove all candidates (except the actual pentet)
            ! - move valid candidates to keys queue
            associate(ch=>find_repeat(hash, 5))
                if (ch /= NULL_CH) then
                    ind = ch_to_ind(ch)
                    print '("Pentet found at position ",i0,1x,a)', pass%gencounter, hash
                    do 
                        ! queue is empty or contains only actual key
                        if (pass%cand_keys(ind)%isempty()) exit
                        if (pass%cand_keys(ind)%front()==pass%gencounter) exit
                        call pass%cand_keys(ind)%dequeue(j)
                        if (pass%gencounter - j <= 1000) then
                            ! candidate is a valid key
                            call pass%keys%enqueue(j)
                            print '("Key ",i0," added. We have already ",i0," keys")',&
                            & j, pass%keys%size()
                            if (pass%keys%size()==KEYS_NEEDED) then
                                ans = j
                                exit MAINLOOP
                            end if
                        else
                            ! candidate has expired
                        end if
                    end do
                end if
            end associate

            if (pass%gencounter>50000) exit
            if (pass%keys%size()>=64) exit
        end do MAINLOOP

        ! final seems bugged in gfortran for arrays :-(
        ! manualy dequeue all items from queues
        do i=1,size(pass%cand_keys)
            do
                if (pass%cand_keys(i)%isempty()) exit
                call pass%cand_keys(i)%dequeue(j)
            end do
        end do
        do
            if (pass%keys%isempty()) exit
            call pass%keys%dequeue(j)
        end do
    end subroutine pass_collect_keys



    type(pass_t) function pass_new(salt) result(new)
        integer :: i
        character(len=*), intent(in) :: salt
        new%keys = queue_t(huge(i))
        do i=1,size(new%cand_keys)
            new%cand_keys(i) = queue_t(huge(i))
        end do
        allocate(character(len=len(salt)) :: new%salt)
        new%salt = salt
        new%gencounter = -1
    end function


    integer function ch_to_ind(ch) result(ind)
        character(len=1), intent(in) :: ch
!
! converts "0,1-9,A-F" to "1-16"
!
        select case(ch)
        case('0','1','2','3','4','5','6','7','8','9')
            read(ch,*) ind
            ind = ind + 1 ! ind>=1, ind<=10
        case('A','B','C','D','E','F')
            ind = iachar(ch)-iachar('A')+11
            ! ind>=11, ind<=16
        case default
            error stop 'ch_to_int - invalid character'
        end select
        if (ind<1 .or. ind>16) error stop 'ch_to_int - wrong result'
    end function


    function find_repeat(str, nreps) result(repchar)
        character(len=1) :: repchar
        character(len=*), intent(in) :: str
        integer, intent(in) :: nreps
!
! Return character that repeats "nreps" times in a row or 
! NULL_CH if no such character
!
        integer :: i, irep
        character(len=1) :: last_ch

        repchar=NULL_CH
        last_ch=NULL_CH
        irep = 0
        do i=1, len(str)
            if (last_ch==str(i:i)) then
                irep = irep+1
            else
                irep = 1
                last_ch=str(i:i)
            end if
            if (irep==nreps) exit
        end do
        if (irep==nreps) repchar = last_ch
    end function find_repeat



    subroutine pass_genhash(this, nstretch, hash)
        class(pass_t), intent(inout) :: this
        integer, intent(in) :: nstretch
        character(len=*) :: hash

        integer :: i, j

        this%gencounter = this%gencounter + 1
        hash = md5(this%salt//int2str(this%gencounter))

        if (nstretch == 1) return
        do i=1,nstretch
            ! convert to lower case letters before re-hashing
            do j=1,len(hash)
                associate(ch=>hash(j:j))
                if (iachar(ch)>=iachar('A')) then
                    hash(j:j) = char(iachar(ch) + iachar('a')-iachar('A'))
                end if
                end associate
            end do
            hash = md5(hash)
        end do

    end subroutine

end module day1614_mod