  module day1621_mod
    use parse_mod, only : string_t, read_strings, split
    implicit none

  contains

    subroutine day1621(file)
      character(len=*), intent(in) :: file

      type(string_t), allocatable :: lines(:)
      character(len=8) :: key, key2
      integer :: i

      ! Part 1
      lines = read_strings(file)
      key = scramble('abcdefgh', lines)
      print '("Answer 21/1 ",a,l2)', key, key=='gfdhebac'

      ! Part 2
      key = unscramble('fbgdceah', lines)
      print '("Answer 21/2 ",a,l2)', key, key=='gfdhebac'
      print *, 'independent check: ', scramble(key, lines)=='fbgdceah'
    end subroutine day1621


    function scramble(key,instr_list) result(res)
      character(len=*), intent(in) :: key
      type(string_t), intent(in) :: instr_list(:)
      character(len=len(key)) :: res

      integer :: i

      res = key
      do i=1,size(instr_list)
        res = mangle(res, instr_list(i)%str, .false.)
      end do
    end function scramble


    function unscramble(key,instr_list) result(res)
      character(len=*), intent(in) :: key
      type(string_t), intent(in) :: instr_list(:)
      character(len=len(key)) :: res

      integer :: i

      res = key
      do i=size(instr_list),1,-1
        res = mangle(res, instr_list(i)%str, .true.)
      end do
    end function unscramble


! ===============================
! Library of scrambling functions
! ===============================

    function mangle(str, instruction, isreverse) result(res)
      character(len=*), intent(in) :: str, instruction
      character(len=len(str)) :: res
      logical, intent(in) :: isreverse

      type(string_t), allocatable :: word(:)
      integer :: n, p1, p2

      call split(instruction, ' ', word)

      if (word(1)%str=='swap' .and. word(2)%str=='position') then
        ! swap position X with position Y (reversible)
        read(word(3)%str,*) p1
        read(word(6)%str,*) p2
        res = swap_position(str, p1, p2)

      else if (word(1)%str=='swap' .and. word(2)%str=='letter') then
        ! swap letter X with letter Y (reversible)
        res = swap_letter(str, word(3)%str(1:1), word(6)%str(1:1))

      else if (word(1)%str=='rotate' .and. word(2)%str/='based') then
        ! rotate left/right X steps (reversible by changing rotation)
        read(word(3)%str, *) n
        res = rotate(str, word(2)%str, n, isreverse)

      else if (word(1)%str=='rotate' .and. word(2)%str=='based') then
        ! rotate based on postion of letter X (difficult reversibility)
        res = rotate_based(str, word(7)%str(1:1), isreverse)

      else if (word(1)%str=='reverse') then
        ! reverse positions X through Y (reversible)
        read(word(3)%str,*) p1
        read(word(5)%str,*) p2
        res = reverse_positions(str, p1, p2)

      else if (word(1)%str=='move') then
        ! move position X to position Y (reversible by swapping operands)
        read(word(3)%str,*) p1
        read(word(6)%str,*) p2
        if (isreverse) then
          res = move_position(str, p2, p1)
        else
          res = move_position(str, p1, p2)
        end if
        
      else
        error stop 'mangle - instruction unrecognized'
      end if
    end function mangle


    function swap_position(str,p1,p2) result(res)
      character(len=*), intent(in) :: str
      integer, intent(in) :: p1, p2
      character(len=len(str)) :: res
!
! swap letters at positions p1, p2
!
      call assert_positions(str, p1, p2)
      res = str
      res(p1+1:p1+1) = str(p2+1:p2+1)
      res(p2+1:p2+1) = str(p1+1:p1+1)
    end function swap_position


    function swap_letter(str,ch1,ch2) result(res)
      character(len=*), intent(in) :: str
      character(len=1), intent(in) :: ch1, ch2
      character(len=len(str)) :: res
!
! swap letters at positions p1, p2
!
      integer :: p1, p2
      
      p1 = scan(str,ch1)-1
      p2 = scan(str,ch2)-1
      if (p1==p2) error stop 'swap_letter - same postions'
      res = swap_position(str,p1,p2)
    end function swap_letter


    function rotate(str,dir,n,isreverse) result(res)
      character(len=*), intent(in) :: str
      character(len=*), intent(in) :: dir
      integer, intent(in) :: n
      logical, intent(in), optional :: isreverse
      character(len=len(str)) :: res
!
! rotate left/right "n" steps
!
      integer :: i, dir0
      logical :: isreverse0
      character(len=len(str)) :: wrk

      isreverse0 = .false.
      if (present(isreverse)) isreverse0 = isreverse
      select case(dir)
      case('left')
        dir0 = -1
      case('right')
        dir0 = +1
      case default
        error stop 'rotate - wrong keyword direction'
      end select
      if (isreverse0) dir0 = dir0 * (-1)

      res = str
      do i=1,n
        wrk = res
        select case(dir0)
        case(-1)
          res(:len(str)-1) = wrk(2:)
          res(len(str):len(str)) = wrk(1:1)
        case(+1)
          res(2:) = wrk(1:len(str)-1)
          res(1:1) = wrk(len(str):len(str))
        case default
          error stop 'rotate - wrong keyword direction'
        end select
      end do
    end function rotate


    function rotate_based(str,ch,isreverse) result(res)
      character(len=*), intent(in) :: str
      character(len=1), intent(in) :: ch
      logical, intent(in), optional :: isreverse
      character(len=len(str)) :: res
!
! Rotate based on position of letter 
!
      integer :: p, n
      logical :: isreverse0

      isreverse0 = .false.
      if (present(isreverse)) isreverse0 = isreverse

      if (.not. isreverse0) then
        p = scan(str, ch)-1
        n = 1+p
        if (p >= 4) n = n + 1
        res = rotate(str,'right',n)
      else
        ! reverse works for 8-length strings at the moment
        ! heuristics from experiments
        p = scan(str, ch)
        select case(p)
        case(2)
          n = 1
        case(4)
          n = 2
        case(6)
          n = 3
        case(8)
          n = 4
        case(3)
          n = 6
        case(5)
          n = 7
        case(7)
          n = 0
        case(1)
          n = 1
        case default
          error stop 'reverse failed'
        end select
        res = rotate(str,'left',n)
      end if
    end function rotate_based


    function reverse_positions(str,p1,p2) result(res)
      character(len=*), intent(in) :: str
      integer, intent(in) :: p1, p2
      character(len=len(str)) :: res
!
! Reverse positions P1 through P2
!
      integer :: i, p10, p20

      call assert_positions(str, p1, p2)
      p10 = p1 + 1
      p20 = p2 + 1 ! "p1" / "p2" are now 1-index based
      res = str
      do i=p10, p20
        res(i:i) = str(p20-i+p10:p20-i+p10)
      end do
    end function reverse_positions


    function move_position(str,p1,p2) result(res)
      character(len=*), intent(in) :: str
      integer, intent(in) :: p1, p2
      character(len=len(str)) :: res
!
! Move postion P1 to position P2
!
      character(len=1) :: ch
      integer :: p10, p20

      call assert_positions(str, p1, p2)
      p10 = p1 + 1
      p20 = p2 + 1 ! "p1" / "p2" are now 1-index based
      ch = str(p10:p10)
      res(:p10-1) = str(:p10-1)
      res(p10:len(str)-1) = str(p10+1:)
      res(p20+1:) = res(p20:len(str)-1)
      res(p20:p20) = ch
    end function move_position


    subroutine assert_positions(str,p1,p2)
      character(len=*), intent(in) :: str
      integer, intent(in) :: p1,p2
      logical :: iserror
      iserror = .false.
      if (p1<1-1 .or. p2<1-1 .or. p1>len_trim(str)-1 .or. p2>len_trim(str)-1) &
        iserror = .true.
      if (iserror) error stop 'verify_positions - postion out of bounds'
    end subroutine

  end module day1621_mod
