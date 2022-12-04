program main
  use day1601_mod, only: day1601
  use day1602_mod, only: day1602
  use day1603_mod, only: day1603
  use day1604_mod, only: day1604
  use day1605_mod, only: day1605
  use day1606_mod, only: day1606
  use day1607_mod, only: day1607
  use day1608_mod, only: day1608
  use day1609_mod, only: day1609
  use day1610_mod, only: day1610
  use day1611_mod, only: day1611
  use day1612_mod, only: day1612
  use day1613_mod, only: day1613
  use day1614_mod, only: day1614
  use day1615_mod, only: day1615
  use day1616_mod, only: day1616
  use day1617_mod, only: day1617
  use day1618_mod, only: day1618
  use day1619_mod, only: day1619
  use day1620_mod, only: day1620
  use day1622_mod, only: day1622
  use day1623_mod, only: day1623
  implicit none

  goto 23
  01 call day1601('./inp/01/input.txt')
  02 call day1602('./inp/02/input.txt')
  03 call day1603('./inp/03/input.txt')
  04 call day1604('./inp/04/input.txt')
  05 call day1605('reyedfim') ! test case: 'abc'
  06 call day1606('./inp/06/input.txt')
  07 call day1607('./inp/07/input.txt')
  08 call day1608('./inp/08/input.txt')
  09 call day1609('./inp/09/input.txt')
  10 call day1610('./inp/10/input.txt')
  11 call day1611(['real','big '])    ! big|real|test
  12 call day1612('inp/12/input.txt') 
  13 call day1613([1364,31,39])       ! test case: [10,7,4] 
  14 call day1614('jlmsuwbz') ! test case: "abc"
  15 call day1615('inp/15/input.txt') ! test.txt
  16 call day1616('10111011111001111', 272, 35651584) 
  17 call day1617('dmypynyp') ! 'ihgpwlah', 'kglvqrro', 'ulqzkmiv'
  18 call day1618('inp/18/input.txt') ! test.txt
  19 call day1619(3017957)
  20 call day1620('inp/20/input.txt')
  22 call day1622('inp/22/input.txt') ! test.txt
  23 call day1623('inp/23/input.txt') 

end program main
