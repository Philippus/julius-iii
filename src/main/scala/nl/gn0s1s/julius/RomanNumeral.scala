package nl.gn0s1s.julius

import scala.annotation.tailrec
import scala.collection.immutable.ListMap

enum RomanNumeral extends Ordered[RomanNumeral] with ExtendedList:

  import RomanDigit._

  case Nulla extends RomanNumeral

  case RomanDigits(l: List[RomanDigit]) extends RomanNumeral

  override def toString = this match
    case Nulla => "nulla"
    case RomanDigits(l) =>
      val substitutes = ListMap[List[RomanDigit], List[RomanDigit]](
        List(D, C, C, C, C) -> List(C, M),
        List(C, C, C, C) -> List(C, D),
        List(L, X, X, X, X) -> List(X, C),
        List(X, X, X, X) -> List(X, L),
        List(V, I, I, I, I) -> List(I, X),
        List(I, I, I, I) -> List(I, V))
      l.substitute(substitutes).mkString

  def toInt: Int = this match
    case RomanNumeral.Nulla => 0
    case RomanNumeral.RomanDigits(l) => l.map(_.toInt).sum
  
  def optimize: RomanNumeral = this match
    case Nulla => Nulla
    case RomanDigits(l) =>
      val substitutes = ListMap[List[RomanDigit], List[RomanDigit]](
        List(I, I, I, I, I) -> List(V),
        List(V, V) -> List(X),
        List(X, X, X, X, X) -> List(L),
        List(L, L) -> List(C),
        List(C, C, C, C, C) -> List(D),
        List(D, D) -> List(M))
      RomanDigits(l.substitute(substitutes))

  def compare(that: RomanNumeral): Int =
    if this == that then 0
    else if this < that then -1
    else 1

  override def <(that: RomanNumeral): Boolean = (this, that) match
    case (Nulla, _) => that != Nulla
    case (_, Nulla) => false
    case (RomanDigits(l), RomanDigits(r)) =>
      @tailrec def lessThanHelper(digits: List[RomanDigit], l: List[RomanDigit], r: List[RomanDigit], compare: Int): Boolean =
        if compare != 0 then compare < 0
        else digits match
          case Nil => compare < 0
          case hd :: tl =>
            lessThanHelper(tl, l, r, l.count(_ == hd).compare(r.count(_ == hd)))

      lessThanHelper(List(RomanDigit.M, RomanDigit.D, RomanDigit.C, RomanDigit.L, RomanDigit.X, RomanDigit.V, RomanDigit.I), l, r, 0)

  def plus(that: RomanNumeral): RomanNumeral = (this, that) match
    case (Nulla, _) => that
    case (_, Nulla) => this
    case (RomanDigits(l), RomanDigits(r)) => RomanNumeral(l ++ r)

  def minus(that: RomanNumeral): RomanNumeral = (this, that) match
    case (Nulla, _) => Nulla
    case (_, Nulla) => this
    case (RomanDigits(_), RomanDigits(_)) if this <= that => Nulla
    case (RomanDigits(l), RomanDigits(r)) =>
      @tailrec def minusHelper(r: List[RomanDigit], s: List[RomanDigit]): RomanNumeral =
        val substitutes = ListMap[RomanDigit, List[RomanDigit]](
          V -> List(I, I, I, I, I),
          X -> List(V, V),
          L -> List(X, X, X, X, X),
          C -> List(L, L),
          D -> List(C, C, C, C, C),
          M -> List(D, D))

        def findAndExpand(r: List[RomanDigit], largestRemaining: RomanDigit): List[RomanDigit] =
          val (largerDigits, smallerOrEqualDigits) = r.partition(_ > largestRemaining)
          val smallestLargerDigit = largerDigits.last
          largerDigits.init ::: substitutes.getOrElse(smallestLargerDigit, List(smallestLargerDigit)) ::: smallerOrEqualDigits

        s match
          case Nil =>
            RomanNumeral(r)
          case hd :: tl =>
            val rExpanded = findAndExpand(r, hd)
            minusHelper(rExpanded.diff(s), s.diff(rExpanded))

      minusHelper(l.diff(r), r.diff(l))

  def times(that: RomanNumeral): RomanNumeral = (this, that) match
    case (Nulla, _) => Nulla
    case (_, Nulla) => Nulla
    case (RomanDigits(_), RomanDigits(_)) =>
      @tailrec def timesHelper(r: RomanNumeral, s: RomanNumeral, acc: List[(RomanNumeral, RomanNumeral)]): List[(RomanNumeral, RomanNumeral)] =
        if r == RomanNumeral(I) then
          acc ::: List((r, s))
        else
          timesHelper(r.halve, s.double, acc ::: List((r, s)))
      timesHelper(this, that, acc = List())
        .filter { (halve, _) => halve.isOdd }
        .map { (_, double) => double }
        .reduce(_.plus(_))

  def halve: RomanNumeral = this match
    case Nulla => Nulla
    case RomanDigits(l) =>
      @tailrec def halveHelper(l: List[RomanDigit], acc: List[RomanDigit]): RomanNumeral = l match
        case M :: tl => halveHelper(tl, D :: acc)
        case D :: tl => halveHelper(C :: tl, C :: C :: acc)
        case C :: tl => halveHelper(tl, L :: acc)
        case L :: tl => halveHelper(X :: tl, X :: X :: acc)
        case X :: tl => halveHelper(tl, V :: acc)
        case V :: tl => halveHelper(I :: tl, I :: I :: acc)
        case I :: I :: tl => halveHelper(tl, I :: acc)
        case I :: tl => halveHelper(tl, acc)
        case Nil => RomanNumeral(acc)
      halveHelper(l, acc = List())

  def double: RomanNumeral = this.plus(this)

  def isOdd: Boolean = this match
    case Nulla => false
    case RomanDigits(l) =>
      @tailrec def isOddHelper(l: List[RomanDigit], isOdd: Boolean): Boolean = l match
        case M :: tl => isOddHelper(tl, isOdd)
        case D :: tl => isOddHelper(tl, isOdd)
        case C :: tl => isOddHelper(tl, isOdd)
        case L :: tl => isOddHelper(tl, isOdd)
        case X :: tl => isOddHelper(tl, isOdd)
        case V :: tl => isOddHelper(tl, !isOdd)
        case I :: tl => isOddHelper(tl, !isOdd)
        case Nil => isOdd
      isOddHelper(l, isOdd = false)

  def div(that: RomanNumeral): RomanNumeral = (this, that) match
    case (_, Nulla) => throw new ArithmeticException("/ by nulla")
    case (Nulla, _) => Nulla
    case (RomanDigits(_), RomanDigits(_)) =>
      val digits = List(M, D, C, L, X, V, I)
      val multiplicationTable = digits.zip(digits.map(x => RomanNumeral(x).times(that)))

      @tailrec def divHelper(multiplicationTable: List[(RomanDigit, RomanNumeral)], acc: List[RomanDigit], remainder: RomanNumeral): RomanNumeral =
        multiplicationTable match
          case Nil =>
            RomanNumeral(acc)
          case hd :: tl =>
            val (digit, multiple) = hd
            if multiple <= remainder then
              divHelper(multiplicationTable, acc ++ List(digit), remainder.minus(multiple))
            else
              divHelper(multiplicationTable.tail, acc, remainder)
      divHelper(multiplicationTable, acc = List(), remainder = this)
end RomanNumeral

object RomanNumeral extends ExtendedList:
  import RomanDigit.{ M, D, C, L, X, V, I }
  
  def apply(): RomanNumeral = RomanNumeral(List())
  def apply(r: RomanDigit): RomanNumeral = RomanNumeral(List(r))
  def apply(r: RomanDigit*): RomanNumeral = RomanNumeral(r.toList)
  def apply(l: List[RomanDigit]): RomanNumeral =
    if l.isEmpty then Nulla else RomanDigits(l.sorted.reverse).optimize

  extension (i: Int)
    def toRomanNumeral: RomanNumeral =
      @tailrec def intToRomanNumeralHelper(digits: List[RomanDigit], acc: List[RomanDigit], remainder: Int): RomanNumeral =
        if remainder == 0 then
          RomanNumeral(acc)
        else digits match
          case Nil =>
            RomanNumeral(acc)
          case hd :: tl =>
            if hd.toInt <= remainder then
              intToRomanNumeralHelper(digits, acc ++ List(hd), remainder - hd.toInt)
            else
              intToRomanNumeralHelper(tl, acc, remainder)
      intToRomanNumeralHelper(digits = List(M, D, C, L, X, V, I), acc = List(), remainder = i)

  def uncompact(l: List[RomanDigit]): List[RomanDigit] = {
    val substitutes = ListMap[List[RomanDigit], List[RomanDigit]](
      List(I, V) -> List(I, I, I, I),
      List(I, X) -> List(V, I, I, I, I),
      List(X, L) -> List(X, X, X, X),
      List(X, C) -> List(L, X, X, X, X),
      List(C, D) -> List(C, C, C, C),
      List(C, M) -> List(D, C, C, C, C))
    l.substitute(substitutes)
  }

  @tailrec private def validMs(l: List[RomanDigit]): Boolean = l match
    case M :: tl => validMs(tl)
    case _ => validCs(l)

  def validCs(l: List[RomanDigit]): Boolean = l match
    case C :: M :: tl => validXs(tl)
    case D :: C :: C :: C :: tl => validXs(tl)
    case D :: C :: C :: tl => validXs(tl)
    case D :: C :: tl => validXs(tl)
    case D :: tl => validXs(tl)
    case C :: D :: tl => validXs(tl)
    case C :: C :: C :: tl => validXs(tl)
    case C :: C :: tl => validXs(tl)
    case C :: tl => validXs(tl)
    case _ => validXs(l)

  def validXs(l: List[RomanDigit]): Boolean = l match
    case X :: C :: tl => validIs(tl)
    case L :: X :: X :: X :: tl => validIs(tl)
    case L :: X :: X :: tl => validIs(tl)
    case L :: X :: tl => validIs(tl)
    case L :: tl => validIs(tl)
    case X :: L :: tl => validIs(tl)
    case X :: X :: X :: tl => validIs(tl)
    case X :: X :: tl => validIs(tl)
    case X :: tl => validIs(tl)
    case _ => validIs(l)

  def validIs(l: List[RomanDigit]): Boolean = l match
    case I :: X :: Nil => true
    case V :: I :: I :: I :: Nil => true
    case V :: I :: I :: Nil => true
    case V :: I :: Nil => true
    case V :: Nil => true
    case I :: V :: Nil => true
    case I :: I :: I :: Nil => true
    case I :: I :: Nil => true
    case I :: Nil => true
    case Nil => true
    case _ => false

  extension (s: String)
    def toRomanNumeral: Option[RomanNumeral] =
      val candidate = s.map(RomanDigit.fromChar(_)).toList
      s match
        case "nulla" =>
          Option(RomanNumeral())
        case _ if !candidate.contains(None) && validMs(candidate.flatten) =>
          Option(RomanNumeral(uncompact(candidate.flatten)))
        case _ =>
          None
  
  extension (lhs: RomanNumeral)
    def +(rhs: RomanDigit): RomanNumeral = lhs.plus(RomanNumeral(rhs))
    def -(rhs: RomanDigit): RomanNumeral = lhs.minus(RomanNumeral(rhs))
    def *(rhs: RomanDigit): RomanNumeral = lhs.times(RomanNumeral(rhs))
    def /(rhs: RomanDigit): RomanNumeral = lhs.div(RomanNumeral(rhs))
    def +(rhs: RomanNumeral): RomanNumeral = lhs.plus(rhs)
    def -(rhs: RomanNumeral): RomanNumeral = lhs.minus(rhs)
    def *(rhs: RomanNumeral): RomanNumeral = lhs.times(rhs)
    def /(rhs: RomanNumeral): RomanNumeral = lhs.div(rhs)

end RomanNumeral

