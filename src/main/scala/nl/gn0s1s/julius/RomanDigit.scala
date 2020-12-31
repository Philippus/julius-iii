package nl.gn0s1s.julius

enum RomanDigit extends Ordered[RomanDigit]:

  case I extends RomanDigit
  case V extends RomanDigit
  case X extends RomanDigit
  case L extends RomanDigit
  case C extends RomanDigit
  case D extends RomanDigit
  case M extends RomanDigit

  def compare(that: RomanDigit): Int =
    if this == that then 0
    else if this < that then -1
    else 1
  
  override def <(that: RomanDigit): Boolean = that match
    case I => false
    case V => this == I
    case X => this <= V
    case L => this <= X
    case C => this <= L
    case D => this <= C
    case M => this <= D
end RomanDigit


object RomanDigit:
  def fromChar(c: Char): Option[RomanDigit] = c match {
    case 'I' => Some(I)
    case 'V' => Some(V)
    case 'X' => Some(X)
    case 'L' => Some(L)
    case 'C' => Some(C)
    case 'D' => Some(D)
    case 'M' => Some(M)
    case _ => None
  }
  
  extension (lhs: RomanDigit)
    def +(rhs: RomanDigit): RomanNumeral = RomanNumeral(lhs).plus(RomanNumeral(rhs))
    def -(rhs: RomanDigit): RomanNumeral = RomanNumeral(lhs).minus(RomanNumeral(rhs))
    def *(rhs: RomanDigit): RomanNumeral = RomanNumeral(lhs).times(RomanNumeral(rhs))
    def /(rhs: RomanDigit): RomanNumeral = RomanNumeral(lhs).div(RomanNumeral(rhs))
    def +(rhs: RomanNumeral): RomanNumeral = RomanNumeral(lhs).plus(rhs)
    def -(rhs: RomanNumeral): RomanNumeral = RomanNumeral(lhs).minus(rhs)
    def *(rhs: RomanNumeral): RomanNumeral = RomanNumeral(lhs).times(rhs)
    def /(rhs: RomanNumeral): RomanNumeral = RomanNumeral(lhs).div(rhs)
    
  extension (romanDigit: RomanDigit)
    def toInt: Int = romanDigit match
      case RomanDigit.I => 1
      case RomanDigit.V => 5
      case RomanDigit.X => 10
      case RomanDigit.L => 50
      case RomanDigit.C => 100
      case RomanDigit.D => 500
      case RomanDigit.M => 1000
  
    def toChar: Char = romanDigit match
      case RomanDigit.I => 'I'
      case RomanDigit.V => 'V'
      case RomanDigit.X => 'X'
      case RomanDigit.L => 'L'
      case RomanDigit.C => 'C'
      case RomanDigit.D => 'D'
      case RomanDigit.M => 'M'
end RomanDigit
