# Julius III - Roman Numerals in Scala 3 (archived)

This project is archived, scala 3 version is available under https://github.com/philippus/julius.

This project is the same as [Julius](https://github.com/Philippus/julius), but it has been converted to Scala 3.

## Constructing a Roman Numeral

```scala
import nl.gn0s1s.julius.RomanDigit._
import nl.gn0s1s.julius.RomanNumeral

RomanNumeral(List(I, I, X, I)) // res0: RomanNumeral = XIII

import nl.gn0s1s.julius.RomanNumeral.toRomanNumeral

1666.toRomanNumeral // res1: RomanNumeral = MDCLXVI
"XIV".toRomanNumeral // res2: Option[RomanNumeral] = Some(XIV)
```

## Nulla

```scala
import nl.gn0s1s.julius.RomanNumeral

RomanNumeral() // res3: RomanNumeral = nulla

import nl.gn0s1s.julius.RomanNumeral.toRomanNumeral

0.toRomanNumeral // res4: RomanNumeral = nulla
"nulla".toRomanNumeral // res5: Option[RomanNumeral] = Some(nulla)
```

## Operators and expressions

```scala
import nl.gn0s1s.julius.RomanDigit._
import nl.gn0s1s.julius.RomanNumeral

M + M + X + V + I // res6: RomanNumeral = MMXVI

import nl.gn0s1s.julius.RomanNumeral.toRomanNumeral

(3.toRomanNumeral * C / V) - L - X // res7: RomanNumeral = nulla
"XX".toRomanNumeral.get * V // res8: RomanNumeral = C
```

## License
The code is available under the [MIT license](LICENSE.md).
