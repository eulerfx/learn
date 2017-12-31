namespace Learn

module Num =
  let inline GenericTwo () =
    LanguagePrimitives.GenericOne + LanguagePrimitives.GenericOne

  [<GeneralizableValue>]
  let inline GenericThree () =
    LanguagePrimitives.GenericOne 
    + LanguagePrimitives.GenericOne 
    + LanguagePrimitives.GenericOne

  let inline GenericOfInt (n:int) =
    let mutable x = LanguagePrimitives.GenericZero
    let mutable i = 0
    while i < n do
      x <- x + LanguagePrimitives.GenericOne
      i <- i + 1
    x


/// A value together with its derivative.
[<NoComparison>]
type D<'a> = D of 'a * 'a

module D =

  let inline konst (x:'a) : D<'a> =
    D (x, LanguagePrimitives.GenericZero)

  let inline id (x:'a) : D<'a> =    
    D (x, LanguagePrimitives.GenericOne)

  let inline ofInt (x:int) =
    konst (Num.GenericOfInt x)

  let inline add (D(a,a') : D<'a>) (D(b,b') : D<'a>) : D<'a> =
    D (a + b, a' + b')

  let inline mul (D(a,a') : D<'a>) (D(b,b') : D<'a>) : D<'a> =
    D (a * b, a' * b')

  let inline neg (D(a,a') : D<'a>) : D<'a> =
    D (-a, -a')

  let inline abs (D(a,a') : D<'a>) : D<'a> =
    D (abs a, abs a')

  let inline sqr (d:D<'a>) : D<'a> =
    mul d d

  let inline exp (D(a,a') : D<'a>) : D<'a> =
    D (exp a, a' * exp a)

  let inline log (D(a,a') : D<'a>) : D<'a> =
    D (log a, a' / a)

  let inline sqrt (D(a,a') : D<'a>) : D<'a> =
    D (sqrt a, a' / (Num.GenericTwo() * sqrt a))

  let inline sin (D(a,a') : D<'a>) : D<'a> =
    D (sin a, a' * cos a)

  let inline cos (D(a,a') : D<'a>) : D<'a> =    
    D (cos a, a' * (-(Operators.sin a)))

  let inline asin (D(a,a') : D<'a>) : D<'a> =    
    D (asin a, a' / Operators.sqrt (LanguagePrimitives.GenericOne - Operators.pown a 2))

  let inline acos (D(a,a') : D<'a>) : D<'a> =
    D (acos a, a' / (-Operators.sqrt (LanguagePrimitives.GenericOne - (Operators.pown a 2))))

type D<'a> with
    static member (*) (a,b) = D.mul a b
    static member (~-) d = D.neg d
    static member (+) (a,b) = D.add a b
    static member inline Abs d = D.abs d
    static member inline Sqrt d = D.sqrt d
    static member inline Sin d = D.sin d
    static member inline Cos d = D.cos d
    static member inline Log d = D.log d

    static member One : D< ^a> = failwith ""

module DTest =

    let test () = 
        let inline f1 a = sqrt (sin a)    
        let x = f1 (D(2.0,1.0))
        printfn "%A" x
        ()

