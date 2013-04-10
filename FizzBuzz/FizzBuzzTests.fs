namespace FizzBuzz

open NUnit.Framework
open FsCheck

[<AutoOpen>]
module private Helpers =

    let checkFizzBuzz prädikat =
        let property (PositiveInt n) =
            match prädikat n <| FizzBuzzer.fizzBuzz n with
            | Some result -> result |@ sprintf "bei fizzBuzz für Eingabe %d" n
            | None        -> true |@ "always true"
        Check.QuickThrowOnFailure property

    let checkFizzBuzzForMultiples mult prädikat =
        let property (PositiveInt n) =
            let n = n * mult
            prädikat <| FizzBuzzer.fizzBuzz n
            |@ sprintf "bei fizzBuzz für Eingabe %d" n
        Check.QuickThrowOnFailure property

    let shouldContain expected (ausgabe : string) = 
        ausgabe.Contains(expected) |@ sprintf "'%s' enthält '%s' nicht" ausgabe expected

    let shouldBe expected (ausgabe : string) = 
        ausgabe = expected |@ sprintf "'%s' <> '%s'" ausgabe expected

    let shouldJustBeConvertedToString input result = 
        result = input.ToString() |@ sprintf "sollte Zahl als String zurückgeben, war aber '%s'" result

    let whenInput guard property =
        (fun input result -> if guard input then Some (property input result) else None)

module Tests = 

    [<Test>]
    let ``Vielfache von 3 enthalten Fizz``() =
        checkFizzBuzzForMultiples 3 <| shouldContain "Fizz"

    [<Test>]
    let ``Vielfache von 5 enthalten Buzz``() =
        checkFizzBuzzForMultiples 5 <| shouldContain "Buzz"

    [<Test>]
    let ``Vielfache von 15 sind FizzBuzz``() =
        checkFizzBuzzForMultiples 15 <| shouldBe "FizzBuzz"

    [<Test>]
    let ``Zahlen die weder 3 noch 5 als Faktoren enthalten werden als String zurückgegeben``() =
        let notDivisibleBy5And3 n = 
            let notDiv d n = n % d <> 0
            (notDiv 3 n) && (notDiv 5 n)
        checkFizzBuzz (whenInput notDivisibleBy5And3 shouldJustBeConvertedToString)


