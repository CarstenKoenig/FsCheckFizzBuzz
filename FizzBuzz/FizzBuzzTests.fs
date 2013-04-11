namespace FizzBuzz

open NUnit.Framework
open FsCheck

[<AutoOpen>]
module private Helpers =

    let checkFizzBuzzFor property =
        let check nr =
            property nr <| FizzBuzzer.fizzBuzz nr
            |@ sprintf "bei fizzBuzz für Eingabe %d" nr
        check

    let checkFizzBuzzForMultiples mult property =
        checkFizzBuzzFor property
        |> Prop.forAll (DataGenerators.arbitraryMultiplesOf mult)

    let checkFizzBuzzForNonMultiples mults property =
        checkFizzBuzzFor property
        |> Prop.forAll (DataGenerators.arbitraryNotMultiplesOf mults)

    let shouldContain expected input (ausgabe : string) = 
        ausgabe.Contains(expected) |@ sprintf "'%s' enthält '%s' nicht" ausgabe expected

    let shouldBe expected input (ausgabe : string) = 
        ausgabe = expected |@ sprintf "'%s' <> '%s'" ausgabe expected

    let shouldJustBeConvertedToString input result = 
        result = input.ToString() |@ sprintf "sollte Zahl als String zurückgeben, war aber '%s'" result

    let shouldJustBeConvertedToStringOrBe expected input result = 
        (result = input.ToString() ||
         result = expected)
        |@ sprintf "sollte Zahl als String zurückgeben, war aber '%s'" result

module Tests = 

    [<Test>]
    let ``Vielfache von 3 enthalten Fizz``() =
        checkFizzBuzzForMultiples 3 <| shouldContain "Fizz"
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``Vielfache von 5 enthalten Buzz``() =
        checkFizzBuzzForMultiples 5 <| shouldContain "Buzz"
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``Vielfache von 15 sind FizzBuzz``() =
        checkFizzBuzzForMultiples 15 <| shouldBe "FizzBuzz"
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``Zahlen die 3 nicht als Faktoren enthalten werden als String zurückgegeben oder sind Buzz``() =
        checkFizzBuzzForNonMultiples [3] (shouldJustBeConvertedToStringOrBe "Buzz")
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``Zahlen die 5 nicht als Faktoren enthalten werden als String zurückgegeben oder sind Fizz``() =
        checkFizzBuzzForNonMultiples [5] (shouldJustBeConvertedToStringOrBe "Fizz")
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``Zahlen die weder 3 noch 5 als Faktoren enthalten werden als String zurückgegeben``() =
        checkFizzBuzzForNonMultiples [3; 5] shouldJustBeConvertedToString
        |> Check.QuickThrowOnFailure


