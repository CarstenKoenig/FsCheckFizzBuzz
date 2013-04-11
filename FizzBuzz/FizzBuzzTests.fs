namespace FizzBuzz

open NUnit.Framework
open FsCheck

[<AutoOpen>]
module private Helpers =

    let checkFizzBuzzFor property =
        let check nr =
            property nr <| FizzBuzzer.fizzBuzz nr
            |@ sprintf "at fizzBuzz for input %d" nr
        check

    let checkFizzBuzzForMultiples mult property =
        checkFizzBuzzFor property
        |> Prop.forAll (DataGenerators.arbitraryMultiplesOf mult)

    let checkFizzBuzzForNonMultiples mults property =
        checkFizzBuzzFor property
        |> Prop.forAll (DataGenerators.arbitraryNotMultiplesOf mults)

    let shouldContain expected input (output : string) = 
        output.Contains(expected) |@ sprintf "'%s' does not contain '%s'" output expected

    let shouldBe expected input (output : string) = 
        output = expected |@ sprintf "'%s' was expected to be '%s'" output expected

    let shouldJustBeConvertedToString input result = 
        result = input.ToString() |@ sprintf "was expected to be just converted into a string but was '%s'" result

    let shouldJustBeConvertedToStringOrBe expected input result = 
        (result = input.ToString() ||
         result = expected)
        |@ sprintf "was expected to be '%s' or just be converted into a string but was '%s'" expected result

module Tests = 

    [<Test>]
    let ``multiples of 3 are converted by fizzBuzz into a string containing Fizz``() =
        checkFizzBuzzForMultiples 3 <| shouldContain "Fizz"
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``multiples of 5 are converted by fizzBuzz into a string containing Buzz``() =
        checkFizzBuzzForMultiples 5 <| shouldContain "Buzz"
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``multiples of 15 are converted by fizzBuzz into 'FizzBuzz'``() =
        checkFizzBuzzForMultiples 15 <| shouldBe "FizzBuzz"
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``integers not divisible by 3 are converted by fizzBuzz into a string or 'Buzz'``() =
        checkFizzBuzzForNonMultiples [3] (shouldJustBeConvertedToStringOrBe "Buzz")
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``integers not divisible by 5 are converted by fizzBuzz into a string or 'Fizz'``() =
        checkFizzBuzzForNonMultiples [5] (shouldJustBeConvertedToStringOrBe "Fizz")
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``integers not divisible by 3 or by 5 are converted by fizzBuzz into 'FizzBuzz'``() =
        checkFizzBuzzForNonMultiples [3; 5] shouldJustBeConvertedToString
        |> Check.QuickThrowOnFailure


