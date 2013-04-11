namespace FizzBuzz

open NUnit.Framework
open FsCheck

module DataGeneratorTests = 

    let checkGeneratorFor n =
        let check nr =
            let remainder = nr % n
            remainder = 0 |@ sprintf "generated number %d mod %d == %d" nr n remainder
        check
        |> Prop.forAll (DataGenerators.arbitraryMultiplesOf n)
        |> Check.QuickThrowOnFailure

    let checkGeneratorForNonMultiplesOf ns =
        let check nr =
            let rec checkSingle ns =
                match ns with
                | []    ->
                    true |@ "ok"
                | n::ns ->
                    let remainder = nr % n
                    if remainder = 0
                    then false |@ sprintf "generated number %d is divisible by %d" nr n
                    else checkSingle ns
            checkSingle ns
        check
        |> Prop.forAll (DataGenerators.arbitraryNotMultiplesOf ns)
        |> Check.QuickThrowOnFailure


    [<Test>]
    let ``generated integers by arbitraryMultiplesOf 3 are 0 mod 3``() =
        checkGeneratorFor 3

    [<Test>]
    let ``generated integers by arbitraryMultiplesOf 5 are 0 mod 5``() =
        checkGeneratorFor 3

    [<Test>]
    let ``generated integers by arbitraryNotMultiplesOf [3;5] are not divisible by 3 nor 5``() =
        checkGeneratorForNonMultiplesOf [3; 5]
