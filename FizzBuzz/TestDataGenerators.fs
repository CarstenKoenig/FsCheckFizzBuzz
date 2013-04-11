namespace FizzBuzz

open FsCheck


module private DataGenerators =
    
    let multipleGenerator mult = 
        gen {
            let! n  = Arb.generate<PositiveInt>
            return mult * n.Get
        }

    let rec multipleShrinker mult a =
        seq {
            if a > mult
            then let a' = a - mult
                 yield a'
                 yield! multipleShrinker mult a'
        }

    let notMultiplesOfGenerator mults =
        let notDivBy n d = n % d <> 0
        let notDivisibleByMults n = 
            mults |> Seq.forall (notDivBy n)
        Arb.generate<PositiveInt>
        |> Gen.map (fun (PositiveInt n) -> n)
        |> Gen.suchThat notDivisibleByMults

    let arbitraryMultiplesOf n =
        { new Arbitrary<int>() with
            override x.Generator = multipleGenerator n
            override x.Shrinker a = 
                a |> multipleShrinker n
        }

    let arbitraryNotMultiplesOf ns =
        { new Arbitrary<int>() with
            override x.Generator = notMultiplesOfGenerator ns
        }