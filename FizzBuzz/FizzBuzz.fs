namespace FizzBuzz

module FizzBuzzer = 

    let (|DivBy|_|) d zahl =
        if zahl % d = 0 then Some () else None

    let fizzBuzz n = 
        match n with
        | DivBy 15 -> "FizzBuzz"
        | DivBy 3  -> "Fizz"
        | DivBy 5  -> "Buzz"
        | sonst    -> n.ToString()
