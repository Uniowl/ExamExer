module Tests

open Factorial
open Xunit

[<Fact>]
let ``Testing factoral`` () =
    let answer = fact(5,1)
    Assert.Equal(answer,  120)
