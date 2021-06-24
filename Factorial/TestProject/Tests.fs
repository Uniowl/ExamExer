module Tests

open Factorial
open Xunit
open FsCheck.Xunit

[<Fact>]
let ``Testing factorial`` () =
    let answer = fact(5,1)
    Assert.Equal(answer,  120)
    

[<Property>]
let ``Result is always larger or equal to input`` (x:int) =
    let answer = fact(x, 1)
    Assert.True(answer > x)
    
   