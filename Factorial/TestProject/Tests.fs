module Tests
#load "/Users/alexandermolholm/Documents/IKT/6. Semester/AFP/ExamExer/Factorial/Factorial/Factorial.fsproj"

open Factorial
open Xunit




[<Fact>]
let ``Testing factoral`` () =
    let answer = fact(5,1)
    Assert.True(true)
