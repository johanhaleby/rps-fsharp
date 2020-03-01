module GameRulesTest

open Xunit
open RPS
open Game

[<Fact>]
let ``rock beats scissors``() =
    Assert.True(Rock .beats Scissors)
    
[<Fact>]
let ``paper beats rock``() =
    Assert.True(Paper .beats Rock)
    
[<Fact>]
let ``scissors beats paper``() =
    Assert.True(Scissors .beats Paper)
    
[<Fact>]
let ``scissors doesn't beat rock``() =
    Assert.False(Scissors .beats Rock)

[<Fact>]
let ``rock doesn't beat paper``() =
    Assert.False(Rock .beats Paper)

[<Fact>]
let ``paper doesn't beat scissors``() =
    Assert.False(Rock .beats Paper)
