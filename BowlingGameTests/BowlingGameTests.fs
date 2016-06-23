namespace BowlingKataTests
open Xunit
open Swensen.Unquote
open BowlingKata.BowlingGame

module BowlingGameTests =

    [<Fact>]
    let ShouldBePossibleToCreateGameWithRolls() =
        let g = Game []
        Assert.NotNull(g)

    [<Fact>]
    let ShouldBePossibleToParseGameResult() =
        let rolls = parseGame "X-/1-" |> fun (Game r) -> r
        let exp = [(Strike, Pins 10); (Roll, Pins 0); (Spare, Pins 10); (Roll, Pins 1); (Roll, Pins 0)]
        Assert.Equal<(Rolls * Pins) list>(exp, rolls)

    [<Fact>]
    let ``calculate scores with no strikes or spares``() =
        test <@ scoreGame (parseGame "11111111111111111111") = 20 @>
        test <@ scoreGame (parseGame "11111111112222222222") = 30 @>
        test <@ scoreGame (parseGame "13521") = 12 @> 

    [<Fact>]
    let ``calculate scores containing a miss``() =
        test <@ scoreGame (parseGame "--------------------") = 0 @> 
        test <@ scoreGame (parseGame "1-1----------------1") = 3 @> 
        test <@ scoreGame (parseGame "9-9-9-9-9-9-9-9-9-9-") = 90 @> 

    [<Fact>]
    let ``calculate scores containing spares``() =
        test <@ scoreGame (parseGame "5/3-----------------") = 16 @> 
        test <@ scoreGame (parseGame "5/11------------3/11") = 26 @> 
        test <@ scoreGame (parseGame "5/5/5/5/5/5/5/5/5/5/5") = 150 @> 

    [<Fact>]
    let ``calculate scores containing strikes``() =
        test <@ scoreGame (parseGame "X34----------------") = 24 @> 
        test <@ scoreGame (parseGame "XXX--------------") = 60 @> 
        test <@ scoreGame (parseGame "XXXXXXXXX---") = 240 @> 
        test <@ scoreGame (parseGame "XXXXXXXXXX--") = 270 @> 
        test <@ scoreGame (parseGame "XXXXXXXXXXX-") = 290 @> 
        test <@ scoreGame (parseGame "XXXXXXXXXXXX") = 300 @>
        test <@ scoreGame (parseGame "XXXXXXXXXX12") = 274 @> 
        test <@ scoreGame (parseGame "1/35XXX458/X3/23") = 160 @> 
        test <@ scoreGame (parseGame "1/35XXX458/X3/XX6") = 189 @> 
