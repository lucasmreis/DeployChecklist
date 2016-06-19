namespace DeployChecklist.Tests

open Xunit
open FsCheck.Xunit
open Swensen.Unquote
open DSL
open DeployChecklist.Core.CheckList

module Tests =

    [<Property>]
    let ``Pull only works when Initial``(state: State) =
        match state with
        | Initial ->
            Given Initial
            |> When Pull
            |> ThenStateShouldBe Pulled
            |> WithEvents [ CodePulled ]
        | another ->
            Given another
            |> When Pull
            |> ThenStateShouldNotChange

    [<Property>]
    let ``Test works only when Pulled``(state: State) =
        match state with
        | Pulled ->
            Given Pulled
            |> When Test
            |> ThenStateShouldBe Tested
            |> WithEvents [ CodeTested ]
        | another ->
            Given another
            |> When Test
            |> ThenStateShouldNotChange

    [<Property>]
    let ``Push works only when Tested``(state: State) =
        match state with
        | Tested ->
            Given Tested
            |> When Push
            |> ThenStateShouldBe (CI { red = Waiting ; blue = Waiting })
            |> WithEvents [ CodePushed ]
        | another ->
            Given another
            |> When Push
            |> ThenStateShouldNotChange

    [<Property>]
    let ``Build Red works only when Waiting``(state: State) =
        match state with
        | CI { red = Waiting ; blue = b } ->
            Given (CI { red = Waiting ; blue = b })
            |> When (Build Red)
            |> ThenStateShouldBe (CI { red = Built ; blue = b })
            |> WithEvents [ SiteBuilt Red ]
        | another ->
            Given another
            |> When (Build Red)
            |> ThenStateShouldNotChange

    [<Property>]
    let ``Deploy Red works only when Waiting``(state: State) =
        match state with
        | CI { red = Built ; blue = b } ->
            Given (CI { red = Built ; blue = b })
            |> When (Deploy Red)
            |> ThenStateShouldBe (CI { red = Deployed ; blue = b })
            |> WithEvents [ SiteDeployed Red ]
        | another ->
            Given another
            |> When (Deploy Red)
            |> ThenStateShouldNotChange

    [<Property>]
    let ``Build Blue works only when Waiting``(state: State) =
        match state with
        | CI { blue = Waiting ; red = r } ->
            Given (CI { blue = Waiting ; red = r })
            |> When (Build Blue)
            |> ThenStateShouldBe (CI { blue = Built ; red = r })
            |> WithEvents [ SiteBuilt Blue ]
        | another ->
            Given another
            |> When (Build Blue)
            |> ThenStateShouldNotChange

    [<Property>]
    let ``Deploy Blue works only when Waiting``(state: State) =
        match state with
        | CI { blue = Built ; red = r } ->
            Given (CI { blue = Built ; red = r })
            |> When (Deploy Blue)
            |> ThenStateShouldBe (CI { blue = Deployed ; red = r })
            |> WithEvents [ SiteDeployed Blue ]
        | another ->
            Given another
            |> When (Deploy Blue)
            |> ThenStateShouldNotChange




    // [<Property>]
    // let ``Pull only works when Initial``(state: State) =
    //     let actual = update Pull state
    //     printfn "BEFORE: %A >> AFTER %A" state actual
    //     match state with
    //     | Initial ->
    //         actual =! (Some Pulled , [ Test ])
    //     | another ->
    //         match actual with
    //         | (shouldBeNone, _) -> shouldBeNone =! None


