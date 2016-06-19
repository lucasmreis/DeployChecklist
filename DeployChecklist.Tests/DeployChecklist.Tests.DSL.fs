module DSL
    open Swensen.Unquote
    open Xunit
    open Chessie.ErrorHandling
    open DeployChecklist.Core.CheckList

    let Given (state: State) = state

    let When command state = (command, state)

    let ThenStateShouldBe expectedState (command, state) =
        match evolve command state with
        | Ok ((actualState, events), _) ->
            actualState =! expectedState
            Some events
        | Bad err ->
            failwith (sprintf "Expected State: %A ; But: %A" expectedState err)
            None

    let ThenStateShouldNotChange (command, state) =
        match evolve command state with
        | Ok _ ->
            failwith (sprintf "State should not have changed when command %A to state %A" command state)
        | Bad _ -> Assert.True true

    let WithEvents expectedEvents actualEvents =
        match actualEvents with
        | Some e -> e =! expectedEvents
        | None -> failwith (sprintf "Expected Events: %A" expectedEvents)