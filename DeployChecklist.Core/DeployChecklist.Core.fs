namespace DeployChecklist.Core

module CheckList =
    open Chessie.ErrorHandling

    // COMMANDS

    type Site =
        | Red
        | Blue

    type Command =
        | Pull
        | Test
        | Push
        | Build of Site
        | Deploy of Site

    // MODEL

    type Package =
        | Waiting
        | Built
        | Deployed

    type CI =
        { red: Package
          blue: Package }

    type State =
        | Initial
        | Pulled
        | Tested
        | CI of CI

    // AVAILABLE COMMANDS GIVEN STATE

    let avCommandsForPackage site package =
        match package with
        | Waiting -> [ Build site ]
        | Built -> [ Deploy site ]
        | Deployed -> []

    let avCommandsForCI ci =
        let avRed = avCommandsForPackage Red ci.red
        let avBlue = avCommandsForPackage Blue ci.blue
        List.concat [ avRed ; avBlue ]

    let avCommands state =
        match state with
        | Initial -> [ Pull ]
        | Pulled -> [ Test ]
        | Tested -> [ Push ]
        | CI ci -> avCommandsForCI ci

    // EVENTS

    type Event =
        | CodePulled
        | CodeTested
        | CodePushed
        | SiteBuilt of Site
        | SiteDeployed of Site

    type Error =
        | AlreadyPulled
        | ShouldBePulled
        | ShouldBeTested
        | NotWaiting of Site
        | NotBuilt of Site

    // EVOLVE

    let pull s =
        match s with
        | Initial -> [ CodePulled ] |> ok
        | _ -> AlreadyPulled |> fail

    let test s =
        match s with
        | Pulled -> [ CodeTested ] |> ok
        | _ -> ShouldBePulled |> fail

    let push s =
        match s with
        | Tested -> [ CodePushed ] |> ok
        | _ -> ShouldBeTested |> fail

    let build site s =
        match site , s with
        | Red , CI { red = Waiting } -> [ SiteBuilt Red ] |> ok
        | Blue , CI { blue = Waiting } -> [ SiteBuilt Blue ] |> ok
        | Red , _ -> NotWaiting Red |> fail
        | Blue , _ -> NotWaiting Blue |> fail

    let deploy site s =
        match site , s with
        | Red , CI { red = Built } -> [ SiteDeployed Red ] |> ok
        | Blue , CI { blue = Built } -> [ SiteDeployed Blue ] |> ok
        | Red , _ -> NotBuilt Red |> fail
        | Blue , _ -> NotBuilt Blue |> fail

    let execute command state =
        match command with
        | Pull -> pull state
        | Test -> test state
        | Push -> push state
        | Build site -> build site state
        | Deploy site -> deploy site state

    let apply state event =
        match event , state with
        | CodePulled , Initial -> Pulled
        | CodeTested , Pulled -> Tested
        | CodePushed , Tested -> CI { red = Waiting ; blue = Waiting }
        | SiteBuilt Red , CI { red = Waiting ; blue = b } -> CI { red = Built ; blue = b }
        | SiteDeployed Red , CI { red = Built ; blue = b } -> CI { red = Deployed ; blue = b }
        | SiteBuilt Blue , CI { blue = Waiting ; red = r } -> CI { blue = Built ; red = r }
        | SiteDeployed Blue , CI { blue = Built ; red = r } -> CI { blue = Deployed ; red = r }
        | _ , s -> s

    let evolve command state =
        match execute command state with
        | Ok (events, _) ->
            let newState = List.fold apply state events
            (newState, events) |> ok
        | Bad err -> Bad err


    // // UPDATE

    // let unsafeUpdate command state =
    //     match command , state with
    //     | Pull , Initial -> Some Pulled
    //     | Test , Pulled -> Some Tested
    //     | Push , Tested -> CI { red = Waiting ; blue = Waiting } |> Some
    //     | Build Red , CI ci -> CI { ci with red = Built } |> Some
    //     | Build Blue , CI ci -> CI { ci with blue = Built } |> Some
    //     | Deploy Red , CI ci -> CI { ci with red = Deployed } |> Some
    //     | Deploy Blue , CI ci -> CI { ci with blue = Deployed } |> Some
    //     | _ , _ -> None


    // let update command state =
    //     let allowedCommands = avCommands state
    //     if List.contains command allowedCommands then
    //         match unsafeUpdate command state with
    //         | Some state -> Some state , avCommands state
    //         | None -> None, allowedCommands
    //     else
    //         None, allowedCommands