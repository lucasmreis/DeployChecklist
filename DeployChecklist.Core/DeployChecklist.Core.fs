namespace DeployChecklist.Core

module CheckList =
    // ACTIONS

    type Site =
        | Red
        | Blue

    type Action =
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

    type Model =
        | Initial
        | Pulled
        | Tested
        | CI of CI

    type AvailableActions = Action list

    type ActionResult = OK | Error

    type UpdateResult = ActionResult * Model * AvailableActions

    // AVAILABLE ACTIONS GIVEN MODEL

    let avActionForPackage site package =
        match package with
        | Waiting -> Some (Build site)
        | Built -> Some (Deploy site)
        | Deployed -> None

    let avActionsForCI (ci: CI): AvailableActions =
        let avRed = avActionForPackage Red ci.red
        let avBlue = avActionForPackage Blue ci.blue
        match avRed, avBlue with
        | Some r , Some b -> [ r ; b ]
        | Some r , _ -> [ r ]
        | _ , Some b -> [ b ]
        | _ , _ -> []

    let avActions model =
        match model with
        | Initial -> [ Pull ]
        | Pulled -> [ Test ]
        | Tested -> [ Push ]
        | CI ci -> avActionsForCI ci

    // UPDATE

    let pull m = Pulled

    let test m = Tested

    let push m = CI { red = Waiting ; blue = Waiting }

    let toSiteState state site m =
        match site with
        | Red -> CI { m with red = state }
        | Blue -> CI { m with blue = state }

    let build = toSiteState Built

    let deploy = toSiteState Deployed

    let unsafeUpdate action model =
        match action, model with
        | Pull , _ -> pull model
        | Test , _ -> test model
        | Push , _ -> push model
        | Build site , CI ci -> build site ci
        | Deploy site , CI ci -> deploy site ci
        | _ , _ -> model // this should never happen

    let update action model =
        let allowedActions = avActions model
        let actionIsAllowed = List.contains action allowedActions
        if actionIsAllowed then
            let updated = unsafeUpdate action model
            Some updated , (avActions updated)
        else
            None , allowedActions
