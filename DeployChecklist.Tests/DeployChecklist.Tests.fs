namespace DeployChecklist.Tests

open Xunit
open Swensen.Unquote
open DeployChecklist.Core.CheckList

module Tests =

    [<Fact>]
    let ``Pull works``() =
      let state = Initial
      let expected = Pulled
      fst (update Pull Initial) =! Some expected
