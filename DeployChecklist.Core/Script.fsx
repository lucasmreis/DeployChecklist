// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "DeployChecklist.Core.fs"
open DeployChecklist.Core

let a = DeployChecklist.Core.CheckList.Pulled

let b = DeployChecklist.Core.CheckList.update DeployChecklist.Core.CheckList.Push a

printfn "%A" b

// match b with
// | x, y -> printfn x
