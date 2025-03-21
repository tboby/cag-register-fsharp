open Fake.Core
open Fake.IO
open Farmer
open Farmer.Builders

open Helpers

initializeContext ()

let sharedPath = Path.getFullName "src/Shared"
let serverPath = Path.getFullName "src/Server"
let clientPath = Path.getFullName "src/Client"
let staticPath = Path.getFullName "src/StaticServer"
let deployPath = Path.getFullName "deploy"
let sharedTestsPath = Path.getFullName "tests/Shared"
let serverTestsPath = Path.getFullName "tests/Server"
let clientTestsPath = Path.getFullName "tests/Client"

Target.create "Clean" (fun _ ->
    Shell.cleanDir deployPath
    run dotnet [ "fable"; "clean"; "--yes" ] clientPath // Delete *.fs.js files created by Fable
)

Target.create "RestoreClientDependencies" (fun _ -> run npm [ "ci" ] ".")

Target.create "Bundle" (fun _ ->
    [
        "server", dotnet [ "publish"; "-c"; "Release"; "-o"; deployPath ] serverPath
        "client", dotnet [ "fable"; "-o"; "output"; "-s"; "--run"; "npx"; "vite"; "build"; "--config"; "vite.config.local.mts" ] clientPath
    ]
    |> runParallel)

Target.create "PublishClient" (fun _ ->
        run dotnet [ "fable"; "--define"; "CSV"; "-o"; "output"; "-s"; "--run"; "npx"; "vite"; "build" ; "--config"; "vite.config.static.mts" ] clientPath
    )

Target.create "Azure" (fun _ ->
    let web = webApp {
        name "SAFE-App"
        operating_system OS.Linux
        runtime_stack (DotNet "8.0")
        zip_deploy "deploy"
    }

    let deployment = arm {
        location Location.WestEurope
        add_resource web
    }

    deployment |> Deploy.execute "SAFE-App" Deploy.NoParameters |> ignore)

Target.create "Run" (fun _ ->
    run dotnet [ "build" ] sharedPath

    [
        "server", dotnet [ "watch"; "run" ] serverPath
        "client", dotnet [ "fable"; "watch"; "-o"; "output"; "-s"; "--run"; "npx"; "vite"; "--config"; "vite.config.local.mts" ] clientPath
    ]
    |> runParallel)

Target.create "RunCsv" (fun _ ->
    run dotnet [ "build" ] sharedPath

    [
        "server", dotnet [ "watch"; "run"; "static" ] serverPath
        "client", dotnet [ "fable"; "watch"; "--define"; "CSV"; "-o"; "output"; "-s"; "--run"; "npx"; "vite" ; "--config"; "vite.config.local.mts"] clientPath
    ]
    |> runParallel)

Target.create "Regenerate" (fun _ ->
    run dotnet [ "build" ] serverPath
    run dotnet [ "run"; "generate"; "api-responses" ] serverPath
    )

Target.create "RunTests" (fun _ ->
    run dotnet [ "build" ] sharedTestsPath

    [
        "server", dotnet [ "watch"; "run" ] serverTestsPath
        "client", dotnet [ "fable"; "watch"; "-o"; "output"; "-s"; "--run"; "npx"; "vite" ; "--config"; "vite.config.local.mts"] clientTestsPath
    ]
    |> runParallel)

Target.create "Format" (fun _ -> run dotnet [ "fantomas"; "." ] ".")

open Fake.Core.TargetOperators

let dependencies = [
    "Clean" ==> "RestoreClientDependencies" ==> "Bundle" ==> "Azure"
    "Clean" ==> "RestoreClientDependencies" ==> "PublishClient"

    "Clean" ==> "RestoreClientDependencies" ==> "Run"
    "Clean" ==> "RestoreClientDependencies" ==> "RunCsv"

    "RestoreClientDependencies" ==> "RunTests"
]

[<EntryPoint>]
let main args = runOrDefault args