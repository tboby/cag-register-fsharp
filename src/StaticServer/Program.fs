open Saturn
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.FileProviders
open System.IO
open Microsoft.AspNetCore.Http
// For more information see https://aka.ms/fsharp-console-apps
let app = application {
    use_gzip
    url "http://localhost:5000/"
    no_router
    use_static ""
}
[<EntryPoint>]
let main _ =
    run app
    0 // return an integer exit code