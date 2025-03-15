module Server.Server

open System.IO
open SAFE
open Saturn
open Server.ApplicationsApi
open OfficeOpenXml // Ensure you have this for EPPlus

let webApp =
    router {
        forward "/ICagApplicationsApi" (Api.make applicationsApi)
    }

let app = application {
    use_router (Api.make(
        api = applicationsApi,
        errorHandler = (fun ex _ ->
            printfn "An error occurred processing your request: %s" ex.Message;
            (Fable.Remoting.Server.Ignore)
        )
    ))
    memory_cache
    use_static "public"
    use_gzip
}

let staticApp = application {
    use_gzip
    memory_cache
    no_router
    use_static ""
}

[<EntryPoint>]
let main args =
    ExcelPackage.LicenseContext <- LicenseContext.NonCommercial // Set the license context
    match args with
    | [||] -> run app
    | [|"generate"; rootOutput|] ->
        Directory.CreateDirectory rootOutput |> ignore
        ToCsv.serializeAll(rootOutput) |> Async.RunSynchronously
    | [|"static"|] -> run staticApp
    | _ -> failwithf "Unexpected args"
    0