module Server.Server

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

[<EntryPoint>]
let main _ =
    ExcelPackage.LicenseContext <- LicenseContext.NonCommercial // Set the license context
    run app
    0