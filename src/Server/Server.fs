module Server

open SAFE
open Saturn
open Shared

///
/// 
module CagRegisterXLSM =
    let cagRegisterFile = "cag-register-research-master-2021_1uhwQyX.xlsm"
    open System.IO
    open ClosedXML.Excel

    let getApplications() =
        let workbook = new XLWorkbook(cagRegisterFile)
        let indexSheet = workbook.Worksheet("Index")
        
        // Get first 20 application numbers from Index sheet
        let applications = 
            [2..21] // Row 1 is header, get next 20 rows
            |> List.map (fun row ->
                let appNo = indexSheet.Cell(row, 1).GetString()
                appNo
            )
            |> List.filter (fun appNo -> not (System.String.IsNullOrWhiteSpace(appNo)))

        // Read each application's sheet
        let appSheets =
            applications
            |> List.map (fun appNo ->
                let sheetName = "A" + appNo
                try
                    let sheet = workbook.Worksheet(sheetName)
                    Some (appNo, sheet)
                with
                | _ -> None
            )
            |> List.choose id
            
        workbook.Dispose()
        appSheets

module Storage =
    let todos =
        ResizeArray [
            Todo.create "Create new SAFE project"
            Todo.create "Write your app"
            Todo.create "Ship it!!!"
        ]

    let addTodo todo =
        if Todo.isValid todo.Description then
            todos.Add todo
            Ok()
        else
            Error "Invalid todo"

let todosApi ctx = {
    getTodos = fun () -> async { return Storage.todos |> List.ofSeq }
    addTodo =
        fun todo -> async {
            return
                match Storage.addTodo todo with
                | Ok() -> todo
                | Error e -> failwith e
        }
}

let webApp = Api.make todosApi

let app = application {
    use_router webApp
    memory_cache
    use_static "public"
    use_gzip
}

[<EntryPoint>]
let main _ =
    run app
    0