module App

open Elmish
open Elmish.React
open Fable.Core.JsInterop

importSideEffects "./index.css"
importAll "ag-grid-community/styles/ag-grid.css"
importAll "ag-grid-community/styles/ag-theme-alpine.css"

// Import the function
let provideGlobalGridOptions: obj -> unit = importMember "ag-grid-community"

// Use the function to set global grid options
provideGlobalGridOptions(
    createObj [
        "paginationPageSizeSelector" ==> [| "10"; "20"; "50"; "100" |]
        "pagination" ==> true
        // Add other global options as needed
    ]
)


#if DEBUG
open Elmish.HMR
#endif

Program.mkProgram Index.init Index.update Index.view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactSynchronous "elmish-app"

|> Program.run