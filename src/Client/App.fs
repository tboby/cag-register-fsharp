module App

open Elmish
open Elmish.React
open Fable.Core.JsInterop

importSideEffects "./index.css"
importAll "ag-grid-community/styles/ag-grid.css"
importAll "ag-grid-community/styles/ag-theme-alpine.css"


#if DEBUG
open Elmish.HMR
#endif

Program.mkProgram Index.init Index.update Index.view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactSynchronous "elmish-app"

|> Program.run