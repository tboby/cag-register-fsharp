module Index

open Elmish
open SAFE
open Shared
open Feliz
open Feliz.AgGrid
open Fable.DateFunctions
open System
open Feliz.Router

let getDisplayNameFromApplication (app: CagApplication) =
    sprintf "%s: %s" app.ApplicationNumber.ApplicationNumber
        (let firstLine = app.Title.Split('\n').[0].Trim()
         if firstLine.Length > 50 then firstLine.Substring(0, 47) + "..."
         else firstLine)

let getRegisterTypeFromTabId = function
    | "research" -> Some Research
    | "non-research" -> Some NonResearch
    | _ -> None

type TabContent =
    | TableContent
    | ApplicationContent of CagApplicationId
    | DiscrepancyContent

type TabState = {
    Id: string
    Title: string
    Content: TabContent
}

type RegisterData = {
    Applications: RemoteData<CagApplication list>
    FrontPageEntries: RemoteData<CagFrontPageEntry list>
    Discrepancies: RemoteData<ApplicationDiscrepancy list>
    FileLoadResult: RemoteData<FileLoadResult option>
}

type Model = {
    Research: RegisterData
    NonResearch: RegisterData
    OpenTabs: TabState list
    ActiveTabId: string
}

type Msg =
    | LoadApplications of ApiCall<RegisterType, ApplicationsResponse>
    | LoadFrontPageEntries of ApiCall<RegisterType, FrontPageEntriesResponse>
    | LoadDiscrepancies of ApiCall<RegisterType, DiscrepanciesResponse>
    | LoadFileResult of ApiCall<RegisterType, FileLoadResultResponse>
    | LoadDisplayNames of ApiCall<RegisterType, ApplicationDisplayNameResponse>
    | OpenTab of TabContent
    | OpenAndFocusTab of TabContent
    | CloseTab of string
    | SetActiveTab of string
    | UpdatePathParams

let findQuery (segments: string list) : (string * string) list =
    segments
    |> List.choose (function
        | Route.Query parameters -> Some parameters
        | _ -> None)
    |> List.tryHead
    |> function
        | Some parameters -> parameters
        | None -> []

let applicationsApi =
    Api.makeProxy<ICagApplicationsApi> ()
let createShortTitle =
    function
    | TableContent -> "Applications (Research)"
    | DiscrepancyContent -> "Discrepancies"
    | ApplicationContent app ->
        let maxTitleLength = 20

        //    let shortTitle =
        //      if app.Title.Length > maxTitleLength then
        //          app.Title.Substring(0, maxTitleLength) + "..."
        //      else
         //       app.Title

        sprintf "%s: %s" app.ApplicationNumber app.ApplicationNumber
let init () =
    let queryParams = findQuery (Router.currentPath())
    let resAppIds = queryParams |> List.choose (fun (key, value) -> if key = "resAppId" then Some value else None)
    let nonResAppIds = queryParams |> List.choose (fun (key, value) -> if key = "nonResAppId" then Some value else None)

    printfn "Initializing with research app IDs: %A" resAppIds
    printfn "Initializing with non-research app IDs: %A" nonResAppIds

    let displayNameCmds = [
        if not (List.isEmpty resAppIds) then
            printfn "Will load research display names"
            LoadDisplayNames(Start(Research)) |> Cmd.ofMsg
        if not (List.isEmpty nonResAppIds) then
            printfn "Will load non-research display names"
            LoadDisplayNames(Start(NonResearch)) |> Cmd.ofMsg
    ]

    let resOpenTabCmds =
        resAppIds
        |> List.map (fun appId -> Cmd.ofMsg (OpenTab (ApplicationContent { ApplicationNumber = appId; RegisterType = Research })))
    let nonResOpenTabCmds =
        nonResAppIds
        |> List.map (fun appId -> Cmd.ofMsg (OpenTab (ApplicationContent { ApplicationNumber = appId; RegisterType = NonResearch })))

    let emptyRegisterData = {
        Applications = NotStarted
        FrontPageEntries = NotStarted
        Discrepancies = NotStarted
        FileLoadResult = NotStarted
    }

    let model = {
        Research = emptyRegisterData
        NonResearch = emptyRegisterData
        OpenTabs = [
            { Id = "research"
              Title = "Research"
              Content = TableContent }
            { Id = "non-research"
              Title = "Non-Research"
              Content = TableContent }
        ]
        ActiveTabId = "research"
    }
    let cmd = Cmd.batch [
        Cmd.batch resOpenTabCmds
        Cmd.batch nonResOpenTabCmds
        LoadApplications(Start(Research)) |> Cmd.ofMsg
        LoadApplications(Start(NonResearch)) |> Cmd.ofMsg
        LoadFileResult(Start(Research)) |> Cmd.ofMsg
        LoadFileResult(Start(NonResearch)) |> Cmd.ofMsg
        Cmd.batch displayNameCmds
    ]
    model, cmd


let update msg model =
    match msg with
    | LoadApplications msg ->
        match msg with
        | Start registerType ->
            let cmd = Cmd.batch [
                Cmd.OfAsync.perform applicationsApi.getApplications registerType (Finished >> LoadApplications)
                Cmd.ofMsg (LoadFileResult(Start(registerType)))
            ]
            let updateRegisterData data = { data with RegisterData.Applications = Loading }
            { model with
                Research = if registerType = Research then updateRegisterData model.Research else model.Research
                NonResearch = if registerType = NonResearch then updateRegisterData model.NonResearch else model.NonResearch }, cmd
        | Finished response ->
            let updateRegisterData data = { data with RegisterData.Applications = Loaded response.Applications }
            let updatedTabs =
                model.OpenTabs
                |> List.map (fun tab ->
                    match tab.Content with
                    | ApplicationContent appId when appId.RegisterType = response.RegisterType ->
                        match response.Applications |> List.tryFind (fun app -> app.ApplicationNumber.ApplicationNumber = appId.ApplicationNumber) with
                        | Some app ->
                            printfn "Updating tab %s with display name from loaded application" tab.Id
                            { tab with Title = getDisplayNameFromApplication app }
                        | None ->
                            printfn "No application found for tab %s" tab.Id
                            tab
                    | _ -> tab
                )
            let cmd = Cmd.ofMsg (LoadFileResult(Start(response.RegisterType)))
            { model with
                Research = if response.RegisterType = Research then updateRegisterData model.Research else model.Research
                NonResearch = if response.RegisterType = NonResearch then updateRegisterData model.NonResearch else model.NonResearch
                OpenTabs = updatedTabs }, cmd
    | LoadFrontPageEntries msg ->
        match msg with
        | Start registerType ->
            let cmd = Cmd.batch [
                Cmd.OfAsync.perform applicationsApi.getFrontPageEntries registerType (Finished >> LoadFrontPageEntries)
                Cmd.ofMsg (LoadFileResult(Start(registerType)))
            ]
            let updateRegisterData data = { data with FrontPageEntries = Loading }
            { model with
                Research = if registerType = Research then updateRegisterData model.Research else model.Research
                NonResearch = if registerType = NonResearch then updateRegisterData model.NonResearch else model.NonResearch }, cmd
        | Finished response ->
            let updateRegisterData data = { data with FrontPageEntries = Loaded response.Entries }
            { model with
                Research = if response.RegisterType = Research then updateRegisterData model.Research else model.Research
                NonResearch = if response.RegisterType = NonResearch then updateRegisterData model.NonResearch else model.NonResearch }, Cmd.none
    | LoadDiscrepancies msg ->
        match msg with
        | Start registerType ->
            let cmd = Cmd.batch [
                Cmd.OfAsync.perform applicationsApi.getDiscrepancies registerType (Finished >> LoadDiscrepancies)
                Cmd.ofMsg (LoadFileResult(Start(registerType)))
            ]
            let updateRegisterData data = { data with RegisterData.Discrepancies = Loading }
            { model with
                Research = if registerType = Research then updateRegisterData model.Research else model.Research
                NonResearch = if registerType = NonResearch then updateRegisterData model.NonResearch else model.NonResearch }, cmd
        | Finished discrepancies ->
            let updateRegisterData data = { data with RegisterData.Discrepancies = Loaded discrepancies.Discrepancies }
            { model with
                Research = if discrepancies.RegisterType = Research then updateRegisterData model.Research else model.Research
                NonResearch = if discrepancies.RegisterType = NonResearch then updateRegisterData model.NonResearch else model.NonResearch }, Cmd.none
    | LoadFileResult msg ->
        match msg with
        | Start registerType ->
            let cmd = Cmd.OfAsync.perform applicationsApi.getFileLoadResult registerType (Finished >> LoadFileResult)
            let updateRegisterData data = { data with RegisterData.FileLoadResult = Loading }
            { model with
                Research = if registerType = Research then updateRegisterData model.Research else model.Research
                NonResearch = if registerType = NonResearch then updateRegisterData model.NonResearch else model.NonResearch }, cmd
        | Finished result ->
            let updateRegisterData data = { data with RegisterData.FileLoadResult= Loaded result.FileLoadResult }
            { model with
                Research = if result.RegisterType = Research then updateRegisterData model.Research else model.Research
                NonResearch = if result.RegisterType = NonResearch then updateRegisterData model.NonResearch else model.NonResearch }, Cmd.none
    | LoadDisplayNames msg ->
        match msg with
        | Start registerType ->
            printfn "Loading display names for %A" registerType
            let cmd = Cmd.OfAsync.perform applicationsApi.getApplicationDisplayNames registerType (Finished >> LoadDisplayNames)
            model, cmd
        | Finished response ->
            printfn "Received display names for %A: %d names" response.RegisterType (Map.count response.ApplicationDisplayNames)
            let updatedTabs =
                model.OpenTabs
                |> List.map (fun tab ->
                    match tab.Content with
                    | ApplicationContent app when app.RegisterType = response.RegisterType ->
                        match Map.tryFind app.ApplicationNumber response.ApplicationDisplayNames with
                        | Some displayName ->
                            printfn "Updating tab %s with display name: %s" tab.Id displayName
                            { tab with Title = displayName }
                        | None ->
                            printfn "No display name found for application %s" app.ApplicationNumber
                            tab
                    | _ -> tab
                )
            { model with OpenTabs = updatedTabs }, Cmd.none
    | OpenAndFocusTab content ->
        let (id, title) =
            match content with
            | TableContent ->
                match getRegisterTypeFromTabId model.ActiveTabId with
                | Some Research -> "research", "Research Applications"
                | Some NonResearch -> "non-research", "Non-Research Applications"
                | None -> "research", "Research Applications"
            | ApplicationContent app ->
                // Try to get application from loaded data first
                let registerData = if app.RegisterType = Research then model.Research else model.NonResearch
                match registerData.Applications with
                | Loaded apps ->
                    match apps |> List.tryFind (fun a -> a.ApplicationNumber.ApplicationNumber = app.ApplicationNumber) with
                    | Some loadedApp -> app.ApplicationNumber, getDisplayNameFromApplication loadedApp
                    | None -> app.ApplicationNumber, sprintf "Loading %s..." app.ApplicationNumber
                | _ -> app.ApplicationNumber, sprintf "Loading %s..." app.ApplicationNumber
            | DiscrepancyContent -> "discrepancies", "Discrepancies"

        let existingTab = model.OpenTabs |> List.tryFind (fun t -> t.Id = id)
        match existingTab with
        | Some _ ->
            { model with ActiveTabId = id }, Cmd.none
        | None ->
            let newTab = {
                Id = id
                Title = title
                Content = content
            }
            let cmd =
                match content with
                | ApplicationContent app ->
                    Cmd.batch [
                        Cmd.ofMsg (LoadDisplayNames(Start(app.RegisterType)))
                        Cmd.ofMsg UpdatePathParams
                    ]
                | DiscrepancyContent ->
                    let registerType =
                        match getRegisterTypeFromTabId model.ActiveTabId with
                        | Some rt -> rt
                        | None -> Research
                    Cmd.ofMsg (LoadDiscrepancies(Start(registerType)))
                | _ -> Cmd.ofMsg UpdatePathParams
            { model with OpenTabs = model.OpenTabs @ [newTab]; ActiveTabId = id }, cmd

    | OpenTab content ->
        let (id, title) =
            match content with
            | TableContent ->
                match getRegisterTypeFromTabId model.ActiveTabId with
                | Some Research -> "research", "Research Applications"
                | Some NonResearch -> "non-research", "Non-Research Applications"
                | None -> "research", "Research Applications"
            | ApplicationContent app ->
                // Try to get application from loaded data first
                let registerData = if app.RegisterType = Research then model.Research else model.NonResearch
                match registerData.Applications with
                | Loaded apps ->
                    match apps |> List.tryFind (fun a -> a.ApplicationNumber.ApplicationNumber = app.ApplicationNumber) with
                    | Some loadedApp -> app.ApplicationNumber, getDisplayNameFromApplication loadedApp
                    | None -> app.ApplicationNumber, sprintf "Loading %s..." app.ApplicationNumber
                | _ -> app.ApplicationNumber, sprintf "Loading %s..." app.ApplicationNumber
            | DiscrepancyContent -> "discrepancies", "Discrepancies"

        let existingTab = model.OpenTabs |> List.tryFind (fun t -> t.Id = id)
        match existingTab with
        | Some _ ->
            model, Cmd.none
        | None ->
            let newTab = {
                Id = id
                Title = title
                Content = content
            }
            let cmd =
                match content with
                | ApplicationContent app ->
                    Cmd.batch [
                        Cmd.ofMsg (LoadDisplayNames(Start(app.RegisterType)))
                        Cmd.ofMsg UpdatePathParams
                    ]
                | DiscrepancyContent ->
                    let registerType =
                        match getRegisterTypeFromTabId model.ActiveTabId with
                        | Some rt -> rt
                        | None -> Research
                    Cmd.ofMsg (LoadDiscrepancies(Start(registerType)))
                | _ -> Cmd.ofMsg UpdatePathParams
            { model with OpenTabs = model.OpenTabs @ [newTab] }, cmd
    | CloseTab id ->
        if id = "research" || id = "non-research" then
            model, Cmd.none // Can't close main tabs
        else
            let newTabs = model.OpenTabs |> List.filter (fun t -> t.Id <> id)
            { model with
                OpenTabs = newTabs
                ActiveTabId =
                    if model.ActiveTabId = id then
                        "research"
                    else model.ActiveTabId
            }, Cmd.ofMsg UpdatePathParams
    | UpdatePathParams ->
        // Get application tabs
        let url =
            model.OpenTabs |> List.map _.Content |> List.choose (function
            | ApplicationContent app -> Some app
            | _ -> None)
            |> List.distinct
            // research to resAppId, non-research to nonResAppId
            |> List.map (fun app -> sprintf "%sAppId=%s" (if app.RegisterType = Research then "res" else "nonRes") app.ApplicationNumber)
            |> String.concat "&"
            |> (fun x -> [$"?{x}"])
        Router.nav url HistoryMode.PushState RouteMode.Path
        model, Cmd.none
    | SetActiveTab tabId ->
        { model with ActiveTabId = tabId }, Cmd.none
module ViewComponents =
    let navigationBreadcrumb (model: Model) dispatch =
        Html.div [
            prop.className "text-sm breadcrumbs p-4 bg-white/80 rounded-md shadow-md mb-4"
            prop.children [
                Html.ul [
                    Html.li [
                        Html.a [
                            prop.className (
                                if model.ActiveTabId = "research" || model.ActiveTabId = "non-research"
                                then "font-bold"
                                else ""
                            )
                            prop.onClick (fun _ -> dispatch (SetActiveTab "research"))
                            prop.text "Applications"
                        ]
                    ]
                    // Show current application tab if one is active
                    match model.OpenTabs |> List.tryFind (fun t -> t.Id = model.ActiveTabId) with
                    | Some tab when tab.Id <> "research" && tab.Id <> "non-research" ->
                        Html.li [
                            Html.a [
                                prop.className "font-bold"
                                prop.text tab.Title
                            ]
                        ]
                    | _ -> ()
                ]
            ]
        ]

    [<ReactComponent>]
    let applicationTable (apps: CagApplication list) (registerType: RegisterType) (isVisible: bool) dispatch =
        // Memoize column definitions
        let columnDefs = React.useMemo((fun () -> [
            ColumnDef.create [
                ColumnDef.headerName ""
                ColumnDef.width 60
                ColumnDef.valueGetter (fun (x : CagApplication) -> x)
                ColumnDef.cellRenderer (fun cellParams ->
                    match cellParams.data with
                    | Some app ->
                        Html.div [
                            prop.className "flex gap-2"
                            prop.children [
                                Html.button [
                                    prop.className "btn btn-sm btn-outline"
                                    prop.onClick (fun _ -> dispatch (OpenAndFocusTab (ApplicationContent app.ApplicationNumber)))
                                    prop.children [
                                        Html.i [
                                            prop.className "fas fa-eye"
                                            prop.title "Open and focus"
                                        ]
                                    ]
                                ]
                                Html.button [
                                    prop.className "btn btn-sm btn-primary"
                                    prop.onClick (fun _ -> dispatch (OpenTab(ApplicationContent app.ApplicationNumber)))
                                    prop.children [
                                        Html.i [
                                            prop.className "fas fa-plus"
                                            prop.title "Open in background"
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    | None -> Html.none
                )
            ]
            ColumnDef.create [
                ColumnDef.filter RowFilter.Text
                ColumnDef.headerName "CAG Reference"
                ColumnDef.width 165
                ColumnDef.valueGetter (fun (x : CagApplication) -> x.Reference)
            ]
            ColumnDef.create [
                ColumnDef.filter RowFilter.Text
                ColumnDef.headerName "App #"
                ColumnDef.width 90
                ColumnDef.valueGetter (fun x -> x.ApplicationNumber.ApplicationNumber)
                if registerType = NonResearch then
                    columnDefProp("sort", "desc")
            ]
            ColumnDef.create [
                ColumnDef.filter RowFilter.Text
                ColumnDef.headerName "Title"
                ColumnDef.width 250
                ColumnDef.valueGetter (fun x -> x.Title)
                columnDefProp("tooltipField", "Title")
            ]
            ColumnDef.create [
                ColumnDef.filter RowFilter.Text
                ColumnDef.headerName "Organisation"
                ColumnDef.width 200
                ColumnDef.valueGetter (fun x -> x.ApplicantOrganisation)
                columnDefProp("tooltipField", "ApplicantOrganisation")
            ]
            ColumnDef.create [
                ColumnDef.filter RowFilter.Text
                ColumnDef.headerName "Status"
                ColumnDef.width 150
                ColumnDef.valueGetter (fun (x : CagApplication) -> x.Status)
                ColumnDef.cellRenderer (fun cellParams ->
                    match cellParams.data with
                    | Some app ->
                        Html.div [
                            prop.className [
                                match (string app.Status).ToLower() with
                                | s when s.Contains("approved") -> "badge badge-success"
                                | s when s.Contains("pending") -> "badge badge-warning"
                                | s when s.Contains("rejected") -> "badge badge-error"
                                | _ -> "badge"
                            ]
                            prop.children [
                                Html.span [
                                    prop.text (string app.Status)
                                ]
                                if app.ApplicationStatus = Obsolete then
                                    Html.span [
                                        prop.className "ml-2 text-xs"
                                        prop.text "(Hidden)"
                                    ]
                            ]
                        ]
                    | None -> Html.none
                )
            ]
            if registerType = Research then
                ColumnDef.create [
                    ColumnDef.filter RowFilter.Date
                    ColumnDef.headerName "Outcome Date"
                    ColumnDef.width 120
                    ColumnDef.valueGetter (fun (x : CagApplication) -> x.OutcomeDate)
                    ColumnDef.valueFormatter (fun valueParams ->
                        match Option.flatten valueParams.value with
                        | Some date -> date.Format("yyyy-MM-dd")
                        | None -> "")
                    columnDefProp("sort", "desc")
                ]
            ColumnDef.create [
                ColumnDef.filter RowFilter.Date
                ColumnDef.headerName "Next Review Date"
                ColumnDef.width 120
                ColumnDef.valueFormatter (fun valueParams ->
                    match Option.flatten valueParams.value with
                    | Some (date : DateTime) -> date.Format("yyyy-MM-dd")
                    | None -> "")
                ColumnDef.valueGetter (fun (x : CagApplication) -> x.NextReviewDate)
            ]

        ]), [| |]) // Empty dependencies array since these don't depend on props

        // Memoize row class rules
        let rowClassRules = React.useMemo((fun () ->
            {| ``opacity-50`` = (fun (rowParams : ICellRendererParams<_, _>) ->
                match rowParams.data with
                | Some app when app.ApplicationStatus = Obsolete -> true
                | _ -> false
            ) |}), [| |])

        Html.div [
            prop.className [
                "bg-white/80 rounded-md shadow-md p-4 ag-theme-alpine overflow-hidden"
                if not isVisible then "hidden"
            ]
            prop.children [
                Html.div [
                    prop.className "flex justify-end mb-4"
                    prop.children [
                        Html.button [
                            prop.className "btn btn-primary"
                            prop.onClick (fun _ -> dispatch (OpenAndFocusTab DiscrepancyContent))
                            prop.text "View Discrepancies"
                        ]
                    ]
                ]
                AgGrid.grid [
                    agGridProp("key", sprintf "applications-%A" registerType)
                    agGridProp("gridId", sprintf "applications-%A" registerType)
                    agGridProp("id", sprintf "applications-%A" registerType)
                    AgGrid.rowData (apps |> Array.ofList)
                    AgGrid.defaultColDef [
                        ColumnDef.resizable true
                        ColumnDef.sortable true
                        ColumnDef.filter RowFilter.Text
                    ]
                    agGridProp("rowClassRules", rowClassRules)
                    AgGrid.columnDefs columnDefs
                    AgGrid.pagination true
                    AgGrid.paginationPageSize 10
                    AgGrid.domLayout DOMLayout.AutoHeight
                    agGridProp("tooltipShowDelay", 200)
                ]
            ]
        ]

    let replaceNewlinesWithBr (text: string) =
        let mergeWhitespace (text: string) =
            String.Join(" ", text.Split([|' '|], StringSplitOptions.RemoveEmptyEntries))
        let removeWhiteSpaceOnlyLines (text: string) =
            text.Split([|'\n'|], StringSplitOptions.RemoveEmptyEntries) |> Array.map mergeWhitespace |> String.concat "\n"
        let rec replaceMultipleNewlines (text: string) =
            let pattern = String.replicate 3 "\n"
            let newText = text.Replace(pattern, "\n")
            if newText = text then text
            else replaceMultipleNewlines newText
        text |> removeWhiteSpaceOnlyLines |> replaceMultipleNewlines |> (fun t -> t.Replace("\n", "<br />"))

    let applicationDetail (app: CagApplication) dispatch =
        Html.div [
            prop.className [
                "bg-white/80 rounded-md shadow-md p-6 animate-fadeIn mb-16 max-w-7xl mx-auto"
                if app.ApplicationStatus = Obsolete then "opacity-50"
            ]
            prop.children [
                // Header Section
                Html.div [
                    prop.className "mb-8 border-b pb-4"
                    prop.children [
                        Html.div [
                            prop.className "flex justify-between items-center"
                            prop.children [
                                Html.h2 [
                                    prop.className "text-3xl font-bold break-words mb-4"
                                    prop.text app.Title
                                ]
                                if app.ApplicationStatus = Obsolete then
                                    Html.div [
                                        prop.className "badge badge-warning"
                                        prop.text "Obsolete Record"
                                    ]
                            ]
                        ]
                        Html.div [
                            prop.className "grid grid-cols-2 md:grid-cols-4 gap-4"
                            prop.children [
                                Html.div [
                                    Html.strong "App #: "
                                    Html.span app.ApplicationNumber.ApplicationNumber
                                ]
                                Html.div [
                                    Html.strong "Reference: "
                                    Html.span app.Reference
                                ]
                                Html.div [
                                    Html.strong "Status: "
                                    Html.div [
                                        prop.className (
                                            match (string app.Status).ToLower() with
                                            | s when s.Contains("approved") -> "badge badge-success"
                                            | s when s.Contains("pending") -> "badge badge-warning"
                                            | s when s.Contains("rejected") -> "badge badge-error"
                                            | _ -> "badge"
                                        )
                                        prop.text (string app.Status)
                                    ]
                                ]
                                Html.div [
                                    Html.strong "Sponsor: "
                                    Html.span app.Sponsor
                                ]
                            ]
                        ]
                    ]
                ]

                // Main content grid
                Html.div [
                    prop.className "grid md:grid-cols-2 gap-8"
                    prop.children [
                        // Contact Information Section
                        Html.div [
                            prop.className "bg-gray-50 p-4 rounded-lg"
                            prop.children [
                                Html.h3 [
                                    prop.className "text-xl font-semibold mb-4"
                                    prop.text "Contact Information"
                                ]
                                Html.div [
                                    prop.className "grid gap-3"
                                    prop.children [
                                        Html.div [
                                            Html.strong "Organisation: "
                                            Html.span app.ApplicantOrganisation
                                        ]
                                        Html.div [
                                            Html.strong "Contact Name: "
                                            Html.span app.ContactName
                                        ]
                                        Html.div [
                                            Html.strong "Email: "
                                            Html.span app.Email
                                        ]
                                        Html.div [
                                            Html.strong "Telephone: "
                                            Html.span app.Telephone
                                        ]
                                        Html.div [
                                            Html.strong "Address: "
                                            Html.span (String.concat ", " app.Address)
                                        ]
                                        Html.div [
                                            Html.strong "Postcode: "
                                            Html.span app.Postcode
                                        ]
                                    ]
                                ]
                            ]
                        ]

                        // Dates and References Section
                        Html.div [
                            prop.className "bg-gray-50 p-4 rounded-lg"
                            prop.children [
                                Html.h3 [
                                    prop.className "text-xl font-semibold mb-4"
                                    prop.text "Dates & References"
                                ]
                                Html.div [
                                    prop.className "grid gap-3"
                                    prop.children [
                                        Html.div [
                                            Html.strong "Outcome Date: "
                                            Html.span (Option.defaultValue "-" (app.OutcomeDate |> Option.map (_.Format("yyyy-MM-dd"))))
                                        ]
                                        Html.div [
                                            Html.strong "Next Review Date: "
                                            Html.span (Option.defaultValue "-" (app.NextReviewDate |> Option.map (_.Format("yyyy-MM-dd"))))
                                        ]
                                        Html.div [
                                            Html.strong "Other References: "
                                            Html.span (Option.defaultValue "-" app.OtherRefs)
                                        ]
                                        Html.div [
                                            Html.strong "NDOO: "
                                            Html.span (Option.defaultValue "-" app.NDOO)
                                        ]
                                        Html.div [
                                            Html.strong "English CPI: "
                                            Html.span (Option.defaultValue "-" (Option.map string app.EnglishCPI))
                                        ]
                                        Html.div [
                                            Html.strong "Welsh CPI: "
                                            Html.span (Option.defaultValue "-" (Option.map string app.WelshCPI))
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]

                // Purpose and Classifications Section
                Html.div [
                    prop.className "mt-8 grid md:grid-cols-2 gap-8"
                    prop.children [
                        // Medical Purposes
                        Html.div [
                            prop.className "bg-gray-50 p-4 rounded-lg"
                            prop.children [
                                Html.h3 [
                                    prop.className "text-xl font-semibold mb-4"
                                    prop.text "Medical Purposes"
                                ]
                                Html.div [
                                    let allMedicalPurposes = [
                                        MedicalPurpose.MedicalResearch
                                        MedicalPurpose.PreventativeMedicine
                                        MedicalPurpose.MedicalDiagnosis
                                        MedicalPurpose.CareAndTreatment
                                        MedicalPurpose.HealthAndSocialCareManagement
                                        MedicalPurpose.InformingIndividuals
                                    ]
                                    for purpose in allMedicalPurposes do
                                        Html.label [
                                            prop.className "flex items-center gap-2 mb-2"
                                            prop.children [
                                                Html.input [
                                                    prop.type' "checkbox"
                                                    prop.isChecked (app.MedicalPurposes.Contains purpose)
                                                    prop.disabled true
                                                ]
                                                Html.span (string purpose)
                                            ]
                                        ]
                                ]
                            ]
                        ]

                        // S251 Classes
                        Html.div [
                            prop.className "bg-gray-50 p-4 rounded-lg"
                            prop.children [
                                Html.h3 [
                                    prop.className "text-xl font-semibold mb-4"
                                    prop.text "S251 Classes"
                                ]
                                Html.div [
                                    let allS251Classes = [
                                        S251Class.SpecificSupport
                                        S251Class.ClassI_Identifiability
                                        S251Class.ClassII_GeographicalLocation
                                        S251Class.ClassIII_IdentifyAndContact
                                        S251Class.ClassIV_LinkingMultipleSources
                                        S251Class.ClassV_AuditAndMonitoring
                                        S251Class.ClassVI_GrantingAccess
                                    ]
                                    for s251Class in allS251Classes do
                                        Html.label [
                                            prop.className "flex items-center gap-2 mb-2"
                                            prop.children [
                                                Html.input [
                                                    prop.type' "checkbox"
                                                    prop.isChecked (app.S251Classes.Contains s251Class)
                                                    prop.disabled true
                                                ]
                                                Html.span (string s251Class)
                                            ]
                                        ]
                                ]
                            ]
                        ]
                    ]
                ]

                // Detailed Information Section
                Html.div [
                    prop.className "mt-8 grid gap-8"
                    prop.children [
                        Html.div [
                            prop.className "bg-gray-50 p-4 rounded-lg"
                            prop.children [
                                Html.h3 [
                                    prop.className "text-xl font-semibold mb-4"
                                    prop.text "Detailed Information"
                                ]
                                Html.div [
                                    prop.className "grid gap-6"
                                    prop.children [
                                        Html.div [
                                            Html.strong "Cohort Description"
                                            Html.div [
                                                prop.className "mt-2 prose max-w-none"
                                                prop.innerHtml (replaceNewlinesWithBr app.CohortDescription)
                                            ]
                                        ]
                                        Html.div [
                                            Html.strong "Summary"
                                            Html.div [
                                                prop.className "mt-2 prose max-w-none"
                                                prop.innerHtml (replaceNewlinesWithBr app.Summary)
                                            ]
                                        ]
                                        Html.div [
                                            Html.strong "Confidential Information"
                                            Html.div [
                                                prop.className "mt-2 prose max-w-none"
                                                prop.innerHtml (replaceNewlinesWithBr app.ConfidentialInfo)
                                            ]
                                        ]

                                        Html.div [
                                            Html.strong "Notes"
                                            Html.div [
                                                prop.className "mt-2 prose max-w-none"
                                                prop.innerHtml (replaceNewlinesWithBr app.Notes)
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        ]

                        // Minutes Section
                        Html.div [
                            prop.className "mt-8"
                            prop.children [
                                Html.div [
                                    prop.className "bg-gray-50 p-4 rounded-lg"
                                    prop.children [
                                        Html.h3 [
                                            prop.className "text-xl font-semibold mb-4"
                                            prop.text "Meeting Minutes References"
                                        ]
                                        match app.RelatedMinutes with
                                        | [] ->
                                            Html.div [
                                                prop.className "text-gray-500 italic"
                                                prop.text "No minutes references found"
                                            ]
                                        | minutes ->
                                            Html.div [
                                                prop.className "grid gap-4"
                                                prop.children [
                                                    for minute in minutes ->
                                                        Html.div [
                                                            prop.className "border-b pb-2"
                                                            prop.children [
                                                                Html.div [
                                                                    prop.className "flex justify-between items-start"
                                                                    prop.children [
                                                                        Html.a [
                                                                            prop.className "text-blue-600 hover:text-blue-800"
                                                                            prop.href (if minute.PageRanges.Length > 0 then $"{minute.Url}#page={minute.PageRanges.[0]}" else minute.Url)
                                                                            prop.target "_blank"
                                                                            prop.text minute.Title
                                                                        ]
                                                                        Html.span [
                                                                            prop.className "text-sm text-gray-500"
                                                                            prop.text (minute.ProcessedDate.ToString("yyyy-MM-dd"))
                                                                        ]
                                                                    ]
                                                                ]
                                                                Html.div [
                                                                    prop.className "text-sm text-gray-600 mt-1"
                                                                    prop.text (sprintf "Pages: %s" (String.concat ", " minute.PageRanges))
                                                                ]
                                                            ]
                                                        ]
                                                ]
                                            ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]

    let discrepancyTable (discrepancies: ApplicationDiscrepancy list) dispatch =
        Html.div [
            prop.className "bg-white/80 rounded-md shadow-md p-4 mb-16 overflow-x-auto max-w-5xl mx-auto"
            prop.children [
                Html.table [
                    prop.className "table w-full"
                    prop.children [
                        Html.thead [
                            Html.tr [
                                Html.th "App #"
                                Html.th "Field"
                                Html.th "Front Page Value"
                                Html.th "Detail Page Value"
                            ]
                        ]
                        Html.tbody [
                            for disc in discrepancies do
                                let addRow (fieldName: string) (frontValue: string) (detailValue: string) =
                                    Html.tr [
                                        Html.td disc.ApplicationNumber.ApplicationNumber
                                        Html.td fieldName
                                        Html.td (Html.div [ prop.innerHtml (replaceNewlinesWithBr frontValue) ])
                                        Html.td (Html.div [ prop.innerHtml (replaceNewlinesWithBr detailValue) ])
                                    ]

                                if disc.Differences.Title then
                                    addRow "Title" disc.FrontPage.Title disc.Detail.Title
                                if disc.Differences.Status then
                                    addRow "Status" disc.FrontPage.Status disc.Detail.Status
                                if disc.Differences.Contact then
                                    addRow "Contact" disc.FrontPage.Contact disc.Detail.ContactName
                                if disc.Differences.Organisation then
                                    addRow "Organisation" disc.FrontPage.Organisation disc.Detail.ApplicantOrganisation
                                if disc.Differences.NationalDataOptOutStatus then
                                    addRow "NDOO"
                                        (Option.defaultValue "None" disc.FrontPage.NationalDataOptOutStatus)
                                        (Option.defaultValue "None" disc.Detail.NDOO)
                                // ... add other differences as needed
                        ]
                    ]
                ]
            ]
        ]

    let renderTab (tab: TabState) activeTabId dispatch =
        Html.div [
            prop.className [
                "flex items-center px-4 py-2 rounded-t-lg cursor-pointer transition-colors"
                if tab.Id = activeTabId then
                    "bg-blue-500 text-white"
                else "bg-gray-100 hover:bg-gray-200"
            ]
            prop.children [
                Html.span [
                    prop.className "mr-2"
                    prop.text tab.Title
                ]
                if tab.Id <> "research" && tab.Id <> "non-research" then  // Only show close button for other tabs
                    Html.button [
                        prop.className "hover:text-red-500 ml-2"
                        prop.onClick (fun e ->
                            e.stopPropagation()
                            dispatch (CloseTab tab.Id)
                        )
                        prop.children [
                            Html.i [
                                prop.className "fas fa-times"
                            ]
                        ]
                    ]
            ]
            prop.onClick (fun _ -> dispatch (SetActiveTab tab.Id))
        ]

    let tabBar (tabs: TabState list) activeTabId dispatch =
        Html.div [
            prop.className "fixed bottom-0 left-0 right-0 bg-white shadow-lg border-t border-gray-200"
            prop.children [
                Html.div [
                    prop.className "flex space-x-2 p-2"
                    prop.children [
                        for tab in tabs -> renderTab tab activeTabId dispatch
                        if List.length (List.filter (fun t ->
                            t.Id <> "research" && t.Id <> "non-research") tabs) > 2 then
                            yield Html.button [
                                prop.className "btn btn-danger ml-2"
                                prop.onClick (fun _ ->
                                    // Dispatch a message to close all tabs except main tabs
                                    tabs
                                    |> List.iter (fun t ->
                                        if t.Id <> "research" && t.Id <> "non-research" then
                                            dispatch (CloseTab t.Id)
                                    )
                                )
                                prop.text "Close All Tabs"
                            ]
                    ]
                ]
            ]
        ]

    let fileLoadInfo (fileLoadResult: RemoteData<FileLoadResult option>) (registerType: RegisterType) =
        match fileLoadResult with
        | Loaded (Some result) when result.RegisterType = registerType ->
            Html.div [
                prop.className "bg-white/80 rounded-md shadow-md p-4 mb-4"
                prop.children [
                    Html.div [
                        prop.className "flex flex-col gap-2"
                        prop.children [
                            Html.div [
                                Html.strong "Current file: "
                                Html.span result.LoadedFile
                            ]
                            Html.div [
                                Html.strong "Last modified: "
                                Html.span (result.LoadedDate.ToString("yyyy-MM-dd HH:mm:ss"))
                            ]
                            if not (List.isEmpty result.FailedFiles) then
                                Html.div [
                                    Html.strong "Failed attempts: "
                                    Html.ul [
                                        prop.className "list-disc list-inside"
                                        prop.children [
                                            for file in result.FailedFiles ->
                                                Html.li file
                                        ]
                                    ]
                                ]
                        ]
                    ]
                ]
            ]
        | _ -> Html.none

[<ReactComponent>]
let TableTab (registerData: RegisterData) (registerType: RegisterType) (visible: bool) dispatch =
    match registerData.Applications with
    | NotStarted ->
        dispatch (LoadApplications(Start(registerType)))
        dispatch (LoadFileResult(Start(registerType)))
        Html.div [
            prop.className "animate-pulse bg-white/80 rounded-md shadow-md p-8 text-center"
            prop.text "Loading..."
        ]
    | Loading ->
        Html.div [
            prop.className "animate-pulse bg-white/80 rounded-md shadow-md p-8 text-center"
            prop.text "Loading applications..."
        ]
    | Loaded apps ->
        ViewComponents.applicationTable apps registerType visible dispatch

[<ReactComponent>]
let ApplicationTab (app: CagApplication) dispatch =
    ViewComponents.applicationDetail app dispatch

[<ReactComponent>]
let DiscrepancyTab (model: Model) dispatch =
    let registerType =
        match getRegisterTypeFromTabId model.ActiveTabId with
        | Some rt -> rt
        | None -> Research // Default to research if no tab selected

    let registerData =
        match registerType with
        | Research -> model.Research
        | NonResearch -> model.NonResearch

    match registerData.Discrepancies with
    | NotStarted ->
        dispatch (LoadDiscrepancies(Start(registerType)))
        dispatch (LoadFileResult(Start(registerType)))
        Html.div "Loading discrepancies..."
    | Loading ->
        Html.div "Loading discrepancies..."
    | Loaded discrepancies ->
        ViewComponents.discrepancyTable discrepancies dispatch

let view model dispatch =
    Html.section [
        prop.className "min-h-screen bg-gradient-to-br from-blue-50 to-indigo-100 relative overflow-x-hidden"
        prop.children [
            Html.div [
                prop.className "container mx-auto px-4 py-4 pb-20 max-w-7xl"
                prop.children [
                    Html.div [
                        prop.className "flex items-center justify-between mb-6"
                        prop.children [
                            Html.div [
                                prop.className "flex flex-col items-start"
                                prop.children [
                                    Html.h1 [
                                        prop.className "text-2xl font-light tracking-wide text-gray-800"
                                        prop.children [
                                            Html.span [
                                                prop.className "font-normal"
                                                prop.text "CAG "
                                            ]
                                            Html.span [
                                                prop.className "font-extralight"
                                                prop.text "Register"
                                            ]
                                            Html.div [
                                                prop.className "text-gray-500 text-xs tracking-wider uppercase"
                                                prop.text "Unofficial Viewer"
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                            match getRegisterTypeFromTabId model.ActiveTabId with
                            | Some Research -> ViewComponents.fileLoadInfo model.Research.FileLoadResult Research
                            | Some NonResearch -> ViewComponents.fileLoadInfo model.NonResearch.FileLoadResult NonResearch
                            | None -> Html.none
                        ]
                    ]

                    // Always render both tables
                    match model.Research.Applications, getRegisterTypeFromTabId model.ActiveTabId = Some Research with
                    | NotStarted, true ->
                        Html.div [
                            prop.className "animate-pulse bg-white/80 rounded-md shadow-md p-8 text-center"
                            prop.text "Loading..."
                        ]
                    | Loading, true ->
                        Html.div [
                            prop.className "animate-pulse bg-white/80 rounded-md shadow-md p-8 text-center"
                            prop.text "Loading research applications..."
                        ]
                    | Loaded researchApps, visible ->
                        TableTab model.Research Research visible dispatch
                    | _ -> Html.none
                    match model.NonResearch.Applications, getRegisterTypeFromTabId model.ActiveTabId = Some NonResearch with
                    | NotStarted, true ->
                        Html.div [
                            prop.className "animate-pulse bg-white/80 rounded-md shadow-md p-8 text-center"
                            prop.text "Loading..."
                        ]
                    | Loading, true ->
                        Html.div [
                            prop.className "animate-pulse bg-white/80 rounded-md shadow-md p-8 text-center"
                            prop.text "Loading non-research applications..."
                        ]
                    | Loaded nonResearchApps, visible ->
                        TableTab model.NonResearch NonResearch visible dispatch
                    | _ -> Html.none

                    // Render other active tab content if not showing tables
                    let activeTab = model.OpenTabs |> List.find (fun t -> t.Id = model.ActiveTabId)
                    match activeTab.Content with
                    | TableContent -> Html.none // Tables are already rendered above
                    | ApplicationContent app ->
                        match app.RegisterType, model.Research.Applications, model.NonResearch.Applications with
                        | Research, Loaded researchApps, _ ->
                            match researchApps |> List.tryFind (fun a -> a.ApplicationNumber.ApplicationNumber = app.ApplicationNumber) with
                            | Some app -> ApplicationTab app dispatch
                            | None -> Html.none
                        | NonResearch, _, Loaded nonResearchApps ->
                            match nonResearchApps |> List.tryFind (fun a -> a.ApplicationNumber.ApplicationNumber = app.ApplicationNumber) with
                            | Some app -> ApplicationTab app dispatch
                            | None -> Html.none
                        | _ -> Html.none
                    | DiscrepancyContent -> DiscrepancyTab model dispatch
                ]
            ]

            // Tab bar
            ViewComponents.tabBar model.OpenTabs model.ActiveTabId dispatch
        ]
    ]