module Index

open Elmish
open SAFE
open Shared
open Feliz
open Feliz.AgGrid

type View =
    | TableView
    | DiscrepancyView

type TabState = {
    Application: CagApplication
    IsMinimized: bool
}

type Model = {
    Applications: RemoteData<CagApplication list>
    FrontPageEntries: RemoteData<CagFrontPageEntry list>
    Discrepancies: RemoteData<ApplicationDiscrepancy list>
    OpenTabs: TabState list
    ActiveTabId: string option
    CurrentView: View
}

type Msg =
    | LoadApplications of ApiCall<unit, CagApplication list>
    | LoadFrontPageEntries of ApiCall<unit, CagFrontPageEntry list>
    | LoadDiscrepancies of ApiCall<unit, ApplicationDiscrepancy list>
    | OpenTab of CagApplication
    | CloseTab of string
    | SetActiveTab of string option
    | ToggleTabMinimize of string
    | SetView of View

let applicationsApi =
    Api.makeProxy<ICagApplicationsApi> ()
let init () =
    let model = {
        Applications = NotStarted
        FrontPageEntries = NotStarted
        Discrepancies = NotStarted
        OpenTabs = []
        ActiveTabId = None
        CurrentView = TableView
    }
    let cmd = LoadApplications(Start()) |> Cmd.ofMsg
    model, cmd

let update msg model =
    match msg with
    | LoadApplications msg ->
        match msg with
        | Start () ->
            let cmd = Cmd.OfAsync.perform applicationsApi.getApplications () (Finished >> LoadApplications)
            { model with Applications = Loading }, cmd
        | Finished apps ->
            { model with Applications = Loaded apps }, Cmd.none
    | LoadFrontPageEntries msg ->
        match msg with
        | Start () ->
            let cmd = Cmd.OfAsync.perform applicationsApi.getFrontPageEntries () (Finished >> LoadFrontPageEntries)
            { model with FrontPageEntries = Loading }, cmd
        | Finished frontPageEntries ->
            { model with FrontPageEntries = Loaded frontPageEntries }, Cmd.none
    | LoadDiscrepancies msg ->
        match msg with
        | Start () ->
            let cmd = Cmd.OfAsync.perform applicationsApi.getDiscrepancies () (Finished >> LoadDiscrepancies)
            { model with Discrepancies = Loading }, cmd
        | Finished discrepancies ->
            { model with Discrepancies = Loaded discrepancies }, Cmd.none
    | OpenTab app ->
        let existingTab = model.OpenTabs |> List.tryFind (fun t -> t.Application.ApplicationNumber = app.ApplicationNumber)
        match existingTab with
        | Some _ ->
            { model with ActiveTabId = Some app.ApplicationNumber }, Cmd.none
        | None ->
            { model with
                OpenTabs = model.OpenTabs @ [{ Application = app; IsMinimized = false }]
                ActiveTabId = Some app.ApplicationNumber
            }, Cmd.none
    | CloseTab appNumber ->
        let newTabs = model.OpenTabs |> List.filter (fun t -> t.Application.ApplicationNumber <> appNumber)
        let newActiveTab =
            if model.ActiveTabId = Some appNumber then
                newTabs |> List.tryLast |> Option.map (fun t -> t.Application.ApplicationNumber)
            else model.ActiveTabId
        { model with
            OpenTabs = newTabs
            ActiveTabId = newActiveTab
        }, Cmd.none
    | SetActiveTab tabId ->
        { model with ActiveTabId = tabId }, Cmd.none
    | ToggleTabMinimize appNumber ->
        let newTabs =
            model.OpenTabs
            |> List.map (fun tab ->
                if tab.Application.ApplicationNumber = appNumber then
                    { tab with IsMinimized = not tab.IsMinimized }
                else tab
            )
        { model with OpenTabs = newTabs }, Cmd.none
    | SetView view ->
        { model with
            CurrentView = view
            ActiveTabId = if view = TableView then None else model.ActiveTabId
        }, Cmd.none

module ViewComponents =
    let navigationBreadcrumb model dispatch =
        Html.div [
            prop.className "text-sm breadcrumbs p-4 bg-white/80 rounded-md shadow-md mb-4"
            prop.children [
                Html.ul [
                    Html.li [
                        Html.a [
                            prop.className (if model.CurrentView = TableView then "font-bold" else "")
                            prop.onClick (fun _ -> dispatch (SetView TableView))
                            prop.text "Applications"
                        ]
                    ]
                    Html.li [
                        Html.a [
                            prop.className (if model.CurrentView = DiscrepancyView then "font-bold" else "")
                            prop.onClick (fun _ ->
                                dispatch (SetView DiscrepancyView)
                                if model.Discrepancies = NotStarted then
                                    dispatch (LoadDiscrepancies(Start()))
                            )
                            prop.text "Discrepancies"
                        ]
                    ]
                    match model.ActiveTabId with
                    | Some activeId ->
                        match model.OpenTabs |> List.tryFind (fun t -> t.Application.ApplicationNumber = activeId) with
                        | Some tab when not tab.IsMinimized ->
                            Html.li [
                                Html.a [
                                    prop.className "font-bold"
                                    prop.text tab.Application.Title
                                ]
                            ]
                        | _ -> ()
                    | None -> ()
                ]
            ]
        ]

    let applicationTable (apps: CagApplication list) dispatch =
        Html.div [
            prop.className "bg-white/80 rounded-md shadow-md p-4 ag-theme-alpine"
            prop.children [
                AgGrid.grid [
                    AgGrid.rowData (apps |> Array.ofList)
                    AgGrid.defaultColDef [
                        ColumnDef.resizable true
                        ColumnDef.sortable true
                        ColumnDef.filter (RowFilter.Text)
                        //ColumnDef.filter
                    ]
                    AgGrid.columnDefs [
                        ColumnDef.create<string> [
                            ColumnDef.field "ApplicationNumber"
                            ColumnDef.headerName "App #"
                            ColumnDef.width 120
                        ]
                        ColumnDef.create<string> [
                            ColumnDef.field "Title"
                            ColumnDef.headerName "Title"
                            ColumnDef.width 250
                            ColumnDef.cellRenderer (fun x y ->
                                Html.div [
                                    Html.div [
                                        prop.className "font-medium"
                                        prop.text (y.Title)
                                    ]
                                    Html.div [
                                        prop.className "text-sm text-gray-500"
                                        prop.text (y.ContactName)
                                    ]
                                ]
                            )
                        ]
                        ColumnDef.create<string> [
                            ColumnDef.field "ApplicantOrganisation"
                            ColumnDef.headerName "Organisation"
                            ColumnDef.width 200
                        ]
                        ColumnDef.create<string> [
                            ColumnDef.field "Status"
                            AgGrid.ColumnDef.headerName "Status"
                            ColumnDef.width 150
                            ColumnDef.cellRenderer (fun x y ->
                                Html.div [
                                    prop.className (
                                        match (string y.Status).ToLower() with
                                        | s when s.Contains("approved") -> "badge badge-success"
                                        | s when s.Contains("pending") -> "badge badge-warning"
                                        | s when s.Contains("rejected") -> "badge badge-error"
                                        | _ -> "badge"
                                    )
                                    prop.text (string y.Status)
                                ]
                            )
                        ]
                        ColumnDef.create<unit> [
                            ColumnDef.headerName "Actions"
                            ColumnDef.width 150
                            ColumnDef.cellRenderer (fun x y ->
                                Html.button [
                                    prop.className "btn btn-sm btn-primary"
                                    prop.onClick (fun _ -> dispatch (OpenTab y))
                                    prop.text "View Details"
                                ]
                            )
                        ]
                    ]
                    AgGrid.pagination true
                    AgGrid.paginationPageSize 10
                    AgGrid.domLayout DOMLayout.AutoHeight
                    //AgGrid.theme "alpine"
                ]
            ]
        ]

    let applicationDetail (app: CagApplication) dispatch =
        Html.div [
            prop.className "bg-white/80 rounded-md shadow-md p-6 animate-fadeIn"
            prop.children [
                Html.div [
                    prop.className "flex justify-between items-center mb-6"
                    prop.children [
                        Html.h2 [
                            prop.className "text-3xl font-bold"
                            prop.text app.Title
                        ]
                        Html.button [
                            prop.className "btn btn-outline"
                            prop.onClick (fun _ -> dispatch (ToggleTabMinimize app.ApplicationNumber))
                            prop.text "Minimize"
                        ]
                    ]
                ]
                Html.div [
                    prop.className "grid grid-cols-2 gap-4"
                    prop.children [
                        Html.div [
                            prop.children [
                                Html.strong "Application Number: "
                                Html.span app.ApplicationNumber
                            ]
                        ]
                        Html.div [
                            prop.children [
                                Html.strong "Reference: "
                                Html.span app.Reference
                            ]
                        ]
                        Html.div [
                            prop.children [
                                Html.strong "Organisation: "
                                Html.span app.ApplicantOrganisation
                            ]
                        ]
                        Html.div [
                            prop.children [
                                Html.strong "Status: "
                                Html.span app.Status
                            ]
                        ]
                        Html.div [
                            prop.children [
                                Html.strong "Contact Name: "
                                Html.span app.ContactName
                            ]
                        ]
                        Html.div [
                            prop.children [
                                Html.strong "Address: "
                                Html.span (String.concat ", " app.Address)
                            ]
                        ]
                        Html.div [
                            prop.children [
                                Html.strong "Postcode: "
                                Html.span app.Postcode
                            ]
                        ]
                        Html.div [
                            prop.children [
                                Html.strong "Telephone: "
                                Html.span app.Telephone
                            ]
                        ]
                        Html.div [
                            prop.children [
                                Html.strong "Email: "
                                Html.span app.Email
                            ]
                        ]
                        Html.div [
                            prop.children [
                                Html.strong "Other References: "
                                Html.span (Option.defaultValue "N/A" app.OtherRefs)
                            ]
                        ]
                        Html.div [
                            prop.className "grid grid-cols-2 gap-4"
                            prop.children [
                                Html.div [
                                    prop.className "col-span-1"
                                    prop.children [
                                        Html.strong "Medical Purposes: "
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
                                                    prop.className "flex items-center"
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
                            ]
                        ]
                        Html.div [
                            prop.children [
                                Html.strong "Cohort Description: "
                                Html.span app.CohortDescription
                            ]
                        ]
                        Html.div [
                            prop.children [
                                Html.strong "Confidential Info: "
                                Html.span app.ConfidentialInfo
                            ]
                        ]
                        Html.div [
                            prop.className "grid grid-cols-2 gap-4"
                            prop.children [
                                Html.div [
                                    prop.className "col-span-1"
                                    prop.children [
                                        Html.strong "S251 Classes: "
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
                                                    prop.className "flex items-center"
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
                        Html.div [
                            prop.children [
                                Html.strong "Sponsor: "
                                Html.span app.Sponsor
                            ]
                        ]
                        Html.div [
                            prop.children [
                                Html.strong "Outcome Date: "
                                Html.span (Option.defaultValue "N/A" (Option.map string app.OutcomeDate))
                            ]
                        ]
                        Html.div [
                            prop.children [
                                Html.strong "Next Review Date: "
                                Html.span (Option.defaultValue "N/A" (Option.map string app.NextReviewDate))
                            ]
                        ]
                        Html.div [
                            prop.children [
                                Html.strong "Notes: "
                                Html.span app.Notes
                            ]
                        ]
                        Html.div [
                            prop.children [
                                Html.strong "NDOO: "
                                Html.span (Option.defaultValue "N/A" app.NDOO)
                            ]
                        ]
                        Html.div [
                            prop.children [
                                Html.strong "English CPI: "
                                Html.span (Option.defaultValue "N/A" (Option.map string app.EnglishCPI))
                            ]
                        ]
                        Html.div [
                            prop.children [
                                Html.strong "Welsh CPI: "
                                Html.span (Option.defaultValue "N/A" (Option.map string app.WelshCPI))
                            ]
                        ]
                        Html.div [
                            prop.className "col-span-2"
                            prop.children [
                                Html.strong "Summary: "
                                Html.p app.Summary
                            ]
                        ]
                    ]
                ]
            ]
        ]

    let discrepancyTable (discrepancies: ApplicationDiscrepancy list) dispatch =
        Html.div [
            prop.className "bg-white/80 rounded-md shadow-md p-4"
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
                                        Html.td disc.ApplicationNumber
                                        Html.td fieldName
                                        Html.td frontValue
                                        Html.td detailValue
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

    let tabBar (tabs: TabState list) activeTabId dispatch =
        Html.div [
            prop.className "fixed bottom-0 left-0 right-0 bg-white shadow-lg border-t border-gray-200"
            prop.children [
                Html.div [
                    prop.className "flex space-x-2 p-2"
                    prop.children [
                        for tab in tabs ->
                            Html.div [
                                prop.className [
                                    "flex items-center px-4 py-2 rounded-t-lg cursor-pointer transition-colors"
                                    if Some tab.Application.ApplicationNumber = activeTabId then
                                        "bg-blue-500 text-white"
                                    else "bg-gray-100 hover:bg-gray-200"
                                ]
                                prop.children [
                                    Html.span [
                                        prop.className "mr-2"
                                        prop.text tab.Application.Title
                                    ]
                                    Html.button [
                                        prop.className "hover:text-red-500 ml-2"
                                        prop.onClick (fun e ->
                                            e.stopPropagation()
                                            dispatch (CloseTab tab.Application.ApplicationNumber)
                                        )
                                        prop.children [
                                            Html.i [
                                                prop.className "fas fa-times"
                                                prop.text "×"
                                            ]
                                        ]
                                    ]
                                ]
                                prop.onClick (fun _ ->
                                    if tab.IsMinimized then
                                        dispatch (ToggleTabMinimize tab.Application.ApplicationNumber)
                                    dispatch (SetActiveTab (Some tab.Application.ApplicationNumber))
                                )
                            ]
                    ]
                ]
            ]
        ]

let view model dispatch =
    Html.section [
        prop.className "min-h-screen w-screen overflow-auto bg-gradient-to-br from-blue-50 to-indigo-100 pb-16"
        prop.children [
            Html.div [
                prop.className "container mx-auto px-4 py-8"
                prop.children [
                    Html.h1 [
                        prop.className "text-center text-5xl font-bold text-gray-800 mb-8"
                        prop.text "CAG Register"
                    ]
                    ViewComponents.navigationBreadcrumb model dispatch

                    // Main content area
                    Html.div [
                        prop.className "transition-all duration-300 ease-in-out"
                        prop.children [
                            // Show table or discrepancy view
                            match model.Applications, model.CurrentView with
                            | NotStarted, _ ->
                                Html.div [
                                    prop.className "animate-pulse bg-white/80 rounded-md shadow-md p-8 text-center"
                                    prop.text "Loading..."
                                ]
                            | Loading, _ ->
                                Html.div [
                                    prop.className "animate-pulse bg-white/80 rounded-md shadow-md p-8 text-center"
                                    prop.text "Loading applications..."
                                ]
                            | Loaded apps, TableView ->
                                ViewComponents.applicationTable apps dispatch
                            | Loaded _, DiscrepancyView ->
                                match model.Discrepancies with
                                | Loaded discrepancies -> ViewComponents.discrepancyTable discrepancies dispatch
                                | Loading -> Html.div "Loading discrepancies..."
                                | NotStarted -> Html.div "Click to load discrepancies"
                        ]
                    ]

                    // Tab content area
                    match model.ActiveTabId with
                    | Some activeId ->
                        match model.OpenTabs |> List.tryFind (fun t -> t.Application.ApplicationNumber = activeId) with
                        | Some tab when not tab.IsMinimized ->
                            Html.div [
                                prop.className "mt-4"
                                prop.children [
                                    ViewComponents.applicationDetail tab.Application dispatch
                                ]
                            ]
                        | _ -> ()
                    | None -> ()
                ]
            ]

            // Tab bar at bottom
            if not (List.isEmpty model.OpenTabs) then
                ViewComponents.tabBar model.OpenTabs model.ActiveTabId dispatch
        ]
    ]