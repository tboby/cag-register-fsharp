module Index

open Elmish
open SAFE
open Shared
open Feliz
open Feliz.AgGrid

type TabContent =
    | TableContent
    | ApplicationContent of CagApplication
    | DiscrepancyContent

type TabState = {
    Id: string
    Title: string
    Content: TabContent
    IsMinimized: bool
}

type Model = {
    Applications: RemoteData<CagApplication list>
    FrontPageEntries: RemoteData<CagFrontPageEntry list>
    Discrepancies: RemoteData<ApplicationDiscrepancy list>
    OpenTabs: TabState list
    ActiveTabId: string
}

type Msg =
    | LoadApplications of ApiCall<unit, CagApplication list>
    | LoadFrontPageEntries of ApiCall<unit, CagFrontPageEntry list>
    | LoadDiscrepancies of ApiCall<unit, ApplicationDiscrepancy list>
    | OpenTab of TabContent
    | CloseTab of string
    | SetActiveTab of string
    | ToggleTabMinimize of string
    | OpenAndFocusTab of TabContent


let applicationsApi =
    Api.makeProxy<ICagApplicationsApi> ()
let init () =
    let model = {
        Applications = NotStarted
        FrontPageEntries = NotStarted
        Discrepancies = NotStarted
        OpenTabs = [
            { Id = "home"
              Title = "Applications"
              Content = TableContent
              IsMinimized = false }
        ]
        ActiveTabId = "home"
    }
    let cmd = LoadApplications(Start()) |> Cmd.ofMsg
    model, cmd
let createShortTitle =
    function
    | TableContent -> "Applications"
    | DiscrepancyContent -> "Discrepancies"
    | ApplicationContent app ->
        let maxTitleLength = 20

        let shortTitle =
            if app.Title.Length > maxTitleLength then
                app.Title.Substring(0, maxTitleLength) + "..."
            else
                app.Title

        sprintf "%s: %s" app.ApplicationNumber shortTitle

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
    | OpenTab content ->
        let (id, title) =
            match content with
            | TableContent -> "home", "Applications"
            | ApplicationContent app -> app.ApplicationNumber, createShortTitle content
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
                IsMinimized = false
            }
            { model with OpenTabs = model.OpenTabs @ [newTab] },
            match content with
            | DiscrepancyContent when model.Discrepancies = NotStarted ->
                Cmd.ofMsg (LoadDiscrepancies(Start()))
            | _ -> Cmd.none
    | CloseTab id ->
        if id = "home" then
            model, Cmd.none // Can't close home tab
        else
            let newTabs = model.OpenTabs |> List.filter (fun t -> t.Id <> id)
            { model with
                OpenTabs = newTabs
                ActiveTabId = if model.ActiveTabId = id then "home" else model.ActiveTabId
            }, Cmd.none
    | SetActiveTab tabId ->
        { model with ActiveTabId = tabId }, Cmd.none
    | ToggleTabMinimize appNumber ->
        let newTabs =
            model.OpenTabs
            |> List.map (fun tab ->
                if tab.Id = appNumber then
                    { tab with IsMinimized = not tab.IsMinimized }
                else tab
            )
        { model with OpenTabs = newTabs }, Cmd.none
    | OpenAndFocusTab content ->
        let (id, title) =
            match content with
            | TableContent -> "home", "Applications"
            | ApplicationContent app -> app.ApplicationNumber, createShortTitle content
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
                IsMinimized = false
            }
            { model with
                OpenTabs = model.OpenTabs @ [newTab]
                ActiveTabId = id
            },
            match content with
            | DiscrepancyContent when model.Discrepancies = NotStarted ->
                Cmd.ofMsg (LoadDiscrepancies(Start()))
            | _ -> Cmd.none
module ViewComponents =
    let navigationBreadcrumb (model: Model) dispatch =
        Html.div [
            prop.className "text-sm breadcrumbs p-4 bg-white/80 rounded-md shadow-md mb-4"
            prop.children [
                Html.ul [
                    Html.li [
                        Html.a [
                            prop.className (if model.ActiveTabId = "home" then "font-bold" else "")
                            prop.onClick (fun _ -> dispatch (SetActiveTab "home"))
                            prop.text "Applications"
                        ]
                    ]
                    Html.li [
                        Html.a [
                            prop.className (if model.ActiveTabId = "discrepancies" then "font-bold" else "")
                            prop.onClick (fun _ -> dispatch (OpenAndFocusTab DiscrepancyContent))
                            prop.text "Discrepancies"
                        ]
                    ]
                    // Show current application tab if one is active
                    match model.OpenTabs |> List.tryFind (fun t -> t.Id = model.ActiveTabId) with
                    | Some tab when tab.Id <> "home" && tab.Id <> "discrepancies" && not tab.IsMinimized ->
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
                            ColumnDef.width 200
                            ColumnDef.cellRenderer (fun x y ->
                                Html.div [
                                    prop.className "flex gap-2"
                                    prop.children [
                                        Html.button [
                                            prop.className "btn btn-sm btn-outline"
                                            prop.onClick (fun _ -> dispatch (OpenTab (ApplicationContent y)))
                                            prop.children [
                                                Html.i [
                                                    prop.className "fas fa-eye"  // Using Font Awesome icon
                                                    prop.title "Open in background"
                                                ]
                                            ]
                                        ]
                                        Html.button [
                                            prop.className "btn btn-sm btn-primary"
                                            prop.onClick (fun _ -> dispatch (OpenAndFocusTab (ApplicationContent y)))
                                            prop.children [
                                                Html.i [
                                                    prop.className "fas fa-external-link-alt"  // Using Font Awesome icon
                                                    prop.title "Open and focus"
                                                ]
                                            ]
                                        ]
                                    ]
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

    let tabContent (tab: TabState) model dispatch =
        match tab.Content with
        | TableContent ->
            match model.Applications with
            | Loaded apps -> applicationTable apps dispatch
            | Loading ->
                Html.div [
                    prop.className "animate-pulse bg-white/80 rounded-md shadow-md p-8 text-center"
                    prop.text "Loading applications..."
                ]
            | NotStarted ->
                Html.div [
                    prop.className "animate-pulse bg-white/80 rounded-md shadow-md p-8 text-center"
                    prop.text "Loading..."
                ]
        | ApplicationContent app ->
            applicationDetail app dispatch
        | DiscrepancyContent ->
            match model.Discrepancies with
            | Loaded discrepancies -> discrepancyTable discrepancies dispatch
            | Loading -> Html.div "Loading discrepancies..."
            | NotStarted -> Html.div "Loading discrepancies..."

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
                                    if tab.Id = activeTabId then
                                        "bg-blue-500 text-white"
                                    else "bg-gray-100 hover:bg-gray-200"
                                ]
                                prop.children [
                                    Html.span [
                                        prop.className "mr-2"
                                        prop.text tab.Title
                                    ]
                                    if tab.Id <> "home" then  // Don't show close button on home tab
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
                                prop.onClick (fun _ ->
                                    if tab.IsMinimized then
                                        dispatch (ToggleTabMinimize tab.Id)
                                    dispatch (SetActiveTab tab.Id)
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

                    // Active tab content
                    match model.OpenTabs |> List.tryFind (fun t -> t.Id = model.ActiveTabId) with
                    | Some tab when not tab.IsMinimized ->
                        ViewComponents.tabContent tab model dispatch
                    | _ -> Html.none
                ]
            ]

            // Tab bar
            ViewComponents.tabBar model.OpenTabs model.ActiveTabId dispatch
        ]
    ]