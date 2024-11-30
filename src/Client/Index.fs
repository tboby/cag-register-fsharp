module Index

open Elmish
open SAFE
open Shared
open Feliz
open Feliz.AgGrid
open Fable.DateFunctions
open System

type TabContent =
    | TableContent
    | ApplicationContent of CagApplication
    | DiscrepancyContent

type TabState = {
    Id: string
    Title: string
    Content: TabContent
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
    | OpenAndFocusTab of TabContent
    | CloseTab of string
    | SetActiveTab of string


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
              Content = TableContent }
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
            }
            { model with OpenTabs = model.OpenTabs @ [newTab]; ActiveTabId = id },
            match content with
            | DiscrepancyContent ->
                Cmd.ofMsg (LoadDiscrepancies(Start()))
            | _ -> Cmd.none

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
            }
            { model with OpenTabs = model.OpenTabs @ [newTab] },
            match content with
            | DiscrepancyContent ->
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
                    // Show current application tab if one is active
                    match model.OpenTabs |> List.tryFind (fun t -> t.Id = model.ActiveTabId) with
                    | Some tab when tab.Id <> "home" && tab.Id <> "discrepancies" ->
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
            prop.className "bg-white/80 rounded-md shadow-md p-4 ag-theme-alpine overflow-hidden"
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
                    //agGridProp("paginationPageSizeSelector", seq{yield 10})
                    AgGrid.rowData (apps |> Array.ofList)
                    AgGrid.defaultColDef [
                        ColumnDef.resizable true
                        ColumnDef.sortable true
                        ColumnDef.filter RowFilter.Text
                    ]
                    agGridProp("rowClassRules", {| ``opacity-50`` = (fun (rowParams : ICellRendererParams<_, _>) ->
                        match rowParams.data with
                        | Some app when app.ApplicationStatus = Obsolete -> true
                        | _ -> false
                    ) |})
                    AgGrid.columnDefs [
                        ColumnDef.create [
                            ColumnDef.headerName ""
                            ColumnDef.width 60
                            ColumnDef.valueGetter (fun x -> x)
                            ColumnDef.cellRenderer (fun cellParams ->
                                match cellParams.data with
                                | Some app ->
                                    Html.div [
                                        prop.className "flex gap-2"
                                        prop.children [
                                            Html.button [
                                                prop.className "btn btn-sm btn-outline"
                                                prop.onClick (fun _ -> dispatch (OpenAndFocusTab (ApplicationContent app)))
                                                prop.children [
                                                    Html.i [
                                                        prop.className "fas fa-eye"
                                                        prop.title "Open and focus"
                                                    ]
                                                ]
                                            ]
                                            Html.button [
                                                prop.className "btn btn-sm btn-primary"
                                                prop.onClick (fun _ -> dispatch (OpenTab(ApplicationContent app)))
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
                            ColumnDef.width 180
                            ColumnDef.valueGetter (fun x -> x.Reference)
                        ]
                        ColumnDef.create [
                            ColumnDef.filter RowFilter.Text
                            ColumnDef.headerName "App #"
                            ColumnDef.width 100
                            ColumnDef.valueGetter (fun x -> x.ApplicationNumber)
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
                            ColumnDef.valueGetter (fun x -> x.Status)
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
                        ColumnDef.create [
                            ColumnDef.filter RowFilter.Date
                            ColumnDef.headerName "Outcome Date"
                            ColumnDef.width 150
                            ColumnDef.valueGetter (fun x -> x.OutcomeDate)
                            ColumnDef.valueFormatter (fun valueParams ->
                                match Option.flatten valueParams.value with
                                | Some date -> date.Format("yyyy-MM-dd")
                                | None -> "")
                            columnDefProp("sort", "desc")
                        ]
                        ColumnDef.create [
                            ColumnDef.filter RowFilter.Date
                            ColumnDef.headerName "Next Review Date"
                            ColumnDef.width 150
                            ColumnDef.valueFormatter (fun valueParams ->
                                match Option.flatten valueParams.value with
                                | Some (date : DateTime) -> date.Format("yyyy-MM-dd")
                                | None -> "")
                            ColumnDef.valueGetter (fun x -> x.NextReviewDate)
                        ]

                    ]
                    AgGrid.pagination true
                    AgGrid.paginationPageSize 10
                    //AgGrid.paginationAutoPageSize true
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
                "bg-white/80 rounded-md shadow-md p-6 animate-fadeIn mb-16 max-w-5xl mx-auto"
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
                                    Html.span app.ApplicationNumber
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
                                        Html.td disc.ApplicationNumber
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
                        if List.length (List.filter (fun t -> t.Id <> "home") tabs) > 1 then
                            yield Html.button [
                                prop.className "btn btn-danger ml-2"
                                prop.onClick (fun _ ->
                                    // Dispatch a message to close all tabs
                                    tabs
                                    |> List.iter (fun t ->
                                        if t.Id <> "home" then
                                            dispatch (CloseTab t.Id)
                                    )
                                )
                                prop.text "Close All Tabs"
                            ]
                    ]
                ]
            ]
        ]

let view model dispatch =
    Html.section [
        prop.className "min-h-screen bg-gradient-to-br from-blue-50 to-indigo-100 relative overflow-x-hidden"
        prop.children [
            Html.div [
                prop.className "container mx-auto px-4 py-8 pb-20 max-w-7xl"
                prop.children [
                    Html.h1 [
                        prop.className "text-center text-5xl font-bold text-gray-800 mb-8"
                        prop.text "CAG Register"
                    ]

                    // Active tab content
                    match model.OpenTabs |> List.tryFind (fun t -> t.Id = model.ActiveTabId) with
                    | Some tab -> ViewComponents.tabContent tab model dispatch
                    | None -> Html.none
                ]
            ]

            // Tab bar
            ViewComponents.tabBar model.OpenTabs model.ActiveTabId dispatch
        ]
    ]