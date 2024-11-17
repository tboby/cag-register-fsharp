module Index

open Elmish
open SAFE
open Shared
open Feliz

type View =
    | TableView
    | DetailView

type Model = {
    Applications: RemoteData<CagApplication list>
    SelectedApplication: CagApplication option
    CurrentView: View
}

type Msg =
    | LoadApplications of ApiCall<unit, CagApplication list>
    | SelectApplication of CagApplication option
    | SetView of View

let applicationsApi =
    Api.makeProxy<ICagApplicationsApi> ()
let init () =
    let model = {
        Applications = NotStarted
        SelectedApplication = None
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
    | SelectApplication appOpt ->
        { model with
            SelectedApplication = appOpt
            CurrentView = match appOpt with Some _ -> DetailView | None -> TableView
        }, Cmd.none
    | SetView view ->
        { model with
            CurrentView = view
            SelectedApplication = if view = TableView then None else model.SelectedApplication
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
                    match model.SelectedApplication with
                    | Some app ->
                        Html.li [
                            Html.a [
                                prop.className (if model.CurrentView = DetailView then "font-bold" else "")
                                prop.text app.Title
                            ]
                        ]
                    | None -> ()
                ]
            ]
        ]

    let applicationTable (apps: CagApplication list) dispatch =
        Html.div [
            prop.className "bg-white/80 rounded-md shadow-md p-4"
            prop.children [
                Html.div [
                    prop.className "overflow-x-auto"
                    prop.children [
                        Html.table [
                            prop.className "table w-full"
                            prop.children [
                                Html.thead [
                                    Html.tr [
                                        Html.th [
                                            prop.className "bg-primary/10"
                                            prop.text "App #"
                                        ]
                                        Html.th [
                                            prop.className "bg-primary/10"
                                            prop.text "Title"
                                        ]
                                        Html.th [
                                            prop.className "bg-primary/10"
                                            prop.text "Organisation"
                                        ]
                                        Html.th [
                                            prop.className "bg-primary/10"
                                            prop.text "Status"
                                        ]
                                        Html.th [
                                            prop.className "bg-primary/10"
                                            prop.text "Actions"
                                        ]
                                    ]
                                ]
                                Html.tbody [
                                    for app in apps do
                                        Html.tr [
                                            prop.className "hover:bg-base-200"
                                            prop.children [
                                                Html.td app.ApplicationNumber
                                                Html.td [
                                                    Html.div [
                                                        prop.className "font-medium"
                                                        prop.text app.Title
                                                    ]
                                                    Html.div [
                                                        prop.className "text-sm text-gray-500"
                                                        prop.text app.ContactName
                                                    ]
                                                ]
                                                Html.td app.ApplicantOrganisation
                                                Html.td [
                                                    Html.div [
                                                        prop.className (
                                                            match app.Status.ToLower() with
                                                            | s when s.Contains("approved") -> "badge badge-success"
                                                            | s when s.Contains("pending") -> "badge badge-warning"
                                                            | s when s.Contains("rejected") -> "badge badge-error"
                                                            | _ -> "badge"
                                                        )
                                                        prop.text app.Status
                                                    ]
                                                ]
                                                Html.td [
                                                    Html.button [
                                                        prop.className "btn btn-sm btn-primary"
                                                        prop.onClick (fun _ ->
                                                            dispatch (SelectApplication (Some app))
                                                        )
                                                        prop.text "View Details"
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
                            prop.onClick (fun _ -> dispatch (SetView TableView))
                            prop.text "Back to List"
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

let view model dispatch =
    Html.section [
        prop.className "min-h-screen w-screen overflow-auto bg-gradient-to-br from-blue-50 to-indigo-100"
        prop.children [
            Html.div [
                prop.className "container mx-auto px-4 py-8"
                prop.children [
                    Html.h1 [
                        prop.className "text-center text-5xl font-bold text-gray-800 mb-8"
                        prop.text "CAG Register"
                    ]
                    ViewComponents.navigationBreadcrumb model dispatch
                    Html.div [
                        prop.className "transition-all duration-300 ease-in-out"
                        prop.children [
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
                            | Loaded _, DetailView ->
                                match model.SelectedApplication with
                                | Some app -> ViewComponents.applicationDetail app dispatch
                                | None ->
                                    Html.div [
                                        prop.className "bg-white/80 rounded-md shadow-md p-4 text-center"
                                        prop.text "Select an application to view details"
                                    ]
                        ]
                    ]
                ]
            ]
        ]
    ]