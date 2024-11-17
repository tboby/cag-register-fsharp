module Index

open Elmish
open SAFE
open Shared
open Feliz

type Model = {
    Applications: RemoteData<CagApplication list>
    SelectedApplication: CagApplication option
}

type Msg =
    | LoadApplications of ApiCall<unit, CagApplication list>
    | SelectApplication of CagApplication

let applicationsApi =
    Api.makeProxy<ICagApplicationsApi> ()
let init () =
    let model = { Applications = NotStarted; SelectedApplication = None }
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
    | SelectApplication app ->
        { model with SelectedApplication = Some app }, Cmd.none

module ViewComponents =
    let applicationList (apps: CagApplication list) dispatch =
        Html.div [
            prop.className "bg-white/80 rounded-md shadow-md p-4 w-full"
            prop.children [
                Html.div [
                    prop.className "grid grid-cols-1 gap-4"
                    prop.children [
                        for app in apps do
                            Html.div [
                                prop.className "p-4 border rounded hover:bg-gray-100 cursor-pointer"
                                prop.onClick (fun _ -> dispatch (SelectApplication app))
                                prop.children [
                                    Html.h3 [
                                        prop.className "font-bold"
                                        prop.text app.Title
                                    ]
                                    Html.div [
                                        prop.className "text-sm text-gray-600"
                                        prop.children [
                                            Html.span [
                                                prop.className "mr-2"
                                                prop.text (sprintf "App #%s" app.ApplicationNumber)
                                            ]
                                            Html.span [
                                                prop.text (sprintf "Status: %s" app.Status)
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                    ]
                ]
            ]
        ]

    let applicationDetail (app: CagApplication) =
        Html.div [
            prop.className "bg-white/80 rounded-md shadow-md p-4 w-full"
            prop.children [
                Html.h2 [
                    prop.className "text-2xl font-bold mb-4"
                    prop.text app.Title
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
        prop.className "h-screen w-screen overflow-auto"
        prop.style [
            style.backgroundSize "cover"
            style.backgroundImageUrl "https://unsplash.it/1200/900?random"
            style.backgroundPosition "no-repeat center center fixed"
        ]
        prop.children [
            Html.div [
                prop.className "container mx-auto px-4 py-8"
                prop.children [
                    Html.h1 [
                        prop.className "text-center text-5xl font-bold text-white mb-8"
                        prop.text "CAG Register"
                    ]
                    Html.div [
                        prop.className "grid grid-cols-1 md:grid-cols-2 gap-8"
                        prop.children [
                            match model.Applications with
                            | NotStarted -> Html.text "Loading..."
                            | Loading -> Html.text "Loading applications..."
                            | Loaded apps ->
                                ViewComponents.applicationList apps dispatch
                                match model.SelectedApplication with
                                | Some app -> ViewComponents.applicationDetail app
                                | None ->
                                    Html.div [
                                        prop.className "bg-white/80 rounded-md shadow-md p-4"
                                        prop.text "Select an application to view details"
                                    ]
                        ]
                    ]
                ]
            ]
        ]
    ]