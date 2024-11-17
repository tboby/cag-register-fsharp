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