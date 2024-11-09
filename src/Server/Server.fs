module Server

open SAFE
open Saturn
open Shared

// CAG Application Types
[<RequireQualifiedAccess>]
type MedicalPurpose =
    | PreventativeMedicine
    | MedicalDiagnosis
    | MedicalResearch
    | CareAndTreatment
    | HealthAndSocialCareManagement
    | InformingIndividuals

[<RequireQualifiedAccess>]
type S251Class =
    | SpecificSupport
    | ClassI_Identifiability
    | ClassII_GeographicalLocation
    | ClassIII_IdentifyAndContact
    | ClassIV_LinkingMultipleSources
    | ClassV_AuditAndMonitoring
    | ClassVI_GrantingAccess

type CagApplication = {
    ApplicationNumber: string
    Reference: string
    OtherRefs: string option
    Title: string
    Summary: string
    ApplicantOrganisation: string
    ContactName: string
    Address: string list // Multiple address lines
    Postcode: string
    Telephone: string
    Email: string
    MedicalPurposes: MedicalPurpose list
    CohortDescription: string
    ConfidentialInfo: string
    S251Classes: S251Class list
    Sponsor: string
    Status: string
    OutcomeDate: System.DateTime option
    NextReviewDate: System.DateTime option
    Notes: string
}

module CagRegisterXLSM =
    let cagRegisterFile = "cag-register-research-master-2021_1uhwQyX.xlsm"
    open System.IO
    open ClosedXML.Excel

    let getApplications(workbook: XLWorkbook) =
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

        appSheets

    let parseMedicalPurposes (sheet: IXLWorksheet) =
        [
            if sheet.Cell("A10").GetString().Contains("Y") then MedicalPurpose.MedicalResearch
            if sheet.Cell("A11").GetString().Contains("Y") then MedicalPurpose.PreventativeMedicine
            if sheet.Cell("A12").GetString().Contains("Y") then MedicalPurpose.MedicalDiagnosis
            if sheet.Cell("A13").GetString().Contains("Y") then MedicalPurpose.CareAndTreatment
            if sheet.Cell("A14").GetString().Contains("Y") then MedicalPurpose.HealthAndSocialCareManagement
            if sheet.Cell("A15").GetString().Contains("Y") then MedicalPurpose.InformingIndividuals
        ]

    let parseS251Classes (sheet: IXLWorksheet) =
        [
            if sheet.Cell("A20").GetString().Contains("Y") then S251Class.SpecificSupport
            if sheet.Cell("A21").GetString().Contains("Y") then S251Class.ClassI_Identifiability
            if sheet.Cell("A22").GetString().Contains("Y") then S251Class.ClassII_GeographicalLocation
            if sheet.Cell("A23").GetString().Contains("Y") then S251Class.ClassIII_IdentifyAndContact
            if sheet.Cell("A24").GetString().Contains("Y") then S251Class.ClassIV_LinkingMultipleSources
            if sheet.Cell("A25").GetString().Contains("Y") then S251Class.ClassV_AuditAndMonitoring
            if sheet.Cell("A26").GetString().Contains("Y") then S251Class.ClassVI_GrantingAccess
        ]

    let parseApplication (sheet: IXLWorksheet) =
        try
            let app = {
                ApplicationNumber = sheet.Cell("B1").GetString()
                Reference = sheet.Cell("B2").GetString()
                OtherRefs =
                    let refs = sheet.Cell("B3").GetString()
                    if System.String.IsNullOrWhiteSpace(refs) then None else Some refs
                Title = sheet.Cell("B4").GetString()
                Summary = sheet.Cell("B5").GetString()
                ApplicantOrganisation = sheet.Cell("B6").GetString()
                ContactName = sheet.Cell("B7").GetString()
                Address =
                    [
                        sheet.Cell("B8").GetString()
                        sheet.Cell("B9").GetString()
                        sheet.Cell("B10").GetString()
                    ] |> List.filter (not << System.String.IsNullOrWhiteSpace)
                Postcode = sheet.Cell("B11").GetString()
                Telephone = sheet.Cell("B12").GetString()
                Email = sheet.Cell("B13").GetString()
                MedicalPurposes = parseMedicalPurposes sheet
                CohortDescription = sheet.Cell("B14").GetString()
                ConfidentialInfo = sheet.Cell("B15").GetString()
                S251Classes = parseS251Classes sheet
                Sponsor = sheet.Cell("B16").GetString()
                Status = sheet.Cell("B17").GetString()
                OutcomeDate =
                    try Some(sheet.Cell("B18").GetDateTime())
                    with _ -> None
                NextReviewDate =
                    try Some(sheet.Cell("B19").GetDateTime())
                    with _ -> None
                Notes = sheet.Cell("B20").GetString()
            }
            Some app
        with ex ->
            printfn "Error parsing application: %s" ex.Message
            None

    let getApplicationDetails() =
        use workbook = new XLWorkbook(cagRegisterFile) // Ensure workbook is disposed after all operations
        let applications = getApplications(workbook)
        applications
        |> List.choose (fun (appNo, sheet) ->
            parseApplication sheet
        )

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