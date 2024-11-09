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

// Define a new discriminated union for CPI values
[<RequireQualifiedAccess>]
type CPIValue =
    | Yes
    | No
    | Other of string

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
    NDOO: string option
    EnglishCPI: CPIValue option
    WelshCPI: CPIValue option
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
            if sheet.Cell("B18").GetString().Contains("Y") then MedicalPurpose.MedicalResearch
            if sheet.Cell("B16").GetString().Contains("Y") then MedicalPurpose.PreventativeMedicine
            if sheet.Cell("B17").GetString().Contains("Y") then MedicalPurpose.MedicalDiagnosis
            if sheet.Cell("B19").GetString().Contains("Y") then MedicalPurpose.CareAndTreatment
            if sheet.Cell("B20").GetString().Contains("Y") then MedicalPurpose.HealthAndSocialCareManagement
            if sheet.Cell("B21").GetString().Contains("Y") then MedicalPurpose.InformingIndividuals
        ]

    let parseS251Classes (sheet: IXLWorksheet) =
        [
            if sheet.Cell("B24").GetString().Contains("Y") then S251Class.SpecificSupport
            if sheet.Cell("B25").GetString().Contains("Y") then S251Class.ClassI_Identifiability
            if sheet.Cell("B26").GetString().Contains("Y") then S251Class.ClassII_GeographicalLocation
            if sheet.Cell("B27").GetString().Contains("Y") then S251Class.ClassIII_IdentifyAndContact
            if sheet.Cell("B28").GetString().Contains("Y") then S251Class.ClassIV_LinkingMultipleSources
            if sheet.Cell("B29").GetString().Contains("Y") then S251Class.ClassV_AuditAndMonitoring
            if sheet.Cell("B30").GetString().Contains("Y") then S251Class.ClassVI_GrantingAccess
        ]

    let parseApplication (sheet: IXLWorksheet) =
        try
            let app = {
                ApplicationNumber = sheet.Cell("B3").GetString()
                Reference = sheet.Cell("B4").GetString()
                OtherRefs =
                    let refs = sheet.Cell("B5").GetString()
                    if System.String.IsNullOrWhiteSpace(refs) then None else Some refs
                Title = sheet.Cell("B6").GetString()
                Summary = sheet.Cell("B7").GetString()
                ApplicantOrganisation = sheet.Cell("B8").GetString()
                ContactName = sheet.Cell("B9").GetString()
                Address =
                    [
                        sheet.Cell("B10").GetString()
                        sheet.Cell("B11").GetString()
                        sheet.Cell("B12").GetString()
                    ] |> List.filter (not << System.String.IsNullOrWhiteSpace)
                Postcode = sheet.Cell("B13").GetString()
                Telephone = sheet.Cell("B14").GetString()
                Email = sheet.Cell("B15").GetString()
                MedicalPurposes = parseMedicalPurposes sheet
                CohortDescription = sheet.Cell("B22").GetString()
                ConfidentialInfo = sheet.Cell("B23").GetString()
                S251Classes = parseS251Classes sheet
                Sponsor = sheet.Cell("B31").GetString()
                Status = sheet.Cell("B32").GetString()
                OutcomeDate =
                    try Some(sheet.Cell("B34").GetDateTime())
                    with _ -> None
                NextReviewDate =
                    try Some(sheet.Cell("B35").GetDateTime())
                    with _ -> None
                Notes = sheet.Cell("B36").GetString()
                NDOO =
                    let value = sheet.Cell("B37").GetString()
                    if System.String.IsNullOrWhiteSpace(value) then None else Some value
                EnglishCPI =
                    let value = sheet.Cell("B38").GetString()
                    if System.String.IsNullOrWhiteSpace(value) then None
                    else if value = "Yes" then Some CPIValue.Yes
                    else if value = "No" then Some CPIValue.No
                    else Some (CPIValue.Other value)
                WelshCPI =
                    let value = sheet.Cell("B39").GetString()
                    if System.String.IsNullOrWhiteSpace(value) then None
                    else if value = "Yes" then Some CPIValue.Yes
                    else if value = "No" then Some CPIValue.No
                    else Some (CPIValue.Other value)
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