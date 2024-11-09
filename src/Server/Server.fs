module Server

open SAFE
open Saturn
open Shared
open OfficeOpenXml // Ensure you have this for EPPlus

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

    let getApplications(workbook: ExcelPackage) =
        let indexSheet = workbook.Workbook.Worksheets.[0] // Access the first worksheet

        // Get all application numbers from Index sheet
        let applications =
            [2..indexSheet.Dimension.End.Row] // Change to get all rows dynamically
            |> List.map (fun row ->
                let appNo = indexSheet.Cells.[sprintf "A%d" row].Text // Use string format for cell access
                appNo
            )
            |> List.filter (fun appNo -> not (System.String.IsNullOrWhiteSpace(appNo)))

        // Read each application's sheet
        let appSheets =
            applications
            |> List.map (fun appNo ->
                let sheetName = "A" + appNo
                try
                    let sheet = workbook.Workbook.Worksheets.[sheetName] // Access the application sheet
                    Some (appNo, sheet)
                with
                | _ -> None
            )
            |> List.choose id

        appSheets

    let parseMedicalPurposes (sheet: ExcelWorksheet) =
        [
            if sheet.Cells.["B18"].Text.Contains("Y") then MedicalPurpose.MedicalResearch
            if sheet.Cells.["B16"].Text.Contains("Y") then MedicalPurpose.PreventativeMedicine
            if sheet.Cells.["B17"].Text.Contains("Y") then MedicalPurpose.MedicalDiagnosis
            if sheet.Cells.["B19"].Text.Contains("Y") then MedicalPurpose.CareAndTreatment
            if sheet.Cells.["B20"].Text.Contains("Y") then MedicalPurpose.HealthAndSocialCareManagement
            if sheet.Cells.["B21"].Text.Contains("Y") then MedicalPurpose.InformingIndividuals
        ]

    let parseS251Classes (sheet: ExcelWorksheet) =
        [
            if sheet.Cells.["B24"].Text.Contains("Y") then S251Class.SpecificSupport
            if sheet.Cells.["B25"].Text.Contains("Y") then S251Class.ClassI_Identifiability
            if sheet.Cells.["B26"].Text.Contains("Y") then S251Class.ClassII_GeographicalLocation
            if sheet.Cells.["B27"].Text.Contains("Y") then S251Class.ClassIII_IdentifyAndContact
            if sheet.Cells.["B28"].Text.Contains("Y") then S251Class.ClassIV_LinkingMultipleSources
            if sheet.Cells.["B29"].Text.Contains("Y") then S251Class.ClassV_AuditAndMonitoring
            if sheet.Cells.["B30"].Text.Contains("Y") then S251Class.ClassVI_GrantingAccess
        ]

    let parseApplication (sheet: ExcelWorksheet) =
        try
            let app = {
                ApplicationNumber = sheet.Cells.["B3"].Text // Use string access
                Reference = sheet.Cells.["B4"].Text // Use string access
                OtherRefs =
                    let refs = sheet.Cells.["B5"].Text // Use string access
                    if System.String.IsNullOrWhiteSpace(refs) then None else Some refs
                Title = sheet.Cells.["B6"].Text // Use string access
                Summary = sheet.Cells.["B7"].Text // Use string access
                ApplicantOrganisation = sheet.Cells.["B8"].Text // Use string access
                ContactName = sheet.Cells.["B9"].Text // Use string access
                Address =
                    [
                        sheet.Cells.["B10"].Text // Use string access
                        sheet.Cells.["B11"].Text // Use string access
                        sheet.Cells.["B12"].Text // Use string access
                    ] |> List.filter (not << System.String.IsNullOrWhiteSpace)
                Postcode = sheet.Cells.["B13"].Text // Use string access
                Telephone = sheet.Cells.["B14"].Text // Use string access
                Email = sheet.Cells.["B15"].Text // Use string access
                MedicalPurposes = parseMedicalPurposes sheet
                CohortDescription = sheet.Cells.["B22"].Text // Use string access
                ConfidentialInfo = sheet.Cells.["B23"].Text // Use string access
                S251Classes = parseS251Classes sheet
                Sponsor = sheet.Cells.["B31"].Text // Use string access
                Status = sheet.Cells.["B32"].Text // Use string access
                OutcomeDate =
                    try Some(sheet.Cells.["B34"].GetValue<System.DateTime>()) // Use string access
                    with _ -> None
                NextReviewDate =
                    try Some(sheet.Cells.["B35"].GetValue<System.DateTime>()) // Use string access
                    with _ -> None
                Notes = sheet.Cells.["B36"].Text // Use string access
                NDOO =
                    let value = sheet.Cells.["B37"].Text // Use string access
                    if System.String.IsNullOrWhiteSpace(value) then None else Some value
                EnglishCPI =
                    let value = sheet.Cells.["B38"].Text // Use string access
                    if System.String.IsNullOrWhiteSpace(value) then None
                    else if value = "Yes" then Some CPIValue.Yes
                    else if value = "No" then Some CPIValue.No
                    else Some (CPIValue.Other value)
                WelshCPI =
                    let value = sheet.Cells.["B39"].Text // Use string access
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
        use package = new ExcelPackage(new FileInfo(cagRegisterFile)) // Change to EPPlus syntax
        let applications = getApplications(package)

        // Read application details in parallel
        let tasks =
            applications
            |> List.map (fun (appNo, sheet) ->
                async {
                    return parseApplication sheet
                }
            )

        // Run tasks in parallel and collect results
        let results = Async.Parallel tasks |> Async.RunSynchronously
        results |> Array.choose id |> List.ofArray // Filter out None values

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
    ExcelPackage.LicenseContext <- LicenseContext.NonCommercial // Set the license context
    run app
    0