module Server

open SAFE
open Saturn
open Shared
open OfficeOpenXml // Ensure you have this for EPPlus
open System
open System.IO
open Microsoft.Data.Sqlite
open Giraffe
open Microsoft.AspNetCore.Http
open System.Text.RegularExpressions

// Add this module for logging
module Logger =
    let logError (ex: exn) (ctx: HttpContext) =
        let timestamp = DateTime.UtcNow.ToString("yyyy-MM-dd HH:mm:ss")
        let path = ctx.Request.Path.ToString()
        let errorMessage =
            sprintf "[%s] Error in %s: %s\nStack trace: %s"
                timestamp path ex.Message ex.StackTrace

        eprintfn "%s" errorMessage
        File.AppendAllText("server_errors.log", errorMessage + "\n\n")

module CagRegisterXLSM =
    let getExcelLastModified (package: ExcelPackage) =
        try
            // Try to get the document properties
            let props = package.Workbook.Properties
            // Use the last modified date from Excel if available, otherwise fall back to created date
            if props.Modified <> DateTime.MinValue then
                props.Modified
            elif props.Created <> DateTime.MinValue then
                props.Created
            else
                DateTime.MinValue  // Return minimum date if neither is available
        with _ ->
            DateTime.MinValue

    let findLatestExcelFile directory registerType =
        let files =
            Directory.GetFiles(directory, "*.xls*")
            |> Array.filter (fun f ->
                let isExcelFile = [".xlsm"; ".xlsx"; ".xls"] |> List.exists (fun ext -> f.EndsWith(ext, StringComparison.OrdinalIgnoreCase))
                let filename = Path.GetFileName(f).ToLowerInvariant()
                let isCorrectType =
                    match registerType with
                    | NonResearch -> filename.Contains("non-research")
                    | Research -> filename.Contains("research") && not (filename.Contains("non-research"))
                isExcelFile && isCorrectType)
            |> Array.map (fun f -> FileInfo(f))
            |> Array.toList

        let rec tryLoadFile (remainingFiles: FileInfo list) (failedFiles: string list) =
            match remainingFiles with
            | [] -> Error (failedFiles |> List.rev)
            | file :: rest ->
                try
                    use package = new ExcelPackage(file)
                    // Try to access first worksheet to verify file is readable
                    let _ = package.Workbook.Worksheets.[0]
                    let lastModified =
                        let excelDate = getExcelLastModified package
                        if excelDate = DateTime.MinValue then
                            file.LastWriteTime  // Fall back to filesystem date
                        else
                            excelDate
                    Ok {
                        LoadedFile = file.Name
                        LoadedDate = lastModified
                        RegisterType = registerType
                        FailedFiles = failedFiles |> List.rev
                    }
                with ex ->
                    printfn "Failed to load %s: %s" file.Name ex.Message
                    tryLoadFile rest (file.Name :: failedFiles)

        // Sort files by filesystem date initially, then try loading each
        let sortedFiles =
            files
            |> List.sortByDescending (fun f -> f.LastWriteTime)

        tryLoadFile sortedFiles []

    let mutable currentLoadResult = None

    let getRegisterFilePath registerType =
        let directory =
            Environment.GetEnvironmentVariable("CAG_REGISTER_FILE_PATH")
            |> Option.ofObj
            |> Option.defaultValue "."
            |> fun path -> if Directory.Exists(path) then path else "."

        match findLatestExcelFile directory registerType with
        | Ok result ->
            currentLoadResult <- Some { result with RegisterType = registerType }
            Path.Combine(directory, result.LoadedFile)
        | Error failures ->
            failwithf "No valid Excel file found in directory. Tried files: %A" failures

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
        let validValues = ["Y"; "X"; "y"; "x"]
        let purposes =
            let addPurposeIfContainsYorX cellRef purpose =
                let cellText = sheet.Cells.[cellRef].Text.Trim()
                let purposeValue = if validValues |> List.exists (fun value -> cellText.Contains(value)) then Some purpose else None

                (purposeValue, (if purposeValue.IsSome then Some cellText else None), (if purposeValue.IsNone then Some cellText else None))
            [
                addPurposeIfContainsYorX "B18" MedicalPurpose.MedicalResearch
                addPurposeIfContainsYorX "B16" MedicalPurpose.PreventativeMedicine
                addPurposeIfContainsYorX "B17" MedicalPurpose.MedicalDiagnosis
                addPurposeIfContainsYorX "B19" MedicalPurpose.CareAndTreatment
                addPurposeIfContainsYorX "B20" MedicalPurpose.HealthAndSocialCareManagement
                addPurposeIfContainsYorX "B21" MedicalPurpose.InformingIndividuals
            ]
            |> fun purposes ->
                let actualValues = Set.ofList (List.map (fun (purposeValue, _, _) -> purposeValue) purposes |> List.choose id)
                let rawMatchedValues = List.map (fun (_, rawValue, _) -> rawValue) purposes |> List.choose id
                let rawNotMatchedValues = List.map (fun (_, _, rawValue) -> rawValue) purposes |> List.choose id
                (actualValues, rawMatchedValues, rawNotMatchedValues)

        purposes
    let parseS251Classes (sheet: ExcelWorksheet) =
        let validValues = ["Y"; "X"; "y"; "x"]
        let classes =
            let addClassIfContainsYorX cellRef s251Class =
                let cellText = sheet.Cells.[cellRef].Text.Trim()
                let classValue = if validValues |> List.exists (fun value -> cellText.Contains(value)) then Some s251Class else None
                (classValue, (if classValue.IsSome then Some cellText else None), (if classValue.IsNone then Some cellText else None))
            [
                addClassIfContainsYorX "B24" S251Class.SpecificSupport
                addClassIfContainsYorX "B25" S251Class.ClassI_Identifiability
                addClassIfContainsYorX "B26" S251Class.ClassII_GeographicalLocation
                addClassIfContainsYorX "B27" S251Class.ClassIII_IdentifyAndContact
                addClassIfContainsYorX "B28" S251Class.ClassIV_LinkingMultipleSources
                addClassIfContainsYorX "B29" S251Class.ClassV_AuditAndMonitoring
                addClassIfContainsYorX "B30" S251Class.ClassVI_GrantingAccess
            ]
            |> fun classes ->
                let actualValues = Set.ofList (List.map (fun (classValue, _, _) -> classValue) classes |> List.choose id)
                let rawMatchedValues = List.map (fun (_, rawValue, _) -> rawValue) classes |> List.choose id
                let rawNotMatchedValues = List.map (fun (_, _, rawValue) -> rawValue) classes |> List.choose id
                (actualValues, rawMatchedValues, rawNotMatchedValues)

        classes

    let getHiddenRowStatus (indexSheet: ExcelWorksheet) (appNo: string) =
        [2..indexSheet.Dimension.End.Row]
        |> List.tryFind (fun row ->
            indexSheet.Cells.[row, 1].Text = appNo
        )
        |> Option.map (fun row -> indexSheet.Row(row).Hidden)
        |> Option.map (fun isHidden -> if isHidden then Obsolete else Active)
        |> Option.defaultValue Active

    let getCellReferences registerType =
        match registerType with
        | Research ->
            {|
                OutcomeDate = "B34"
                NextReviewDate = "B35"
                Notes = "B36"
                NDOO = "B37"
                EnglishCPI = "B38"
                WelshCPI = "B39"
            |}
        | NonResearch ->
            {|
                OutcomeDate = "B33"  // One row up for non-research
                NextReviewDate = "B34"
                Notes = "B35"
                NDOO = "B36"
                EnglishCPI = "B37"
                WelshCPI = "B38"
            |}

    let parseApplication (sheet: ExcelWorksheet) (indexSheet: ExcelWorksheet) registerType =
        try
            let appNo = sheet.Cells.["B3"].Text
            let appStatus = getHiddenRowStatus indexSheet appNo
            let cellRefs = getCellReferences registerType
            let app = {
                ApplicationNumber = { RegisterType = registerType; ApplicationNumber = appNo }
                Reference = sheet.Cells.["B4"].Text
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
                MedicalPurposes = parseMedicalPurposes sheet |> (fun (values, _, _) -> values) // Corrected to use only the first part of the tuple
                MedicalPurposesRawValues = parseMedicalPurposes sheet |> (fun (_, rawValues, _) -> rawValues) // Added to include raw values
                MedicalPurposesRawValuesNotChecked = parseMedicalPurposes sheet |> (fun (_, _, rawValues) -> rawValues) // Added to include raw values
                CohortDescription = sheet.Cells.["B22"].Text // Use string access
                ConfidentialInfo = sheet.Cells.["B23"].Text // Use string access
                S251Classes = parseS251Classes sheet |> (fun (values, _, _) -> values) // Corrected to use only the first part of the tuple
                S251ClassRawValues = parseS251Classes sheet |> (fun (_, rawValues, _) -> rawValues) // Added to include raw values
                S251ClassRawValuesNotChecked = parseS251Classes sheet |> (fun (_, _, rawValues) -> rawValues) // Added to include raw values
                Sponsor = sheet.Cells.["B31"].Text // Use string access
                Status = sheet.Cells.["B32"].Text // Use string access
                OutcomeDate =
                    try Some(sheet.Cells.[cellRefs.OutcomeDate].GetValue<System.DateTime>())
                    with _ -> None
                NextReviewDate =
                    try Some(sheet.Cells.[cellRefs.NextReviewDate].GetValue<System.DateTime>())
                    with _ -> None
                Notes = sheet.Cells.[cellRefs.Notes].Text
                NDOO =
                    let value = sheet.Cells.[cellRefs.NDOO].Text
                    if System.String.IsNullOrWhiteSpace(value) then None else Some value
                EnglishCPI =
                    let value = sheet.Cells.[cellRefs.EnglishCPI].Text
                    if System.String.IsNullOrWhiteSpace(value) then None
                    else if value = "Yes" then Some CPIValue.Yes
                    else if value = "No" then Some CPIValue.No
                    else Some (CPIValue.Other value)
                WelshCPI =
                    let value = sheet.Cells.[cellRefs.WelshCPI].Text
                    if System.String.IsNullOrWhiteSpace(value) then None
                    else if value = "Yes" then Some CPIValue.Yes
                    else if value = "No" then Some CPIValue.No
                    else Some (CPIValue.Other value)
                ApplicationStatus = appStatus
                RelatedMinutes = []
            }
            Some app
        with ex ->
            printfn "Error parsing application: %s" ex.Message
            None

    let getApplicationDetails registerType =
        use package = new ExcelPackage(new FileInfo(getRegisterFilePath registerType))
        let indexSheet = package.Workbook.Worksheets.[0]
        let applications = getApplications(package)

        let tasks =
            applications
            |> List.map (fun (appNo, sheet) ->
                async {
                    return parseApplication sheet indexSheet registerType
                }
            )

        let results = Async.Parallel tasks |> Async.RunSynchronously
        results |> Array.choose id |> List.ofArray

    let getFrontPageColumnIndices registerType =
        match registerType with
        | Research ->
            {|
                Reference = 2
                Title = 3
                Status = 4
                OutcomeDate = Some 5
                NextReviewDate = 6
                Contact = 7
                Organisation = 8
                NDOO = 9
                EnglishCPI = 10
                WelshCPI = 11
            |}
        | NonResearch ->
            {|
                Reference = 2
                Title = 3
                Status = 4
                OutcomeDate = None  // No outcome date for non-research
                NextReviewDate = 5  // Shifted up by one
                Contact = 6
                Organisation = 7
                NDOO = 8
                EnglishCPI = 9
                WelshCPI = 10
            |}

    let getFrontPageEntries registerType =
        use workbook = new ExcelPackage(new FileInfo(getRegisterFilePath registerType))
        let indexSheet = workbook.Workbook.Worksheets.[0]
        let columns = getFrontPageColumnIndices registerType

        [2..indexSheet.Dimension.End.Row]
        |> List.choose (fun row ->
            let appNo = indexSheet.Cells.[row, 1].Text
            if System.String.IsNullOrWhiteSpace(appNo) then None
            else
                try
                    let isHidden = indexSheet.Row(row).Hidden
                    Some {
                        ApplicationNumber = { RegisterType = registerType; ApplicationNumber = appNo }
                        Reference = indexSheet.Cells.[row, columns.Reference].Text
                        Title = indexSheet.Cells.[row, columns.Title].Text
                        Status = indexSheet.Cells.[row, columns.Status].Text
                        OutcomeDate =
                            match columns.OutcomeDate with
                            | Some col ->
                                try Some(indexSheet.Cells.[row, col].GetValue<System.DateTime>())
                                with _ -> None
                            | None -> None
                        NextReviewDate =
                            try Some(indexSheet.Cells.[row, columns.NextReviewDate].GetValue<System.DateTime>())
                            with _ -> None
                        Contact = indexSheet.Cells.[row, columns.Contact].Text
                        Organisation = indexSheet.Cells.[row, columns.Organisation].Text
                        NationalDataOptOutStatus =
                            let value = indexSheet.Cells.[row, columns.NDOO].Text
                            if System.String.IsNullOrWhiteSpace(value) then None
                            else Some value
                        EnglishConfidentialPatientInfo =
                            match indexSheet.Cells.[row, columns.EnglishCPI].Text.ToLowerInvariant() with
                            | "yes" -> Some true
                            | "no" -> Some false
                            | _ -> None
                        WelshConfidentialPatientInfo =
                            match indexSheet.Cells.[row, columns.WelshCPI].Text.ToLowerInvariant() with
                            | "yes" -> Some true
                            | "no" -> Some false
                            | _ -> None
                        ApplicationStatus = if isHidden then Obsolete else Active
                    }
                with ex ->
                    printfn "Error parsing front page row %d: %s" row ex.Message
                    None
        )

    let getDiscrepancies registerType =
        let frontPage = getFrontPageEntries registerType
        let details = getApplicationDetails registerType

        frontPage
        |> List.choose (fun front ->
            details
            |> List.tryFind (fun detail -> detail.ApplicationNumber = front.ApplicationNumber)
            |> Option.map (fun detail ->
                let differences = {
                    Title =
                        let detailTitleFirstLine = detail.Title.Trim().Split('\n').[0]
                        let frontPageTitleFirstLine = front.Title.Trim().Split('\n').[0]
                        detailTitleFirstLine <> frontPageTitleFirstLine

                    Status =
                        detail.Status.Trim() <> front.Status.Trim()

                    OutcomeDate =
                        detail.OutcomeDate <> front.OutcomeDate

                    NextReviewDate =
                        detail.NextReviewDate <> front.NextReviewDate

                    Contact =
                        detail.ContactName.Trim() <> front.Contact.Trim()

                    Organisation =
                        detail.ApplicantOrganisation.Trim() <> front.Organisation.Trim()

                    NationalDataOptOutStatus =
                        detail.NDOO <> front.NationalDataOptOutStatus

                    EnglishConfidentialPatientInfo =
                        match detail.EnglishCPI, front.EnglishConfidentialPatientInfo with
                        | Some CPIValue.Yes, Some true
                        | Some CPIValue.No, Some false -> false
                        | _ -> true

                    WelshConfidentialPatientInfo =
                        match detail.WelshCPI, front.WelshConfidentialPatientInfo with
                        | Some CPIValue.Yes, Some true
                        | Some CPIValue.No, Some false -> false
                        | _ -> true
                }

                {
                    ApplicationNumber = front.ApplicationNumber
                    FrontPage = front
                    Detail = detail
                    Differences = differences
                }
            )
        )

    let getCurrentLoadResult registerType =
        currentLoadResult |> Option.filter (fun r -> r.RegisterType = registerType)

    let getApplicationDisplayNames registerType =
        printfn "Getting display names for %A" registerType
        use package = new ExcelPackage(new FileInfo(getRegisterFilePath registerType))
        let indexSheet = package.Workbook.Worksheets.[0]
        let columns = getFrontPageColumnIndices(registerType)

        let displayNames =
            [2..indexSheet.Dimension.End.Row]
            |> List.choose (fun row ->
                let appNo = indexSheet.Cells.[row, 1].Text
                if System.String.IsNullOrWhiteSpace(appNo) then None
                else
                    let title = indexSheet.Cells.[row, columns.Title].Text
                    let shortTitle =
                        if String.IsNullOrWhiteSpace(title) then appNo
                        else
                            let firstLine = title.Split('\n').[0].Trim()
                            if firstLine.Length > 50 then
                                firstLine.Substring(0, 47) + "..."
                            else firstLine
                    let displayName = sprintf "%s: %s" appNo shortTitle
                    printfn "Created display name for %s: %s" appNo displayName
                    Some (appNo, displayName)
            )
            |> Map.ofList

        printfn "Returning %d display names for %A" (Map.count displayNames) registerType
        displayNames


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

module MinuteParsing =
    let parseMinuteType (title: string) =
        let lowerTitle = title.ToLowerInvariant()
        if lowerTitle.Contains("precedent") then MinuteType.PrecedentSet
        elif lowerTitle.Contains("subcommittee") || lowerTitle.Contains("sub-committee") then MinuteType.Subcommittee
        elif lowerTitle.Contains("cag meeting") || lowerTitle.Contains("full cag") then MinuteType.Full
        else MinuteType.Other title

    let parseDate (title: string) =
        try
            // Try to match various date formats
            let patterns = [
                // Full date patterns
                @"(\d{1,2})(?:st|nd|rd|th)?\s+(Jan(?:uary)?|Feb(?:ruary)?|Mar(?:ch)?|Apr(?:il)?|May|Jun(?:e)?|Jul(?:y)?|Aug(?:ust)?|Sep(?:tember)?|Oct(?:ober)?|Nov(?:ember)?|Dec(?:ember)?)\s+(\d{4})"
                @"(Jan(?:uary)?|Feb(?:ruary)?|Mar(?:ch)?|Apr(?:il)?|May|Jun(?:e)?|Jul(?:y)?|Aug(?:ust)?|Sep(?:tember)?|Oct(?:ober)?|Nov(?:ember)?|Dec(?:ember)?)\s+(\d{1,2})(?:st|nd|rd|th)?\s+(\d{4})"
                // Month and year only
                @"(Jan(?:uary)?|Feb(?:ruary)?|Mar(?:ch)?|Apr(?:il)?|May|Jun(?:e)?|Jul(?:y)?|Aug(?:ust)?|Sep(?:tember)?|Oct(?:ober)?|Nov(?:ember)?|Dec(?:ember)?)\s+(\d{4})"
            ]

            let monthMap =
                [("jan", 1); ("feb", 2); ("mar", 3); ("apr", 4); ("may", 5); ("jun", 6);
                 ("jul", 7); ("aug", 8); ("sep", 9); ("oct", 10); ("nov", 11); ("dec", 12)]
                |> Map.ofList

            let getMonthNumber (month: string) =
                let key = month.ToLower().Substring(0, 3)
                monthMap.[key]

            let mutable result = None
            for pattern in patterns do
                if result.IsNone then  // Only continue if we haven't found a match
                    let m = Regex.Match(title, pattern, RegexOptions.IgnoreCase)
                    if m.Success then
                        result <-
                        match m.Groups.Count with
                                 | 4 -> // Full date with day-month-year
                                     let day =
                                         if m.Groups.[1].Value.Length > 0 then
                                             int m.Groups.[1].Value
                                         else
                                             int m.Groups.[2].Value
                                     let month =
                                         if m.Groups.[1].Value.Length > 0 then
                                             getMonthNumber m.Groups.[2].Value
                                         else
                                             getMonthNumber m.Groups.[1].Value
                                     let year =
                                         if m.Groups.[1].Value.Length > 0 then
                                             int m.Groups.[3].Value
                                         else
                                             int m.Groups.[3].Value
                                     Some(DateTime(year, month, day))
                                 | 3 -> // Month and year only
                                     let month = getMonthNumber m.Groups.[1].Value
                                     let year = int m.Groups.[2].Value
                                     Some(DateTime(year, month, 1))
                                 | _ -> None
            result
        with _ ->
            None

let applicationsApi ctx = {
    getApplications = fun registerType -> async {
        let apps = CagRegisterXLSM.getApplicationDetails registerType

        // Get related minutes for each application
        let appsWithMinutes =
            apps |> List.map (fun app ->

                let file = Environment.GetEnvironmentVariable("CAG_REGISTER_DB_PATH")
                        |> Option.ofObj
                        |> Option.defaultValue "minutes.db"
                use connection = new SqliteConnection($"Data Source={file}")
                connection.Open()

                use command = connection.CreateCommand()
                command.CommandText <- """
                    SELECT m.title, m.url, c.PageRanges, c.ProcessedDate
                    FROM CagReferences c
                    LEFT JOIN minutes m ON m.url = c.PdfUrl
                    WHERE c.CagId = @appId
                    ORDER BY m.CreatedAt DESC;
                """
                command.Parameters.AddWithValue("@appId", app.Reference) |> ignore

                use reader = command.ExecuteReader()
                let minutes = [
                    while reader.Read() do
                        let title = reader.GetString(0)
                        yield {
                            Title = title
                            Url = reader.GetString(1)
                            PageRanges = reader.GetString(2).Split(',')
                                            |> Array.map (fun s -> s.Trim().Trim('p'))
                            ProcessedDate = new DateTime(reader.GetInt64(3))
                            Type = MinuteParsing.parseMinuteType title
                            MeetingDate = MinuteParsing.parseDate title
                        }
                ]
                let orderedMinutes = minutes |> List.sortBy (fun m -> m.MeetingDate)
                { app with RelatedMinutes = orderedMinutes }
            )

        return { RegisterType = registerType; Applications = appsWithMinutes }
    }
    getFrontPageEntries = fun registerType -> async {
        let entries = CagRegisterXLSM.getFrontPageEntries registerType
        return { RegisterType = registerType; Entries = entries }
    }
    getDiscrepancies = fun registerType -> async {
        let discrepancies = CagRegisterXLSM.getDiscrepancies registerType
        return { RegisterType = registerType; Discrepancies = discrepancies }
    }
    getFileLoadResult = fun registerType -> async {
        return { RegisterType = registerType; FileLoadResult = CagRegisterXLSM.getCurrentLoadResult registerType }
    }
    getApplicationDisplayNames = fun registerType -> async {
        let displayNames = CagRegisterXLSM.getApplicationDisplayNames registerType
        return { RegisterType = registerType; ApplicationDisplayNames = displayNames }
    }
}

let webApp =
    router {
        forward "/todos" (Api.make todosApi)
        forward "/ICagApplicationsApi" (Api.make applicationsApi)
    }

open Microsoft.Extensions.Logging
let app = application {
    use_router (Api.make(
        api = applicationsApi,
        errorHandler = (fun ex _ ->
            printfn "An error occurred processing your request: %s" ex.Message;
            (Fable.Remoting.Server.Ignore)
        )
    ))
    memory_cache
    use_static "public"
    use_gzip
}

[<EntryPoint>]
let main _ =
    ExcelPackage.LicenseContext <- LicenseContext.NonCommercial // Set the license context
    run app
    0