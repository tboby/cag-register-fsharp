module Server.Tests

open Expecto

open Shared
open Server

open OfficeOpenXml
open Shared.CagReference // Ensure you have this for EPPlus
do ExcelPackage.LicenseContext <- LicenseContext.NonCommercial // Set the license context


let excel = testList "Excel" [
    testCase "Can parse application details"
    <| fun _ ->
        let applications = CagRegisterXLSM.getApplicationDetails(RegisterType.Research)

        // Should return some applications
        Expect.isGreaterThan applications.Length 0 "Should parse some applications"

        // Test first application has required fields
        let firstApp = applications.[0]
        Expect.isTrue (not (System.String.IsNullOrWhiteSpace(firstApp.ApplicationNumber.ApplicationNumber))) "Should have application number"
        Expect.isTrue (not (System.String.IsNullOrWhiteSpace(firstApp.Title))) "Should have title"
        Expect.isTrue (firstApp.Address.Length > 0) "Should have address"

    testCase "All applications have some category of support and medical purpose"
    <| fun _ ->
        let applications = CagRegisterXLSM.getApplicationDetails(RegisterType.Research)

        // Test that all applications have at least one category of support and one medical purpose
        applications
        |> List.iter (fun app ->
            Expect.isTrue (app.S251Classes.Count > 0) (sprintf "Application %s must have at least one category of support" app.ApplicationNumber.ApplicationNumber)
            Expect.isTrue (app.MedicalPurposes.Count > 0) (sprintf "Application %s must have at least one medical purpose" app.ApplicationNumber.ApplicationNumber)
        )

    ftestCase "No applications should be Unknown reference type"
    <| fun _ ->
        let applications = CagRegisterXLSM.getApplicationDetails(RegisterType.Research)

        // Test that all applications have at least one category of support and one medical purpose
        applications
        |> List.iter (fun app ->
            Expect.isTrue (app.Reference |> function |{parsed = Unknown _} -> false |_ -> true) (sprintf "Application %s with ref %s failed parse" app.ApplicationNumber.ApplicationNumber (app.Reference |> function |{parsed = Unknown x} -> x |_ -> ""))
        )

    testCase "Retrieve all raw values of MedicalPurposes and S251Classes"
    <| fun _ ->
        let applications = CagRegisterXLSM.getApplicationDetails(RegisterType.Research)

        // Collect all MedicalPurposes and S251Classes
        let allMedicalPurposes =
            applications
            |> List.collect (fun app -> app.MedicalPurposesRawValues |> List.map (fun mp -> mp, app.ApplicationNumber))
            |> List.distinctBy fst
        let allS251Classes =
            applications
            |> List.collect (fun app -> app.S251ClassRawValues |> List.map (fun s251 -> s251, app.ApplicationNumber))
            |> List.distinctBy fst
        let allMedicalPurposesNotChecked =
            applications
            |> List.collect (fun app -> app.MedicalPurposesRawValuesNotChecked |> List.map (fun mp -> mp, app.ApplicationNumber))
            |> List.distinctBy fst
        let allS251ClassesNotChecked =
            applications
            |> List.collect (fun app -> app.S251ClassRawValuesNotChecked |> List.map (fun s251 -> s251, app.ApplicationNumber))
            |> List.distinctBy fst
        let allCPIOtherValues =
            applications
            |> List.collect (fun app -> app.WelshCPI |> function | Some (CPIValue.Other x) -> [x] | _ -> [])
            |> List.distinct

        // Output the results (you can replace this with any assertion or logging as needed)
        printfn "All Medical Purposes: %A" allMedicalPurposes
        printfn "All S251 Classes: %A" allS251Classes
        printfn "All Medical Purposes Not Checked: %A" allMedicalPurposesNotChecked
        printfn "All S251 Classes Not Checked: %A" allS251ClassesNotChecked
        printfn "All CPI Other Values: %A" allCPIOtherValues

    testCase "Can parse front page entries"
    <| fun _ ->
        let entries = CagRegisterXLSM.getApplicationDetails(RegisterType.Research)

        // Should return some entries
        Expect.isGreaterThan entries.Length 0 "Should parse some front page entries"

        // Test first entry has required fields
        let firstEntry = entries.[0]
        Expect.isTrue (not (System.String.IsNullOrWhiteSpace(firstEntry.ApplicationNumber.ApplicationNumber))) "Should have application number"
        // Expect.isTrue (not (System.String.IsNullOrWhiteSpace(firstEntry.Reference))) "Should have reference"
        Expect.isTrue (not (System.String.IsNullOrWhiteSpace(firstEntry.Title))) "Should have title"

    testCase "Front page entries correlate with detailed entries"
    <| fun _ ->
        let frontPageEntries = CagRegisterXLSM.getFrontPageEntries(RegisterType.Research)
        let detailedEntries = CagRegisterXLSM.getApplicationDetails(RegisterType.Research)

        // Check that every front page entry has a corresponding detailed entry
        frontPageEntries |> List.iter (fun frontPage ->
            let matching = detailedEntries |> List.tryFind (fun detail ->
                detail.ApplicationNumber = frontPage.ApplicationNumber)

            Expect.isSome matching
                (sprintf "Application %s should have a detailed entry" frontPage.ApplicationNumber.ApplicationNumber)

            matching |> Option.iter (fun detail ->
                let detailTitleFirstLine = detail.Title.Trim().Split('\n').[0]
                let frontPageTitleFirstLine = frontPage.Title.Trim().Split('\n').[0]
                Expect.equal detailTitleFirstLine frontPageTitleFirstLine
                    (sprintf "Title should match for application %s" frontPage.ApplicationNumber.ApplicationNumber)
                Expect.equal (detail.Status.Trim()) (frontPage.Status.Trim())
                    (sprintf "Status should match for application %s" frontPage.ApplicationNumber.ApplicationNumber)
            )
        )
]

[<Tests>]
let all = testList "All" [ Shared.Tests.shared;  excel ]

[<EntryPoint>]
let main _ = runTestsWithCLIArgs [] [||] all