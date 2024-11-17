module Server.Tests

open Expecto

open Shared
open Server

open OfficeOpenXml // Ensure you have this for EPPlus
do ExcelPackage.LicenseContext <- LicenseContext.NonCommercial // Set the license context
let server =
    testList "Server" [
        testCase "Adding valid Todo"
        <| fun _ ->
            let validTodo = Todo.create "TODO"
            let expectedResult = Ok()

            let result = Storage.addTodo validTodo

            Expect.equal result expectedResult "Result should be ok"
            Expect.contains Storage.todos validTodo "Storage should contain new todo"
    ]

let excel = testList "Excel" [
    testCase "Can parse application details"
    <| fun _ ->
        let applications = CagRegisterXLSM.getApplicationDetails()

        // Should return some applications
        Expect.isGreaterThan applications.Length 0 "Should parse some applications"

        // Test first application has required fields
        let firstApp = applications.[0]
        Expect.isTrue (not (System.String.IsNullOrWhiteSpace(firstApp.ApplicationNumber))) "Should have application number"
        Expect.isTrue (not (System.String.IsNullOrWhiteSpace(firstApp.Title))) "Should have title"
        Expect.isTrue (firstApp.Address.Length > 0) "Should have address"

    testCase "All applications have some category of support and medical purpose"
    <| fun _ ->
        let applications = CagRegisterXLSM.getApplicationDetails()

        // Test that all applications have at least one category of support and one medical purpose
        applications
        |> List.iter (fun app ->
            Expect.isTrue (app.S251Classes.Count > 0) (sprintf "Application %s must have at least one category of support" app.ApplicationNumber)
            Expect.isTrue (app.MedicalPurposes.Count > 0) (sprintf "Application %s must have at least one medical purpose" app.ApplicationNumber)
        )

    testCase "Retrieve all raw values of MedicalPurposes and S251Classes"
    <| fun _ ->
        let applications = CagRegisterXLSM.getApplicationDetails()

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

        // Output the results (you can replace this with any assertion or logging as needed)
        printfn "All Medical Purposes: %A" allMedicalPurposes
        printfn "All S251 Classes: %A" allS251Classes
        printfn "All Medical Purposes Not Checked: %A" allMedicalPurposesNotChecked
        printfn "All S251 Classes Not Checked: %A" allS251ClassesNotChecked
]

[<Tests>]
let all = testList "All" [ Shared.Tests.shared; server; excel ]

[<EntryPoint>]
let main _ = runTestsWithCLIArgs [] [||] all