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
]

[<Tests>]
let all = testList "All" [ Shared.Tests.shared; server; excel ]

[<EntryPoint>]
let main _ = runTestsWithCLIArgs [] [||] all