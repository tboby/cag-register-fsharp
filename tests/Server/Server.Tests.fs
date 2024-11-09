module Server.Tests

open Expecto

open Shared
open Server

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
     testCase "Excel extraction returns applications"
        <| fun _ ->
            let applications = CagRegisterXLSM.getApplications()
            
            // Should return some applications
            Expect.isGreaterThan applications.Length 0 "Should find some applications"
            
            // Each application should have a number and worksheet
            applications |> List.iter (fun (appNo, sheet) ->
                Expect.isTrue (not (System.String.IsNullOrWhiteSpace(appNo))) "Application number should not be empty"
                Expect.isNotNull sheet "Worksheet should exist"
                Expect.equal sheet.Name ("A" + appNo) "Sheet name should match application number"
            )
]

[<Tests>]
let all = testList "All" [ Shared.Tests.shared; server; excel ]

[<EntryPoint>]
let main _ = runTestsWithCLIArgs [] [||] all