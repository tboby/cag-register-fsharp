// For more information see https://aka.ms/fsharp-console-apps
// Create a function to dump API responses to files

open System.IO
open System.Net.Http
open System.Text
open Fable.Remoting.DotnetClient
open Feliz.Router
open Shared

// Configuration
let baseUrl = "http://localhost:5000/api/ICagApplicationsApi"
let outputDir = "./api-responses"

// Create output directory if it doesn't exist
Directory.CreateDirectory(outputDir) |> ignore

// Define API endpoints to call
let endpoints = [
    "getApplications"
    "getFrontPageEntries"
    "getDiscrepancies"
    "getFileLoadResult"
    "getApplicationDisplayNames"
]

// Function to make POST request and save response
let callAndSaveEndpoint (endpoint: string) (registerParam: string) =
    async {
        use client = new HttpClient()
        let url = $"{baseUrl}/{endpoint}"

        // Create JSON payload with the register parameter
        let jsonContent = $"[\"{registerParam}\"]"
        let content = new StringContent(jsonContent, Encoding.UTF8, "application/json")

        printfn "Calling %s..." url

        try
            let! response = client.PostAsync(url, content) |> Async.AwaitTask
            response.EnsureSuccessStatusCode() |> ignore

            let! responseBody = response.Content.ReadAsStringAsync() |> Async.AwaitTask
            let outputPath = Path.Combine(outputDir, $"{endpoint}-{registerParam}.json")

            File.WriteAllText(outputPath, responseBody)
            printfn "Response saved to %s" outputPath

            return true
        with ex ->
            printfn "Error calling %s: %s" endpoint ex.Message
            return false
    }

// Call all endpoints and wait for completion
endpoints
    |> List.collect (fun x -> [ callAndSaveEndpoint x "Research"; callAndSaveEndpoint x "NonResearch" ])
    |> Async.Parallel
    |> Async.RunSynchronously
    |> Array.forall id
    |> function
        | true ->
            printfn "All API responses successfully saved to %s" outputDir
        | false ->
            printfn "Some API calls failed. Check the output for details."