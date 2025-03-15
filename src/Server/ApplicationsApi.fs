module Server.ApplicationsApi

open System
open System.IO
open System.Text
open Fable.Remoting.Json
open Microsoft.Data.Sqlite
open Newtonsoft.Json
open Shared


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
                    SELECT DISTINCT m.title, m.url, c.PageRanges, c.ProcessedDate
                    FROM CagReferences c
                    LEFT JOIN minutes m ON m.url = c.PdfUrl
                    WHERE c.CagId = @appId
                    ORDER BY m.CreatedAt DESC;
                """
                command.Parameters.AddWithValue("@appId", app.Reference) |> ignore

                use reader = command.ExecuteReader()
                let minutes =
                    [ while reader.Read() do
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
                    |> List.distinctBy (fun m -> m.Url)  // Remove any remaining duplicates
                    |> List.sortBy (fun m -> m.MeetingDate)
                { app with RelatedMinutes = minutes }
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

module ToCsv =
    let private fableConverter = new FableJsonConverter() :> JsonConverter

    let private settings = JsonSerializerSettings(DateParseHandling = DateParseHandling.None)

    let private fableSerializer =
        let serializer = JsonSerializer()
        serializer.Converters.Add fableConverter
        serializer

    let private jsonEncoding = UTF8Encoding false

    let jsonSerialize (o: 'a) (stream: Stream) =
        use sw = new StreamWriter (stream, jsonEncoding, 1024, true)
        use writer = new JsonTextWriter (sw, CloseOutput = false)
        fableSerializer.Serialize (writer, o)

    let inline serializeOne (root: string) (api : Async<'a>) (filePath: string) = async {
        let! result = api
        use file = File.Create (Path.Combine(root, filePath))
        jsonSerialize result file
    }
    let serializeAll (root: string) = async {
        let api = applicationsApi ()
        let serializeOne() : Async<'a> -> string -> Async<unit> = serializeOne root
        do! serializeOne() (api.getApplicationDisplayNames Research) "getApplicationDisplayNames-Research.json"
        do! serializeOne() (api.getApplicationDisplayNames NonResearch) "getApplicationDisplayNames-NonResearch.json"

        do! serializeOne() (api.getApplications Research) "getApplications-Research.json"
        do! serializeOne() (api.getApplications NonResearch) "getApplications-NonResearch.json"

        do! serializeOne() (api.getDiscrepancies Research) "getDiscrepancies-Research.json"
        do! serializeOne() (api.getDiscrepancies NonResearch) "getDiscrepancies-NonResearch.json"

        do! serializeOne() (api.getFileLoadResult Research) "getFileLoadResult-Research.json"
        do! serializeOne() (api.getFileLoadResult NonResearch) "getFileLoadResult-NonResearch.json"

        do! serializeOne() (api.getFrontPageEntries Research) "getFrontPageEntries-Research.json"
        do! serializeOne() (api.getFrontPageEntries NonResearch) "getFrontPageEntries-NonResearch.json"
    }

