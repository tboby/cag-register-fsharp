module Server.ApplicationsApi

open System
open Microsoft.Data.Sqlite
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
