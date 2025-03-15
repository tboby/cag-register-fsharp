module Server.MinuteParsing

open System
open System.Text.RegularExpressions
open Shared

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

