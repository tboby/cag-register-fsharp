module Shared.CagReference

open System
open Shared.Parsec
open Shared.Parsec.FParsecCompat

// Types to represent the different CAG reference formats
type CagReferenceParsed =
    | PerYearFormat of year:int * id:int
    | PerMeetingFormat of quarter:int * meeting:int * item:string * year:int
    | CrFormat of application:int * year:int
    | Unknown of string
    member this.Year =
        match this with
        | PerYearFormat(year, id) -> year
        | PerMeetingFormat(quarter, meeting, item, year) -> year
        | CrFormat(application, year) -> year
        | Unknown s -> 0


    member this.CompareToCustom other =
        match this, other with
        | PerYearFormat(year, id), PerYearFormat(year1, id1) when year = year ->
            compare id id1
        | PerYearFormat(year, id), PerYearFormat(year1, id1) ->
            compare year year1
        | PerYearFormat(year, id), _ -> 1
        | PerMeetingFormat(quarter, meeting, item, year), PerYearFormat(year1, id) -> -1
        | PerMeetingFormat(quarter, meeting, item, year), PerMeetingFormat(quarter1, meeting1, item1, year1) ->
            let yearComp = compare year year1
            if yearComp <> 0 then yearComp else
            let quarterComp = compare quarter quarter1
            if quarterComp <> 0 then quarterComp else
            let meetingComp = compare meeting meeting1
            if meetingComp <> 0 then meetingComp else
            compare item item1
        | PerMeetingFormat(quarter, meeting, item, year), _ -> 1
        | CrFormat(application, year), PerYearFormat(year1, id) -> -1
        | CrFormat(application, year), PerMeetingFormat(quarter, meeting, item, year1) -> -1
        | CrFormat(application, year), CrFormat(application1, year1) ->
            let yearComp = compare year year1
            if yearComp <> 0 then yearComp else
            compare application application1
        | CrFormat(application, year), Unknown s -> 1
        | Unknown s, Unknown s1 -> compare s s1
        | Unknown s, _ -> -1

type CagReference = {
    raw: string
    parsed: CagReferenceParsed
    }


// Helper parsers
let digits n = parray n digit |>> (Array.map string >> String.concat "")

// Parsers for each format
let oldFormatParser =
    pipe3
        (digits 2 .>> pstring "/CAG")
        (optional (pchar '/'))
        (digits 4)
        (fun year _ id -> PerYearFormat(int ($"20{year}"), id |> int))

let newFormatParser =
    pipe4
        (pstring "CAG " >>. digit .>>. optional digit |>> string)
        (pstring "-" >>. digits 2 .>> spaces)
        (optional (between (pstring "(") (pstring ")") (many1Satisfy (fun c -> c <> ')'))) |>> string)
        (pstring "/" >>. digits 4)
        (fun quarter meeting item year ->
            PerMeetingFormat(quarter |> int, meeting |> int, item, year |> int))

let applicationParser =
    pipe2
        (pstring "CR" >>. digit .>>. optional digit |>> string)
        (pstring "/" >>. digits 4)
        (fun app year -> CrFormat(app |> int, year |> int))

// Combined parser that tries each format
let cagReferenceParser =
    choice [
        attempt oldFormatParser
        attempt newFormatParser
        applicationParser
    ]
// Function to run the parser
let parseCagReference input =
    match runString cagReferenceParser () input with
    | Ok (result, _, _) -> result
    | Error errorValue -> Unknown input
    // | stringSegmentFunc -> failwith "todo"
    // |
    // | Success(result, _, _) -> Some result
    // | Failure(_, _, _) -> None
