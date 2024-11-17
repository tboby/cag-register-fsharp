namespace Shared

open System

type Todo = { Id: Guid; Description: string }

module Todo =
    let isValid (description: string) =
        String.IsNullOrWhiteSpace description |> not

    let create (description: string) = {
        Id = Guid.NewGuid()
        Description = description
    }

type ITodosApi = {
    getTodos: unit -> Async<Todo list>
    addTodo: Todo -> Async<Todo>
}

[<RequireQualifiedAccess>]
type MedicalPurpose =
    | PreventativeMedicine
    | MedicalDiagnosis
    | MedicalResearch
    | CareAndTreatment
    | HealthAndSocialCareManagement
    | InformingIndividuals

[<RequireQualifiedAccess>]
type S251Class =
    | SpecificSupport
    | ClassI_Identifiability
    | ClassII_GeographicalLocation
    | ClassIII_IdentifyAndContact
    | ClassIV_LinkingMultipleSources
    | ClassV_AuditAndMonitoring
    | ClassVI_GrantingAccess

[<RequireQualifiedAccess>]
type CPIValue =
    | Yes
    | No
    | Other of string

type CagApplication = {
    ApplicationNumber: string
    Reference: string
    OtherRefs: string option
    Title: string
    Summary: string
    ApplicantOrganisation: string
    ContactName: string
    Address: string list
    Postcode: string
    Telephone: string
    Email: string
    MedicalPurposes: Set<MedicalPurpose>
    MedicalPurposesRawValues: string list
    CohortDescription: string
    ConfidentialInfo: string
    S251Classes: Set<S251Class>
    S251ClassRawValues: string list
    Sponsor: string
    Status: string
    OutcomeDate: System.DateTime option
    NextReviewDate: System.DateTime option
    Notes: string
    NDOO: string option
    EnglishCPI: CPIValue option
    WelshCPI: CPIValue option
}

type ICagApplicationsApi = {
    getApplications: unit -> Async<CagApplication list>
}