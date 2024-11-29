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
    MedicalPurposesRawValuesNotChecked: string list
    CohortDescription: string
    ConfidentialInfo: string
    S251Classes: Set<S251Class>
    S251ClassRawValues: string list
    S251ClassRawValuesNotChecked: string list
    Sponsor: string
    Status: string
    OutcomeDate: System.DateTime option
    NextReviewDate: System.DateTime option
    Notes: string
    NDOO: string option
    EnglishCPI: CPIValue option
    WelshCPI: CPIValue option
}

type CagFrontPageEntry = {
    ApplicationNumber: string
    Reference: string
    Title: string
    Status: string
    OutcomeDate: System.DateTime option
    NextReviewDate: System.DateTime option
    Contact: string
    Organisation: string
    NationalDataOptOutStatus: string option
    EnglishConfidentialPatientInfo: bool option
    WelshConfidentialPatientInfo: bool option
}

type ApplicationDiscrepancyDifferences = {
    Title: bool
    Status: bool
    OutcomeDate: bool
    NextReviewDate: bool
    Contact: bool  // Different between ContactName and Contact
    Organisation: bool // Different between ApplicantOrganisation and Organisation
    NationalDataOptOutStatus: bool // Different between NDOO and NationalDataOptOutStatus
    EnglishConfidentialPatientInfo: bool // Different between EnglishCPI and EnglishConfidentialPatientInfo
    WelshConfidentialPatientInfo: bool // Different between WelshCPI and WelshConfidentialPatientInfo
}

type ApplicationDiscrepancy = {
    ApplicationNumber: string
    FrontPage: CagFrontPageEntry
    Detail: CagApplication
    Differences: ApplicationDiscrepancyDifferences
}

type ICagApplicationsApi = {
    getApplications: unit -> Async<CagApplication list>
    getFrontPageEntries: unit -> Async<CagFrontPageEntry list>
    getDiscrepancies: unit -> Async<ApplicationDiscrepancy list>
}