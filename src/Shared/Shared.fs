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

type ApplicationStatus =
    | Active
    | Obsolete

type RegisterType =
    | Research
    | NonResearch

type CagApplicationId = {
    RegisterType: RegisterType
    ApplicationNumber: string
}

type MinuteReference = {
    Title: string
    Url: string
    PageRanges: string
    ProcessedDate: DateTime
}

type CagApplication = {
    ApplicationNumber: CagApplicationId
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
    ApplicationStatus: ApplicationStatus
    RelatedMinutes: MinuteReference list
}

type CagFrontPageEntry = {
    ApplicationNumber: CagApplicationId
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
    ApplicationStatus: ApplicationStatus
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
    ApplicationNumber: CagApplicationId
    FrontPage: CagFrontPageEntry
    Detail: CagApplication
    Differences: ApplicationDiscrepancyDifferences
}



type FileLoadResult = {
    LoadedFile: string
    LoadedDate: DateTime
    RegisterType: RegisterType
    FailedFiles: string list
}

type ApplicationsResponse = {
    RegisterType: RegisterType
    Applications: CagApplication list
}

type FrontPageEntriesResponse = {
    RegisterType: RegisterType
    Entries: CagFrontPageEntry list
}

type DiscrepanciesResponse = {
    RegisterType: RegisterType
    Discrepancies: ApplicationDiscrepancy list
}

type FileLoadResultResponse = {
    RegisterType: RegisterType
    FileLoadResult: FileLoadResult option
}

type ApplicationDisplayNameResponse = {
    RegisterType: RegisterType
    ApplicationDisplayNames: Map<string, string>  // Map from application number to display name
}

type ICagApplicationsApi = {
    getApplications: RegisterType -> Async<ApplicationsResponse>
    getFrontPageEntries: RegisterType -> Async<FrontPageEntriesResponse>
    getDiscrepancies: RegisterType -> Async<DiscrepanciesResponse>
    getFileLoadResult: RegisterType -> Async<FileLoadResultResponse>
    getApplicationDisplayNames: RegisterType -> Async<ApplicationDisplayNameResponse>
}
