module Relogify.ApplicationSettings

open Xamarin.Forms
open Newtonsoft.Json
open System

type ChallengeType =
    | Outgoing
    | Incoming

type PlayerInCommunity =
    { CommunityName: string
      PlayerName: string }

type ApplicationSettings =
    { Communities: PlayerInCommunity list }

type Challenge = {
    PlayerInCommunity: PlayerInCommunity
    Type: ChallengeType
    UtcTimeStamp: DateTime }

[<Literal>]
let ApplicationSettingsStorageKey = "app_settings_key"

[<Literal>]
let ChallengesStorageKey = "challenges_key"

let getApplicationSettingsOrNone key =
    try
        match Application.Current.Properties.TryGetValue key with
        | true, (:? string as json) -> JsonConvert.DeserializeObject<ApplicationSettings>(json) |> Some
        | _ -> None
    with ex ->
        None

// TODO: Remove duplication
let getChallengesOrNone key =
    try
        match Application.Current.Properties.TryGetValue key with
        | true, (:? string as json) -> JsonConvert.DeserializeObject<Challenge list>(json) |> Some
        | _ -> None
    with ex ->
        None

let getApplicationSettings (): ApplicationSettings =
    getApplicationSettingsOrNone ApplicationSettingsStorageKey |>
        Option.defaultValue { Communities = [] }

let saveApplicationSettings (appSettings : ApplicationSettings) =
    let json = JsonConvert.SerializeObject(appSettings)
    Application.Current.Properties.[ApplicationSettingsStorageKey] <- json

    async {
        do! Application.Current.SavePropertiesAsync () |> Async.AwaitTask

        let messagingService = DependencyService.Get<IMessagingService>()
        do! messagingService.SendRegistrationToServer ()
    } |> Async.StartAsTask // TODO: Keep it as async? Note: might be a problem with race conditions right after adding community

// TODO: Remove duplication
let getChallenges (): Challenge list =
    getChallengesOrNone ChallengesStorageKey |>
        Option.defaultValue []

let saveChallenges (challenges : Challenge list) =
    let json = JsonConvert.SerializeObject(challenges)
    Application.Current.Properties.[ChallengesStorageKey] <- json
    Application.Current.SavePropertiesAsync ()

let addCommunityToSettings (communityName : string) (playerName : string) =
    let currentSettings = getApplicationSettings ()
    let newCommunity = { CommunityName = communityName; PlayerName = playerName }

    // TODO: Check if community exists first
    saveApplicationSettings { currentSettings with Communities = [newCommunity] @ currentSettings.Communities }

let addChallengeToLocalStorage (playerInCommunity: PlayerInCommunity) (challengeType: ChallengeType) =
    async {
        let savedChallenges = getChallenges ()
        let newChallengeList =
            if savedChallenges |> List.exists (fun c -> c.PlayerInCommunity = playerInCommunity) then
                savedChallenges
            else
                { Type = challengeType; PlayerInCommunity = playerInCommunity; UtcTimeStamp = DateTime.UtcNow; } :: savedChallenges
        do! saveChallenges newChallengeList |> Async.AwaitTask
        return newChallengeList
    }

let removeChallengeFromLocalStorage (playerInCommunity: PlayerInCommunity) =
    async {
        let savedChallenges = getChallenges ()
        let newChallengeList = savedChallenges |> List.filter (fun c -> c.PlayerInCommunity <> playerInCommunity)
        do! saveChallenges newChallengeList |> Async.AwaitTask
        return newChallengeList
    }

let areSet (applicationSettings: ApplicationSettings) =
    applicationSettings.Communities |> List.isEmpty |> not

let getCurrentPlayerInCommunity (communityName: string) (settings : ApplicationSettings) =
    settings.Communities |> List.find (fun s -> s.CommunityName = communityName)
