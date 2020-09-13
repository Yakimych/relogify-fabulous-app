module Relogify.ApplicationSettings

open Xamarin.Forms
open Newtonsoft.Json

type PlayerInCommunity =
    { CommunityName: string
      PlayerName: string }

type ApplicationSettings =
    { Communities: PlayerInCommunity list }

let ApplicationSettingsStorageKey = "app_settings_key"

let getApplicationSettingsOrNone key =
    try
        match Application.Current.Properties.TryGetValue key with
        | true, (:? string as json) -> JsonConvert.DeserializeObject<ApplicationSettings>(json) |> Some
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


let addCommunityToSettings (communityName : string) (playerName : string) =
    let currentSettings = getApplicationSettings ()
    let newCommunity = { CommunityName = communityName; PlayerName = playerName }

    // TODO: Check if community exists first
    saveApplicationSettings { currentSettings with Communities = [newCommunity] @ currentSettings.Communities }

let areSet (applicationSettings: ApplicationSettings) =
    applicationSettings.Communities |> List.isEmpty |> not

let getCurrentPlayerInCommunity (communityName: string) (settings : ApplicationSettings) =
    settings.Communities |> List.find (fun s -> s.CommunityName = communityName)
