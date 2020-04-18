module Relogify.ApplicationSettings

open Xamarin.Forms
open Newtonsoft.Json

type Community =
    { CommunityName: string
      PlayerName: string }

type ApplicationSettings =
    { Communities: Community list }

let ApplicationSettinsStorageKey = "app_settings_key"

let getApplicationSettingsOrNone key =
    try
        match Application.Current.Properties.TryGetValue key with
        | true, (:? string as json) -> JsonConvert.DeserializeObject<ApplicationSettings>(json) |> Some
        | _ -> None
    with ex ->
        None

let getApplicationSettings (): ApplicationSettings =
    getApplicationSettingsOrNone ApplicationSettinsStorageKey |>
        Option.defaultValue { Communities = [] }

let saveApplicationSettings (appSettings : ApplicationSettings) =
    let json = JsonConvert.SerializeObject(appSettings)
    Application.Current.Properties.[ApplicationSettinsStorageKey] <- json

    Application.Current.SavePropertiesAsync()

let areSet (applicationSettings: ApplicationSettings) =
    applicationSettings.Communities |> List.isEmpty |> not 

let getSelectedCommunity (settings : ApplicationSettings) =
    settings.Communities |> List.tryHead 
