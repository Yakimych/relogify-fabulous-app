module Relogify.ApplicationSettings

open System
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

let getApplicationSettings(): ApplicationSettings =
    getApplicationSettingsOrNone ApplicationSettinsStorageKey |>
        Option.defaultValue { Communities = [] }

let saveApplicationSettings (communityName: string) (playerName: string) =
    match communityName, playerName with 
    | cn, pn when not (String.IsNullOrEmpty(cn)) && not (String.IsNullOrEmpty(pn)) ->
        let communities = [{ CommunityName = cn ; PlayerName = pn}]

        let json = JsonConvert.SerializeObject({ Communities = communities })

        Application.Current.Properties.[ApplicationSettinsStorageKey] <- json
    | _ -> ignore ()

    Application.Current.SavePropertiesAsync()

let areSet (applicationSettings: ApplicationSettings) =
    applicationSettings.Communities |> List.isEmpty |> not 
