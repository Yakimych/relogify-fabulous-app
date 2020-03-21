module Relogify.ApplicationSettings

open System
open Xamarin.Forms

type ApplicationSettings = {
    CommunityName: string option
    PlayerName: string option
}

let CommunityNameStorageKey = "community_name"
let PlayerNameStorageKey = "player_name"

let getStringValueOrNone key =
    let found, property = Application.Current.Properties.TryGetValue(key)
    if found then Some(property.ToString()) else None

let getApplicationSettings (): ApplicationSettings =
    { CommunityName = getStringValueOrNone CommunityNameStorageKey
      PlayerName = getStringValueOrNone PlayerNameStorageKey }

let saveApplicationSettings (communityName: string) (playerName: string) =
    if not (String.IsNullOrEmpty(communityName)) then
        Application.Current.Properties.[CommunityNameStorageKey] <- communityName
    if not (String.IsNullOrEmpty(playerName)) then
        Application.Current.Properties.[PlayerNameStorageKey] <- playerName

    Application.Current.SavePropertiesAsync()
