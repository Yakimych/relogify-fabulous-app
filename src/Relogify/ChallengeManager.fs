module Relogify.ChallengeManager

open System
open FSharp.Data
open Relogify.ApplicationSettings
open Xamarin.Forms
open Newtonsoft.Json

type ChallengeType =
    | Outgoing
    | Incoming of notificationId: int

type Challenge = {
    PlayerInCommunity: PlayerInCommunity
    Type: ChallengeType
    UtcTimeStamp: DateTime }

[<Literal>]
let ChallengesStorageKey = "challenges_key"

let getChallenges (): Challenge list =
    getApplicationPropertyOrNone ChallengesStorageKey |> Option.defaultValue []

let saveChallenges (challenges : Challenge list) =
    let json = JsonConvert.SerializeObject(challenges)
    Application.Current.Properties.[ChallengesStorageKey] <- json
    Application.Current.SavePropertiesAsync ()

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

let performChallengeApiCall (fromPlayer: string) (toPlayer: string) (communityName: string) =
    let notificationFunctionBaseUrl = ConfigManager.getNotificationFunctionBaseUrl ()
    let challengeFunctionName = "SendChallenge"
    let challengeFunctionCode = ConfigManager.getChallengeFunctionCode ()
    let challengeUrl = sprintf "%s/%s?code=%s&challengeFrom=%s&challengeTo=%s&communityName=%s" notificationFunctionBaseUrl challengeFunctionName challengeFunctionCode fromPlayer toPlayer communityName

    Http.AsyncRequest(challengeUrl)

let cancelNotification = DependencyService.Get<IMessagingService>().CancelNotification

let respondToChallenge (fromPlayer: string) (communityName: string) =
    let applicationSettings = getApplicationSettings ()

    // NOTE: This might behave incorrectly if more that one player in the same community is allowed in the settings
    let maybePlayerInCommunity = applicationSettings.Communities |> List.tryFind (fun c -> c.CommunityName = communityName)

    // "fromPlayer" becomes "toPlayer" in the response-challenge
    maybePlayerInCommunity
    |> Option.map (fun playerInCommunity -> performChallengeApiCall playerInCommunity.PlayerName fromPlayer communityName |> Async.Ignore)
    |> Option.defaultValue (async { return () })


