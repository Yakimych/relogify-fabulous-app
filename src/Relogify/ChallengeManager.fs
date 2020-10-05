module Relogify.ChallengeManager

open FSharp.Data
open Relogify.ApplicationSettings

let performChallengeApiCall (fromPlayer: string) (toPlayer: string) (communityName: string) =
    let notificationFunctionBaseUrl = ConfigManager.getNotificationFunctionBaseUrl ()
    let challengeFunctionName = "SendChallenge"
    let challengeFunctionCode = ConfigManager.getChallengeFunctionCode ()
    let challengeUrl = sprintf "%s/%s?code=%s&challengeFrom=%s&challengeTo=%s&communityName=%s" notificationFunctionBaseUrl challengeFunctionName challengeFunctionCode fromPlayer toPlayer communityName

    Http.AsyncRequest(challengeUrl)

let respondToChallenge (fromPlayer: string) (communityName: string) =
    let applicationSettings = getApplicationSettings ()

    // NOTE: This might behave incorrectly if more that one player in the same community is allowed in the settings
    let maybePlayerInCommunity = applicationSettings.Communities |> List.tryFind (fun c -> c.CommunityName = communityName)

    // "fromPlayer" becomes "toPlayer" in the response-challenge
    maybePlayerInCommunity
    |> Option.map (fun playerInCommunity -> performChallengeApiCall playerInCommunity.PlayerName fromPlayer communityName |> Async.Ignore)
    |> Option.defaultValue (async { return () })


