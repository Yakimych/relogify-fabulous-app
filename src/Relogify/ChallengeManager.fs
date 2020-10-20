module Relogify.ChallengeManager

open System
open FSharp.Data
open Relogify.ApplicationSettings
open Xamarin.Forms
open Newtonsoft.Json

type ChallengeType =
    | Outgoing
    | Incoming of challengeId: int

type ChallengeResponse = Accept | Decline

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

let headers = ["Content-Type", "application/json"]

let performChallengeApiCall (fromPlayer: string) (toPlayer: string) (communityName: string) =
    let challengeApiBaseUrl = ConfigManager.getChallengeApiBaseUrl ()
    let requestBody = TextRequest <| sprintf """ { "communityName": "%s", "fromPlayer": "%s", "toPlayer": "%s" }  """ communityName fromPlayer toPlayer

    Http.AsyncRequest(challengeApiBaseUrl, headers = headers, httpMethod = HttpMethod.Post, body = requestBody)

let asUrlParameter =
    function
    | Accept -> "accept"
    | Decline -> "decline"

let respondToChallengeApiCall (challengeResponse: ChallengeResponse) (challengeId: int) =
    let challengeApiBaseUrl = ConfigManager.getChallengeApiBaseUrl ()
    let challengeResponseUrl = sprintf "%s/rpc/%s/%d" challengeApiBaseUrl (challengeResponse |> asUrlParameter) challengeId

    Http.AsyncRequest(challengeResponseUrl, httpMethod = HttpMethod.Post, body = TextRequest "")

let cancelNotification = DependencyService.Get<IMessagingService>().CancelNotification
