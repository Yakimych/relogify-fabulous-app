module ChallengeStateTests

open System
open Relogify
open Relogify.ApplicationSettings
open Relogify.ChallengeManager
open Xunit
open AddResult
open FsCheck.Xunit

[<Property>]
let ``getChallengeState returns NotChallenged when challenge list is empty`` (playerInCommunity: PlayerInCommunity) =
    let actualChallengeState = getChallengeState [] playerInCommunity
    Assert.Equal(NotChallenged, actualChallengeState)

[<Property>]
let ``getChallengeState returns WaitingForResponse if Outgoing challenge exists for player`` (randomChallenges: Challenge list) (playerInCommunity: PlayerInCommunity) =
    let outgoingChallenge: Challenge =
        { PlayerInCommunity = playerInCommunity
          Type = Outgoing
          UtcTimeStamp = DateTime(2020, 01, 01) }
    let challenges = outgoingChallenge :: randomChallenges

    let result = getChallengeState challenges playerInCommunity

    Assert.Equal(WaitingForResponse, result)

[<Property>]
let ``getChallengeState returns ReceivedChallenge if Incoming challenge exists for player`` (randomChallenges: Challenge list) (playerInCommunity: PlayerInCommunity) (notificationId: int) =
    let outgoingChallenge: Challenge =
        { PlayerInCommunity = playerInCommunity
          Type = Incoming notificationId
          UtcTimeStamp = DateTime(2020, 01, 01) }
    let challenges = outgoingChallenge :: randomChallenges

    let result = getChallengeState challenges playerInCommunity

    Assert.Equal(ReceivedChallenge notificationId, result)
