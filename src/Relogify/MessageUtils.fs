module Relogify.MessageUtils

open System

// TODO: Send challengeFrom and communityName as part of the message instead of parsing a string
let private parsePlayer (messagePlayerPart: string): string =
    let words = messagePlayerPart.Split([|" "|], StringSplitOptions.RemoveEmptyEntries)
    match words |> List.ofArray with
    | [] -> failwith "The message player part cannot be empty"
    | playerName :: _otherWords -> playerName

let parsePlayerInCommunity (messageBody: string): ApplicationSettings.PlayerInCommunity =
    let parts = messageBody.Split([|":"|], StringSplitOptions.RemoveEmptyEntries)
    match parts |> List.ofArray with
    | [] -> failwith "Message cannot be empty!"
    | [_singleElement] -> failwith "Message should contain player information"
    | communityName :: secondPart :: _restOfMessage -> { CommunityName = communityName; PlayerName = secondPart |> parsePlayer }

