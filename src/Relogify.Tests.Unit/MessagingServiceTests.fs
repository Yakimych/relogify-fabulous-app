module MessagingServiceTests

open Relogify.ApplicationSettings
open Relogify.MessageUtils
open Xunit

[<Fact>]
let ``Community and player names are parsed correctly from message body``() =
    let expectedCommunityName = "communityname"
    let expectedPlayerName = "player1"

    let messageBody = sprintf "%s: %s has challenged you to a game" expectedCommunityName expectedPlayerName
    let playerInCommunity = messageBody |> parsePlayerInCommunity

    Assert.Equal(expectedCommunityName, playerInCommunity.CommunityName)
    Assert.Equal(expectedPlayerName, playerInCommunity.PlayerName)

