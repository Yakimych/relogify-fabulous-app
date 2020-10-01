module MessagingServiceTests

open Relogify.Android
open Relogify.ApplicationSettings
open Xunit

[<Fact>]
let ``Community and player names are parsed correctly from message body``() =
    let expectedCommunityName = "communityname"
    let expectedPlayerName = "player1"

    let messageBody = sprintf "%s: %s has challenged you to a game" expectedCommunityName expectedPlayerName
    let playerInCommunity = messageBody |> MessageUtils.parsePlayerInCommunity

    Assert.Equal(expectedCommunityName, playerInCommunity.CommunityName)
    Assert.Equal(expectedPlayerName, playerInCommunity.PlayerName)

