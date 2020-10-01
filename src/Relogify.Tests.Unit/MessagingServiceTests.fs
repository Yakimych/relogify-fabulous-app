module MessagingServiceTests

open Relogify.Android
open Xunit

[<Fact>]
let ``Community and player names are parsed correctly from message body``() =
    let expectedCommunityName = "communityname"
    let expectedPlayerName = "player1"

    let messageBody = sprintf "%s: %s has challenged you to a game" expectedCommunityName expectedPlayerName
    let (actualCommunityName, actualPlayerName) = messageBody |> MessageUtils.parseCommunityAndPlayer

    Assert.Equal(expectedCommunityName, actualCommunityName)
    Assert.Equal(expectedPlayerName, actualPlayerName)

