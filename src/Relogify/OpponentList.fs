module Relogify.OpponentList

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms

type Model =
     { CurrentPlayerName: string
       OpponentNames: string list }

let initModel =
    { CurrentPlayerName = "TestPlayer"
      OpponentNames = [ "PlaceHolder" ] }

type Msg =
    | FetchPlayersClicked
    | PlayersFetched of string list

type CmdMsg = FetchPlayersCmdMsg

let fetchPlayersCmd () =
    async {
        do! Async.Sleep 200
        return PlayersFetched ["Kalle"; "Leif"]
    }
    |> Cmd.ofAsyncMsg

let mapCommands =
    function
    | FetchPlayersCmdMsg -> fetchPlayersCmd()

let update model msg: Model * CmdMsg list =
    match msg with
    | FetchPlayersClicked -> model, [FetchPlayersCmdMsg]
    | PlayersFetched players -> { model with OpponentNames = players }, []

let view model dispatch =
    View.ContentPage(
        title = "Select Opponent",
        content = View.CollectionView(
            items = [
                View.StackLayout(
                     padding = Thickness 20.0,
                     verticalOptions = LayoutOptions.Center,
                     children =
                         [
                             yield View.Label(
                                text = model.CurrentPlayerName,
                                horizontalOptions = LayoutOptions.Center,
                                width = 200.0,
                                horizontalTextAlignment = TextAlignment.Center)

                             yield! model.OpponentNames
                                    |> List.map (fun opponentName ->
                                    View.Label(
                                           text = opponentName,
                                           horizontalOptions = LayoutOptions.Center,
                                           width = 200.0,
                                           horizontalTextAlignment = TextAlignment.Center)
                                    )

                             yield View.Button(
                                text = "Fetch Players",
                                horizontalOptions = LayoutOptions.Center,
                                width = 200.0,
                                command = (fun () -> dispatch FetchPlayersClicked))
                         ])
                ]
            )
        )
