module Relogify.OpponentList

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms
open Relogify.Graphql

type Model =
     { CurrentPlayerName: string
       OpponentNames: string array }

let initModel =
    { CurrentPlayerName = "TestPlayer"
      OpponentNames = [| "PlaceHolder" |] }

type Msg =
    | FetchPlayersClicked
    | PlayersFetched of string array
    | FetchPlayerError of string

type CmdMsg = FetchPlayersCmdMsg

let fetchPlayersCmd () =
    async {
        let! result = getPlayersOperation.AsyncRun(runtimeContext, "test")
        return
            match result.Data with
            | Some data -> data.Players |> Array.map (fun p -> p.Name) |> PlayersFetched
            | None -> FetchPlayerError "Error fetching players" // TODO: Handle errors correctly
    }
    |> Cmd.ofAsyncMsg

let mapCommands =
    function
    | FetchPlayersCmdMsg -> fetchPlayersCmd()

let update model msg: Model * CmdMsg list =
    match msg with
    | FetchPlayersClicked -> model, [FetchPlayersCmdMsg]
    | PlayersFetched players -> { model with OpponentNames = players }, []
    | FetchPlayerError errorMessage -> model, [] // TODO: Handle errors correctly

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
                                    |> Array.map (fun opponentName ->
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
