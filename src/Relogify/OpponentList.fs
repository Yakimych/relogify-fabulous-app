module Relogify.OpponentList

open Fabulous
open Fabulous.XamarinForms
open Relogify.ApplicationSettings
open Relogify.Graphql
open Xamarin.Forms

type CommunitiesWithPlayers = Map<string, string []>

type State =
    | Fetching of communities: PlayerInCommunity list
    | FetchSuccess of communitiesWithPlayers: CommunitiesWithPlayers
    | FetchError of communities: PlayerInCommunity list * errorMessage: string

type Model = { state: State; currentCommunityName: string }

type Msg =
    | FetchPlayers of PlayerInCommunity list
    | PlayersFetched of CommunitiesWithPlayers
    | FetchPlayersError of communities: PlayerInCommunity list * errorMessage: string
    | PlayerSelected of int option
    | SelectCurrentCommunity of string

type CmdMsg =
    | FetchPlayersCmdMsg of communities: PlayerInCommunity list
    | DeselectPlayerCmdMsg

type OutMsg =
    | PlayerSelectedOutMsg of PlayerInCommunity

let initModel (communities : PlayerInCommunity list): Model * CmdMsg list =
    match communities with
    | [] -> { state = FetchError ([], "Application settings are empty"); currentCommunityName = "" }, []
    | firstCommunity :: _ -> { state = Fetching communities; currentCommunityName = firstCommunity.CommunityName }, [FetchPlayersCmdMsg communities]

let toCommunitiesWithPlayers (selectedPlayersInCommunities: PlayerInCommunity list) (fetchedPlayers: PlayersForCommunities): CommunitiesWithPlayers =
    fetchedPlayers.Players
    |> Array.map (fun p -> { PlayerName = p.Name; CommunityName = p.Community.Name })
    |> Array.where (fun p -> selectedPlayersInCommunities |> List.contains p |> not)
    |> Array.groupBy (fun p -> p.CommunityName)
    |> Array.map (fun (communityName, playerInCommunity) -> (communityName, playerInCommunity |> Array.map (fun z -> z.PlayerName)))
    |> Map.ofArray

let fetchPlayersCmd (playersInCommunities: PlayerInCommunity list) =
    async {
        let communityNames = playersInCommunities |> List.map (fun c -> c.CommunityName) |> Array.ofList
        let! result = getPlayersForCommunitiesOperation.AsyncRun(runtimeContext, communityNames)
        return
            match result.Data with
            | Some data -> data |> (toCommunitiesWithPlayers playersInCommunities) |> PlayersFetched
            | None -> FetchPlayersError (playersInCommunities, sprintf "Error fetching players for communities '%A'. Please check your internet connection and try again. If the problem persists, check your settings and restart the app" communityNames)
    }
    |> Cmd.ofAsyncMsg

let listViewRef = ViewRef<CustomListView>()

let deselectPlayer () =
    listViewRef.TryValue |> Option.iter (fun listView -> listView.SelectedItem <- null)
    Cmd.none

let mapCommands: (CmdMsg -> Cmd<Msg>) =
    function
    | FetchPlayersCmdMsg communities -> fetchPlayersCmd communities
    | DeselectPlayerCmdMsg -> deselectPlayer ()

let update (model: Model) (msg: Msg): Model * CmdMsg list * OutMsg option =
    match msg with
    | SelectCurrentCommunity communityName -> { model with currentCommunityName = communityName }, [], None
    | FetchPlayers communities -> { model with state = Fetching communities }, [FetchPlayersCmdMsg communities], None
    | PlayersFetched communitiesWithPlayers -> { model with state = FetchSuccess communitiesWithPlayers }, [], None
    | FetchPlayersError (communities, errorMessage) -> { model with state = FetchError (communities, errorMessage) }, [], None
    | PlayerSelected maybeIndex ->
        match maybeIndex, model.state with
        | Some index, FetchSuccess communitiesWithPlayers ->
            let selectedCommunity = communitiesWithPlayers.[model.currentCommunityName]
            let selectedPlayer = selectedCommunity.[index]
            model, [DeselectPlayerCmdMsg], Some <| PlayerSelectedOutMsg { PlayerName = selectedPlayer; CommunityName = model.currentCommunityName }
        | _ -> model, [], None

let applyTabButtonStyle (isSelected: bool) (button: ViewElement) =

    let baseButton =
        button
            .Margin(Thickness(2.0, 5.0))
            .ButtonCornerRadius(5)
            .HeightRequest(60.0)
            .BorderColor(Color.Black)
            .BorderWidth(1.0)
            .FontSize(FontSize.fromNamedSize(NamedSize.Small))

    if isSelected then
        baseButton
            .BackgroundColor(Color.FromHex("#2196F3"))
            .TextColor(Color.White)
            .FontAttributes(FontAttributes.Bold)
    else
        baseButton
            .BackgroundColor(Color.LightGray)
            .TextColor(Color.Black)

let view (allCommunities: PlayerInCommunity list) (model: Model) (dispatch: Msg -> unit): ViewElement =
    View.ContentPage(
        title = "Players",
        icon = Image.fromPath "tab_feed.png",
        content =
            match model.state with
            | Fetching _ -> View.ActivityIndicator(isRunning = true)
            | FetchError (communities, errorMessage) ->
                View.StackLayout(
                    orientation = StackOrientation.Vertical,
                    children = [
                        View.Label(
                            text = errorMessage,
                            horizontalOptions = LayoutOptions.Center,
                            verticalOptions = LayoutOptions.Center,
                            margin = Thickness(20.0),
                            textColor = Color.Red
                        )
                        View.Button(
                            text = "Try again",
                            command = (fun _ -> dispatch <| FetchPlayers communities)
                        )
                    ]
                )
            | FetchSuccess communitiesWithPlayers ->
                let players = communitiesWithPlayers.[model.currentCommunityName]
                View.StackLayout(
                    orientation = StackOrientation.Vertical,
                    children = [
                        View.Grid(
                            coldefs = [ Star; Star; Star ],
                            children = (allCommunities
                                        |> List.mapi (fun column community ->
                                            let isSelected = community.CommunityName = model.currentCommunityName
                                            View.Button(
                                                text = community.CommunityName,
                                                command = (fun _ -> dispatch (SelectCurrentCommunity community.CommunityName))
                                            ).Column(column) |> applyTabButtonStyle isSelected))
                        )
                        View.ListView(
                            ref = listViewRef,
                            isPullToRefreshEnabled = false,
                            items = (players |> Array.map (fun playerName -> View.TextCell(text = playerName)) |> List.ofArray),
                            itemSelected = (dispatch << PlayerSelected)
                        )
                    ]
                )
    )
