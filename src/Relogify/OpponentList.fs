module Relogify.OpponentList

open Fabulous
open Fabulous.XamarinForms
open Relogify.ApplicationSettings
open Relogify.Graphql
open Xamarin.Forms

//type CommunityWithPlayers = { communityName: string; playerNames: string list }

type CommunitiesWithPlayers = Map<string, string []>

type State =
    | Fetching of communities: Community list
    | FetchSuccess of communitiesWithPlayers: CommunitiesWithPlayers
    | FetchError of communities: Community list * errorMessage: string

type Model = { state: State; currentCommunityName: string }

type Msg =
    | FetchPlayers of Community list
    | PlayersFetched of CommunitiesWithPlayers
    | FetchPlayersError of communities: Community list * errorMessage: string
    | PlayerSelected of int option
    | SelectCurrentCommunity of string

type CmdMsg =
    | FetchPlayersCmdMsg of communities: Community list
    | DeselectPlayerCmdMsg

type OutMsg =
    | PlayerSelectedOutMsg of player: string * communityName: string

let initModel (communities : Community list): Model * CmdMsg list =
    match communities with
    | [] -> { state = FetchError ([], "Application settings are empty"); currentCommunityName = "" }, []
    | firstCommunity :: _ -> { state = Fetching communities; currentCommunityName = firstCommunity.CommunityName }, [FetchPlayersCmdMsg communities]

// TODO: Remove
type TempType = {
    playerName: string
    communityName: string
}

// TODO: Rewrite without intermediate type
// TODO: Filter out current player from each list
let toCommunitiesWithPlayers (fetchedPlayers: MyProvider.Operations.GetPlayersForCommunities.Types.Query_root): CommunitiesWithPlayers =
    let asd = fetchedPlayers.Players |> Array.map (fun p -> { playerName = p.Name; communityName = p.Community.Name })
    let qwe = asd |> Array.groupBy (fun p -> p.communityName) //|> Array.map (fun x -> ())
    let zxc = qwe |> Array.map (fun (communityName, tempType) -> (communityName, tempType |> Array.map (fun z -> z.playerName)))
    zxc |> Map.ofArray

let fetchPlayersCmd (communities: Community list) =
    async {
        let communityNames = communities |> List.map (fun c -> c.CommunityName) |> Array.ofList
        let! result = getPlayersForCommunitiesOperation.AsyncRun(runtimeContext, communityNames)
        return
            match result.Data with
//            | Some data -> data.Players |> List.ofArray |> List.map (fun p -> p.Name) |> List.except [currentPlayerName] |> PlayersFetched
            | Some data -> data |> toCommunitiesWithPlayers |> PlayersFetched
            | None -> FetchPlayersError (communities, sprintf "Error fetching players for communities '%A'. Please check your internet connection and try again. If the problem persists, check your settings and restart the app" communityNames)
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

// TODO: currentPlayerName was used to filter out the current player from the list
let update (model: Model) (msg: Msg) (currentPlayerName: string): Model * CmdMsg list * OutMsg option =
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
            model, [DeselectPlayerCmdMsg], Some <| PlayerSelectedOutMsg (selectedPlayer, model.currentCommunityName)
        | _ -> model, [], None

let applyTabButtonStyle (isSelected: bool) (button: ViewElement) =

    let baseButton =
        button
            .Margin(Thickness(2.0, 5.0))
            .ButtonCornerRadius(5)
            .BorderColor(Color.Black)
            .Height(60.0)
            .FontSize(FontSize.FontSize(16.0))

    if isSelected then
        baseButton
            .BorderWidth(2.0)
            .BackgroundColor(Color.FromHex("#2196F3"))
            .TextColor(Color.White)
            .FontAttributes(FontAttributes.Bold)
    else
        baseButton
            .BorderWidth(1.0)
            .BackgroundColor(Color.LightGray)
            .TextColor(Color.Black)

let view (allCommunities: Community list) (model: Model) (dispatch: Msg -> unit): ViewElement =
    View.ContentPage(
        title = "Players",
        icon = ImagePath "tab_feed.png",
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
