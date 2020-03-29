namespace Relogify

open Fabulous
open Fabulous.XamarinForms
open Relogify.ApplicationSettings
open Routes
open Xamarin.Forms

module App =
    type Page =
        | Home
        | AddResult of opponentName: string
        | Timer

    type Model =
      { SomeFlag : bool
        PageStack : Page list
        SelectedTabIndex : int
        OpponentListModel : OpponentList.Model
        SettingsModel : Settings.Model
        AddResultModel : AddResult.Model
        ApplicationSettings : ApplicationSettings
        AboutModel : About.Model }

    type Msg =
        | OpponentListMsg of OpponentList.Msg
        | AddResultMsg of AddResult.Msg
        | SettingsMsg of Settings.Msg
        | SetCurrentPage of tabIndex: int
        | PushPage of Page
        | PopPage
        | PopBackToHome

    type CmdMsg =
        | OpponentListCmdMsg of OpponentList.CmdMsg
        | AddResultCmdMsg of AddResult.CmdMsg
        | SettingsCmdMsg of Settings.CmdMsg

    let mapCommands cmdMsg =
        match cmdMsg with
        | OpponentListCmdMsg x -> OpponentList.mapCommands x |> Cmd.map OpponentListMsg
        | AddResultCmdMsg x -> AddResult.mapCommands x |> Cmd.map AddResultMsg
        | SettingsCmdMsg x -> Settings.mapCommands x |> Cmd.map SettingsMsg
//        | ShowTimerCmdMsg -> showTimer ()

    let selectOpponentTabIndex = 0
    let settingsTabIndex = 1

    let getPlayerNameIfAddingResult (pageType: Page) =
        match pageType with
        | AddResult playerName -> Some(playerName)
        | _ -> None

    let isAddingResultFor (model: Model): string option =
        model.PageStack
        |> List.choose getPlayerNameIfAddingResult
        |> List.tryHead

    let init () =
        Routing.RegisterRoute("TestRoute", typeof<TestRoutingPage>)

        let applicationSettings = ApplicationSettings.getApplicationSettings ()
        let opponentListModel, opponentListCmdMsgs = OpponentList.initModel applicationSettings
        let cmdMsgs = opponentListCmdMsgs |> List.map OpponentListCmdMsg

        { SomeFlag = false
          PageStack = [Home]
          SelectedTabIndex = if applicationSettings |> areSet then selectOpponentTabIndex else settingsTabIndex
          OpponentListModel = opponentListModel
          AboutModel = About.initModel
          AddResultModel = AddResult.initModel
          ApplicationSettings = applicationSettings
          SettingsModel = Settings.initModel applicationSettings.CommunityName.IsSome }, cmdMsgs

    let pushPage (page: Page) (model: Model): Model =
        { model with PageStack = [page] @ model.PageStack }

    let update msg (model: Model) =
        match msg with
        | OpponentListMsg opponentListMsg ->
            if model.ApplicationSettings |> areSet then
                let opponentListModel, opponentListCmdMsgs, opponentListOutMsg = OpponentList.update model.OpponentListModel opponentListMsg
                match opponentListOutMsg with
                | Some (OpponentList.PlayerSelectedOutMsg selectedOpponentName) ->
                    model |> pushPage (AddResult selectedOpponentName), opponentListCmdMsgs |> List.map OpponentListCmdMsg
                | None ->
                    { model with OpponentListModel = opponentListModel }, opponentListCmdMsgs |> List.map OpponentListCmdMsg
            else
                model, []
        | AddResultMsg addResultMsg ->
            let maybeOpponentName = model |> isAddingResultFor
            match model.ApplicationSettings.PlayerName, maybeOpponentName with
            | Some ownName, Some opponentName ->
                let addResultModel, addResultCmdMsgs = AddResult.update model.AddResultModel addResultMsg ownName opponentName
                { model with AddResultModel = addResultModel }, addResultCmdMsgs |> List.map AddResultCmdMsg
            | _ -> model, []
        | SettingsMsg settingsMsg ->
            let settingsModel, settingsCmdMsgs = Settings.update model.SettingsModel settingsMsg
            let updatedModel = { model with SettingsModel = settingsModel }
            let appCmdMsgsFromSettings = settingsCmdMsgs |> List.map SettingsCmdMsg

            match settingsMsg with
            | Settings.SettingsSaved newSettings ->
                // Refetch the players
                let opponentListCmdMsg = OpponentList.FetchPlayersCmdMsg newSettings.CommunityName |> OpponentListCmdMsg
                let newCmdMsgs = appCmdMsgsFromSettings @ [opponentListCmdMsg]

                { updatedModel with ApplicationSettings = newSettings |> Settings.toApplicationSettings }, newCmdMsgs
            | _ -> updatedModel, appCmdMsgsFromSettings
        | SetCurrentPage tabIndex -> { model with SelectedTabIndex = tabIndex }, []
        | PushPage page -> model |> pushPage page, []
        | PopPage -> { model with PageStack = model.PageStack |> List.skip 1 }, [] // TODO: Check whether we're on the HomePage
        | PopBackToHome -> { model with PageStack = [Home] }, []

    let navigationPrimaryColor = Color.FromHex("#2196F3")

    let getPageTitle (tabIndex: int) =
        match tabIndex with
        | 0 -> "Select Opponent"
        | 1 -> "Settings"
        | 2 -> "Report result" // TODO: Remove
        | 3 -> "About"
        | _ -> failwith (sprintf "No tab with index %d" tabIndex)

    let timerIsShown (model: Model) = model.PageStack |> List.contains Timer

    let view (model: Model) dispatch =
        // TODO: Refactor this away as soon as ApplicationSettings are refactored into an Option/List of communities
        let currentPlayerOrEmpty = model.ApplicationSettings.PlayerName |> Option.defaultValue ""

        let modalPage dispatch =
                        (Settings.view
                            model.SettingsModel
                            currentPlayerOrEmpty
                            (model.ApplicationSettings.CommunityName |> Option.defaultValue "")
                            (Msg.SettingsMsg >> dispatch))

        let addingResultFor = model |> isAddingResultFor

        View.NavigationPage(
            barBackgroundColor = navigationPrimaryColor,
            barTextColor = Color.White,
            popped = (fun _ -> dispatch PopPage),

            pages = [
                yield View.TabbedPage(
                    currentPageChanged = (fun maybeIndex ->
                        match maybeIndex with
                        | None -> ()
                        | Some index -> dispatch (SetCurrentPage index)
                    ),
                    title = getPageTitle model.SelectedTabIndex,
                    barBackgroundColor = navigationPrimaryColor,
                    unselectedTabColor = Color.FromHex("#95FFFFFF"),
                    selectedTabColor = Color.White,
                    barTextColor = Color.White,
                    children = [
                        yield!
                            match model.ApplicationSettings.PlayerName, model.ApplicationSettings.CommunityName with
                            | Some playerName, Some communityName ->
                                [(OpponentList.view model.OpponentListModel playerName communityName (Msg.OpponentListMsg >> dispatch)) // TODO: Remove parens and ToolBarItems
                                    .BackButtonPressed(fun _ -> dispatch PopPage)
                                    .ToolbarItems(
                                        [
                                            View.ToolbarItem(
                                                text = "Add Result",
                                                command = (fun () -> AddResult "TestPlayer" |> PushPage |> dispatch)
                                            )
                                        ])
                                 ]
                            | _, _ -> []

                        yield (Settings.view
                            model.SettingsModel
                            (model.ApplicationSettings.PlayerName |> Option.defaultValue "")
                            (model.ApplicationSettings.CommunityName |> Option.defaultValue "")
                            (Msg.SettingsMsg >> dispatch))

                        yield About.view model.AboutModel
                ])

                yield! addingResultFor
                    |> Option.map (fun playerNameToAddResultFor -> // TODO: Use the playerName
                        [(AddResult.view model.AddResultModel (Msg.AddResultMsg >> dispatch) currentPlayerOrEmpty playerNameToAddResultFor) // TODO: This should not be yielded if the currentPlayer is empty
                            .ToolbarItems(
                                [
                                    View.ToolbarItem(
                                        text = "Timer",
                                        command = (fun () -> dispatch (PushPage Timer))
                                    )
                                ])]
                    )
                    |> Option.defaultValue []

                if model |> timerIsShown then
                    yield (modalPage dispatch).BackButtonPressed(fun _ -> dispatch PopPage)
        ])

    let program = Program.mkProgramWithCmdMsg init update view mapCommands

type App () as app =
    inherit Application ()

    let runner =
        App.program
#if DEBUG
        |> Program.withConsoleTrace
#endif
        |> XamarinFormsProgram.run app

// Uncomment this code to save the application state to app.Properties using Newtonsoft.Json
// See https://fsprojects.github.io/Fabulous/Fabulous.XamarinForms/models.html#saving-application-state for further  instructions.
#if APPSAVE
    let modelId = "model"
    override __.OnSleep() =

        let json = Newtonsoft.Json.JsonConvert.SerializeObject(runner.CurrentModel)
        Console.WriteLine("OnSleep: saving model into app.Properties, json = {0}", json)

        app.Properties.[modelId] <- json

    override __.OnResume() =
        Console.WriteLine "OnResume: checking for model in app.Properties"
        try
            match app.Properties.TryGetValue modelId with
            | true, (:? string as json) ->

                Console.WriteLine("OnResume: restoring model from app.Properties, json = {0}", json)
                let model = Newtonsoft.Json.JsonConvert.DeserializeObject<App.Model>(json)

                Console.WriteLine("OnResume: restoring model from app.Properties, model = {0}", (sprintf "%0A" model))
                runner.SetCurrentModel (model, Cmd.none)

            | _ -> ()
        with ex ->
            App.program.onError("Error while restoring model found in app.Properties", ex)

    override this.OnStart() =
        Console.WriteLine "OnStart: using same logic as OnResume()"
        this.OnResume()
#endif


