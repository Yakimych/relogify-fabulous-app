namespace Relogify

open Fabulous
open Fabulous.XamarinForms
open Relogify.ApplicationSettings
open Routes
open Xamarin.Forms

module App =
    type Model =
      { SomeFlag : bool
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
//        | ShowTimer

    type CmdMsg =
        | OpponentListCmdMsg of OpponentList.CmdMsg
        | AddResultCmdMsg of AddResult.CmdMsg
        | SettingsCmdMsg of Settings.CmdMsg
//        | ShowTimerCmdMsg

    let mapCommands cmdMsg =
        match cmdMsg with
        | OpponentListCmdMsg x -> OpponentList.mapCommands x |> Cmd.map OpponentListMsg
        | AddResultCmdMsg x -> AddResult.mapCommands x |> Cmd.map AddResultMsg
        | SettingsCmdMsg x -> Settings.mapCommands x |> Cmd.map SettingsMsg
//        | ShowTimerCmdMsg -> showTimer ()

    let selectOpponentTabIndex = 0
    let settingsTabIndex = 1

    let init () =
        Routing.RegisterRoute("TestRoute", typeof<TestRoutingPage>)

        let applicationSettings = ApplicationSettings.getApplicationSettings ()
        let opponentListModel, opponentListCmdMsgs = OpponentList.initModel applicationSettings
        let cmdMsgs = opponentListCmdMsgs |> List.map OpponentListCmdMsg

        { SomeFlag = false
          SelectedTabIndex = if applicationSettings |> areSet then selectOpponentTabIndex else settingsTabIndex
          OpponentListModel = opponentListModel
          AboutModel = About.initModel
          AddResultModel = AddResult.initModel
          ApplicationSettings = applicationSettings
          SettingsModel = Settings.initModel applicationSettings.CommunityName.IsSome }, cmdMsgs

    let update msg (model: Model) =
        match msg with
        | OpponentListMsg opponentListMsg ->
            if model.ApplicationSettings |> areSet then
                let opponentListModel, opponentListCmdMsgs = OpponentList.update model.OpponentListModel opponentListMsg
                { model with OpponentListModel = opponentListModel }, opponentListCmdMsgs |> List.map OpponentListCmdMsg
            else
                model, []
        | AddResultMsg addResultMsg ->
            let addResultModel, addResultCmdMsgs = AddResult.update model.AddResultModel addResultMsg
            { model with AddResultModel = addResultModel }, addResultCmdMsgs |> List.map AddResultCmdMsg
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
//        | ShowTimer -> model, [ShowTimerCmdMsg]

    let navigationPrimaryColor = Color.FromHex("#2196F3")

    let getPageTitle (tabIndex: int) =
        match tabIndex with
        | 0 -> "Select Opponent"
        | 1 -> "Settings"
        | 2 -> "Report result" // TODO: Remove
        | 3 -> "About"
        | _ -> failwith (sprintf "No tab with index %d" tabIndex)

    let view (model: Model) dispatch =
        View.NavigationPage(
            barBackgroundColor = navigationPrimaryColor,
            barTextColor = Color.White,

            pages = [
                View.TabbedPage(
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
                                [OpponentList.view model.OpponentListModel playerName communityName (Msg.OpponentListMsg >> dispatch)]
                            | _, _ -> []

                        yield (Settings.view
                            model.SettingsModel
                            (model.ApplicationSettings.PlayerName |> Option.defaultValue "")
                            (model.ApplicationSettings.CommunityName |> Option.defaultValue "")
                            (Msg.SettingsMsg >> dispatch))
                            .ToolbarItems([View.ToolbarItem(text = "Timer")])

                        yield (AddResult.view model.AddResultModel (Msg.AddResultMsg >> dispatch))
                            .ToolbarItems(
                                [
                                    View.ToolbarItem(
                                        text = "Timer"
    //                                                        command = (fun () -> dispatch ShowTimer)
                                    )
                                ])

                        yield About.view model.AboutModel
            ])
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


