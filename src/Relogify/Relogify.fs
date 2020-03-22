// Copyright 2018-2019 Fabulous contributors. See LICENSE.md for license.
namespace Relogify

open System.Diagnostics
open System
open Fabulous
open Fabulous.XamarinForms
open Fabulous.XamarinForms.LiveUpdate
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
//        | SetSelectedTabMsg of tabIndex: int
        | ShowTimer

    type CmdMsg =
        | OpponentListCmdMsg of OpponentList.CmdMsg
        | AddResultCmdMsg of AddResult.CmdMsg
        | SettingsCmdMsg of Settings.CmdMsg
        | ShowTimerCmdMsg
//        | SetSelectedTabCmdMsg of tabIndex: int

    let shellRef = ViewRef<Shell>()

    let navigateToPage (pageName: string) =
        match shellRef.TryValue with
        | None -> ()
        | Some shell ->
            let route = ShellNavigationState.op_Implicit (sprintf "%s?name=whatever" pageName)
            async {
                do! shell.GoToAsync route |> Async.AwaitTask
            } |> Async.StartImmediate

        Cmd.none

    let showTimer () =
        match shellRef.TryValue with
        | None -> ()
        | Some shell ->
            async {
                let popModalFunc = fun () -> shell.Navigation.PopModalAsync() |> ignore

                let timerModel = Timer.init popModalFunc
                let timerView = Timer.view timerModel
                let timerPage = timerView.Create() :?> Page
                do! shell.Navigation.PushModalAsync(timerPage) |> Async.AwaitTask

            } |> Async.StartImmediate

        Cmd.none

    let mapCommands cmdMsg =
        match cmdMsg with
        | OpponentListCmdMsg x -> OpponentList.mapCommands x |> Cmd.map OpponentListMsg
        | AddResultCmdMsg x -> AddResult.mapCommands x |> Cmd.map AddResultMsg
        | SettingsCmdMsg x -> Settings.mapCommands x |> Cmd.map SettingsMsg
        | ShowTimerCmdMsg -> showTimer ()
//        | SetSelectedTabCmdMsg tabIndex -> setTabBarItem tabIndex

    let selectOpponentTabIndex = 0
    let settingsTabIndex = 1

    let initModel () =
        let applicationSettings = ApplicationSettings.getApplicationSettings ()
        { SomeFlag = false
          SelectedTabIndex = if applicationSettings |> areSet then selectOpponentTabIndex else settingsTabIndex
          OpponentListModel= OpponentList.initModel
          AboutModel = About.initModel
          AddResultModel = AddResult.initModel
          ApplicationSettings = applicationSettings
          SettingsModel = Settings.initModel applicationSettings }

    let init () =
        Routing.RegisterRoute("TestRoute", typeof<TestRoutingPage>)
        initModel (), []

    let update msg (model: Model) =
        match msg with
        | OpponentListMsg opponentListMsg ->
            let opponentListModel, opponentListCmdMsgs = OpponentList.update model.OpponentListModel opponentListMsg
            { model with OpponentListModel= opponentListModel }, opponentListCmdMsgs |> List.map OpponentListCmdMsg
        | AddResultMsg addResultMsg ->
            let addResultModel, addResultCmdMsgs = AddResult.update model.AddResultModel addResultMsg
            { model with AddResultModel = addResultModel }, addResultCmdMsgs |> List.map AddResultCmdMsg
        | SettingsMsg settingsMsg ->
            let settingsModel, settingsCmdMsgs = Settings.update model.SettingsModel settingsMsg
            let updatedModel = { model with SettingsModel = settingsModel }

            match settingsMsg with
            | Settings.SettingsSaved newSettings ->
                let updatedSettingsModel = { settingsModel with Settings = newSettings }
                { updatedModel with ApplicationSettings = newSettings |> Settings.toApplicationSettings; SettingsModel = updatedSettingsModel }, settingsCmdMsgs |> List.map SettingsCmdMsg
            | _ -> updatedModel, settingsCmdMsgs |> List.map SettingsCmdMsg
        | ShowTimer -> model, [ShowTimerCmdMsg]
//        | SetSelectedTabMsg tabIndex -> model, [SetSelectedTabCmdMsg tabIndex]

    let navigationPrimaryColor = Color.FromHex("#2196F3")

    let view (model: Model) dispatch =
        View.Shell(
            ref=shellRef,
            shellBackgroundColor = navigationPrimaryColor,
            shellForegroundColor = Color.White,
            shellTitleColor = Color.White,
            shellDisabledColor = Color.FromHex("#B4FFFFFF"),
            shellUnselectedColor = Color.FromHex("#95FFFFFF"),
            shellTabBarBackgroundColor = navigationPrimaryColor,
            shellTabBarForegroundColor = Color.White,
            shellTabBarUnselectedColor = Color.FromHex("#95FFFFFF"),
            shellTabBarTitleColor = Color.White,
            items = [
                View.TabBar(
                    created = (fun tabBar -> tabBar.CurrentItem <- tabBar.Items.[model.SelectedTabIndex]),
                    items = [
                        View.Tab(
                            title = "Select Opponent",
                            icon = Image.Path "tab_about.png",
                            isEnabled = (model.ApplicationSettings |> areSet),
                            items = [
                                View.ShellContent(
                                    content = OpponentList.view model.OpponentListModel (Msg.OpponentListMsg >> dispatch)
                                )
                            ])
                        View.Tab(
                            title = "Settings",
                            icon = Image.Path "tab_settings.png",
                            items = [
                                View.ShellContent(
                                    content = Settings.view model.SettingsModel (Msg.SettingsMsg >> dispatch)
                                )
                            ])
                        View.Tab(
                            title = "Add Result",
                            icon = Image.Path "tab_feed.png",
                            items = [
                                View.ShellContent(
                                    content =
                                        (AddResult.view model.AddResultModel (Msg.AddResultMsg >> dispatch))
                                            .ToolbarItems(
                                                [
                                                    View.ToolbarItem(
                                                        text = "Timer",
                                                        command = (fun () -> dispatch ShowTimer)
                                                    )
                                                ])
                                )
                            ])
                        View.Tab(
                            title = "About",
                            icon = Image.Path "tab_about.png",
                            items = [
                                View.ShellContent(
                                    content = About.view model.AboutModel
                                )
                            ])
                    ])
            ])

    // Note, this declaration is needed if you enable LiveUpdate
    let program = Program.mkProgramWithCmdMsg init update view mapCommands

type App () as app =
    inherit Application ()

    let runner =
        App.program
#if DEBUG
        |> Program.withConsoleTrace
#endif
        |> XamarinFormsProgram.run app

#if DEBUG
    // Uncomment this line to enable live update in debug mode.
    // See https://fsprojects.github.io/Fabulous/Fabulous.XamarinForms/tools.html#live-update for further  instructions.
    //
    //do runner.EnableLiveUpdate()
#endif

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


