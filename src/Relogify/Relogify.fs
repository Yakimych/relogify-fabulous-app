﻿// Copyright 2018-2019 Fabulous contributors. See LICENSE.md for license.
namespace Relogify

open System.Diagnostics
open Fabulous
open Fabulous.XamarinForms
open Fabulous.XamarinForms.LiveUpdate
open Routes
open Xamarin.Forms

module App =
    type Model =
      { SomeFlag : bool
        SettingsModel : Settings.Model
        AddResultModel : AddResult.Model
        AboutModel : About.Model }

    type Msg =
        | AddResultMsg of AddResult.Msg
        | SettingsMsg of Settings.Msg
        | ShowTimer

    type CmdMsg =
        | AddResultCmdMsg of AddResult.CmdMsg
        | SettingsCmdMsg of Settings.CmdMsg
        | ShowTimerCmdMsg

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
        | AddResultCmdMsg x -> AddResult.mapCommands x |> Cmd.map AddResultMsg
        | SettingsCmdMsg x -> Settings.mapCommands x |> Cmd.map SettingsMsg
        | ShowTimerCmdMsg -> showTimer ()

    let initModel () =
        { SomeFlag = false
          AboutModel = About.initModel
          AddResultModel = AddResult.initModel
          SettingsModel = Settings.initModel }

    let init () =
        Routing.RegisterRoute("TestRoute", typeof<TestRoutingPage>)
        initModel (), []

    let update msg model =
        match msg with
        | AddResultMsg addResultMsg ->
            let addResultModel, addResultCmdMsgs = AddResult.update model.AddResultModel addResultMsg
            { model with AddResultModel = addResultModel }, addResultCmdMsgs |> List.map AddResultCmdMsg
        | SettingsMsg settingsMsg ->
            let settingsModel, settingsCmdMsgs = Settings.update model.SettingsModel settingsMsg
            { model with SettingsModel = settingsModel }, settingsCmdMsgs |> List.map SettingsCmdMsg
        | ShowTimer -> model, [ShowTimerCmdMsg]

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
                View.TabBar(items = [
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
                        title = "Settings",
                        icon = Image.Path "tab_settings.png",
                        items = [
                            View.ShellContent(
                                content = Settings.view model.SettingsModel (Msg.SettingsMsg >> dispatch)
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


