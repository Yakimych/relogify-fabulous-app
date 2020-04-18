﻿namespace Relogify

open AddResult
open Fabulous
open Fabulous.XamarinForms
open Relogify.ApplicationSettings
open Xamarin.Forms

module App =
    type Page =
        | AddResult of opponentName: string
        | Timer

    type Model =
      { PageStack : Page list
        SelectedTabIndex : int
        OpponentListModel : OpponentList.Model
        SettingsModel : Settings.Model
        AddResultModel : AddResult.Model
        TimerModel : Timer.Model
        ApplicationSettings : ApplicationSettings
        AboutModel : About.Model }

    type Msg =
        | OpponentListMsg of OpponentList.Msg
        | AddResultMsg of AddResult.Msg
        | TimerMsg of Timer.Msg
        | SettingsMsg of Settings.Msg
        | SetCurrentPage of tabIndex: int
        | PushPage of Page
        | PopPage
        | PopBackToHome

    type CmdMsg =
        | OpponentListCmdMsg of OpponentList.CmdMsg
        | AddResultCmdMsg of AddResult.CmdMsg
        | SettingsCmdMsg of Settings.CmdMsg
        | TimerCmdMsg of Timer.CmdMsg

    let mapCommands cmdMsg =
        match cmdMsg with
        | OpponentListCmdMsg x -> OpponentList.mapCommands x |> Cmd.map OpponentListMsg
        | AddResultCmdMsg x -> AddResult.mapCommands x |> Cmd.map AddResultMsg
        | SettingsCmdMsg x -> Settings.mapCommands x |> Cmd.map SettingsMsg
        | TimerCmdMsg x -> Timer.mapCommands x |> Cmd.map TimerMsg

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

    // TODO: save selected community in settings
    let getSelectedCommunity () = 
        let applicationSettings = getApplicationSettings ()
        applicationSettings.Communities 
        |> List.tryHead 

    let getSelectedCommunityOrDefault () =
        getSelectedCommunity() 
        |> Option.defaultWith (fun _ -> { CommunityName = ""; PlayerName = "" })

    let init () =
        let selectedCommunity = getSelectedCommunityOrDefault ()

        let opponentListModel, opponentListCmdMsgs = OpponentList.initModel selectedCommunity.CommunityName
        let cmdMsgs = opponentListCmdMsgs |> List.map OpponentListCmdMsg
        let applicationSettings = ApplicationSettings.getApplicationSettings ()

        { PageStack = []
          SelectedTabIndex = if applicationSettings |> areSet then selectOpponentTabIndex else settingsTabIndex
          OpponentListModel = opponentListModel
          AboutModel = About.initModel
          AddResultModel = AddResult.initModel
          TimerModel = Timer.initModel
          ApplicationSettings = applicationSettings
          SettingsModel = Settings.initModel true }, cmdMsgs

    let pushPage (page: Page) (model: Model): Model =
        { model with PageStack = [page] @ model.PageStack }

    let popPage (model: Model): Model =
        match model.PageStack with
        | [] -> model
        | pageToPop :: restOfPages ->
            let newModel = { model with PageStack = restOfPages }
            match pageToPop with
            | Timer -> { newModel with TimerModel = Timer.initModel }
            | AddResult _ -> { newModel with AddResultModel = AddResult.initModel }

    let navigationPageRef = ViewRef<NavigationPage>()

    let forcePopLastPage () =
        match navigationPageRef.TryValue with
        | None -> ()
        | Some navigationPage -> async { do! navigationPage.PopAsync() |> Async.AwaitTask |> Async.Ignore } |> Async.StartImmediate

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
            // TODO: Refactor/decouple the logic
            let maybeOpponentName = model |> isAddingResultFor
            let updatedModel =
                match addResultMsg with
                // TODO: Should this be an OutCmd?
                | AddResult.ResultAddedSuccess ->
                    do forcePopLastPage () // TODO: Should this be a command?
                    { model with AddResultModel = AddResult.initModel } // TODO: Remove this, since the AddResultModel is already reset in the popPage function
                | _ -> model

            let selectedCommunity = getSelectedCommunityOrDefault ()
            match  maybeOpponentName with
            | Some opponentName ->
                let addResultModel, addResultCmdMsgs = AddResult.update updatedModel.AddResultModel addResultMsg selectedCommunity.CommunityName selectedCommunity.PlayerName opponentName
                { updatedModel with AddResultModel = addResultModel }, addResultCmdMsgs |> List.map AddResultCmdMsg
            | _ -> updatedModel, []
        | TimerMsg timerMsg ->
            let timerModel, timerCmdMsgs, timerOutMsg = Timer.update model.TimerModel timerMsg
            match timerOutMsg with
            | Some (Timer.OutMsg.TimerExpiredOutMsg timerWasRunInExtraTime) ->
                do forcePopLastPage () // TODO: Should this be a command?

                let updatedResultModel = { model.AddResultModel.resultModel with ExtraTime = timerWasRunInExtraTime }
                let updatedAddResultModel = { model.AddResultModel with resultModel = updatedResultModel }
                let updatedModel =
                    { model with
                          TimerModel = Timer.initModel
                          AddResultModel = updatedAddResultModel }

                updatedModel, timerCmdMsgs |> List.map TimerCmdMsg
            | _ ->
                { model with TimerModel = timerModel }, timerCmdMsgs |> List.map TimerCmdMsg
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
        | PopPage -> model |> popPage, [] // TODO: Check whether we're on the HomePage
        | PopBackToHome -> { model with PageStack = [] }, []

    let navigationPrimaryColor = Color.FromHex("#2196F3")

    let getPageTitle (tabIndex: int) =
        match tabIndex with
        | 0 -> "Select Opponent"
        | 1 -> "Settings"
        | 2 -> "Report result" // TODO: Remove
        | 3 -> "About"
        | _ -> failwith (sprintf "No tab with index %d" tabIndex)

    let timerIsShown (model: Model) = model.PageStack |> List.contains Timer

    let renderPage (model: Model) dispatch (page: Page) =
        match page with
        | AddResult playerNameToAddResultFor ->
            let currentPlayerOrEmpty = getSelectedCommunityOrDefault().PlayerName
            (AddResult.view model.AddResultModel (Msg.AddResultMsg >> dispatch) currentPlayerOrEmpty playerNameToAddResultFor) // TODO: This should not be yielded if the currentPlayer is empty
                .ToolbarItems([ View.ToolbarItem( text = "Timer", command = (fun () -> dispatch <| PushPage Timer)) ])
        | Timer ->
            Timer.view model.TimerModel (Msg.TimerMsg >> dispatch)

    let view (model: Model) dispatch =
        let pagesInTheStack: ViewElement seq =
            model.PageStack |> Seq.map (renderPage model dispatch) |> Seq.rev

        let selectedCommunity = getSelectedCommunity()
        let communities = (getApplicationSettings ()).Communities

        View.NavigationPage(
            ref = navigationPageRef,
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
                            match selectedCommunity with
                            | Some {PlayerName=playerName; CommunityName=communityName} ->
                                [OpponentList.view model.OpponentListModel playerName communityName (Msg.OpponentListMsg >> dispatch)]
                            | _ -> []

                        yield (Settings.view
                            model.SettingsModel
                            communities
                            (Msg.SettingsMsg >> dispatch))

                        yield About.view model.AboutModel
                ])

                yield! pagesInTheStack
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


