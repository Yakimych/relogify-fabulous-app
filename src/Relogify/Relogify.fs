﻿namespace Relogify

open AddResult
open Fabulous
open Fabulous.XamarinForms
open Relogify.ApplicationSettings
open Relogify.Styles
open Xamarin.Forms

module App =
    type Page =
        | AddResult of PlayerInCommunity
        | Timer

    type Model =
      { PageStack : Page list
        SelectedTabIndex : int
        OpponentListModel : OpponentList.Model
        SettingsModel : Settings.Model
        FirstRunModel : FirstRun.Model
        AddResultModel : AddResult.Model
        TimerModel : Timer.Model
        ApplicationSettings : ApplicationSettings }

    type Msg =
        | OpponentListMsg of OpponentList.Msg
        | AddResultMsg of AddResult.Msg
        | TimerMsg of Timer.Msg
        | FirstRunMsg of FirstRun.Msg
        | SettingsMsg of Settings.Msg
        | SetCurrentPage of tabIndex: int
        | PushPage of Page
        | PopPage
        | PopBackToHome
        | SettingsUpdated of ApplicationSettings

    type CmdMsg =
        | OpponentListCmdMsg of OpponentList.CmdMsg
        | AddResultCmdMsg of AddResult.CmdMsg
        | SettingsCmdMsg of Settings.CmdMsg
        | FirstRunCmdMsg of FirstRun.CmdMsg
        | TimerCmdMsg of Timer.CmdMsg
        | PopLastPageCmdMsg
        | UpdateApplicationSettings of ApplicationSettings

    let navigationPageRef = ViewRef<NavigationPage>()

    let forcePopLastPage () =
        navigationPageRef.TryValue
        |> Option.iter (fun navigationPage -> navigationPage.PopAsync() |> Async.AwaitTask |> Async.Ignore |> Async.StartImmediate)

        Cmd.none

    let updateSettingsCmd (settings : ApplicationSettings) =
        async {
            do! Async.SwitchToThreadPool ()

            do! saveApplicationSettings settings |> Async.AwaitTask
            return getApplicationSettings () |> SettingsUpdated
        } |> Cmd.ofAsyncMsg

    let mapCommands (cmdMsg: CmdMsg): Cmd<Msg> =
        match cmdMsg with
        | OpponentListCmdMsg x -> OpponentList.mapCommands x |> Cmd.map OpponentListMsg
        | AddResultCmdMsg x -> AddResult.mapCommands x |> Cmd.map AddResultMsg
        | SettingsCmdMsg x -> Settings.mapCommands x |> Cmd.map SettingsMsg
        | FirstRunCmdMsg x -> FirstRun.mapCommands x |> Cmd.map FirstRunMsg
        | TimerCmdMsg x -> Timer.mapCommands x |> Cmd.map TimerMsg
        | PopLastPageCmdMsg -> forcePopLastPage ()
        | UpdateApplicationSettings settings -> updateSettingsCmd settings

    let selectOpponentTabIndex = 0
    let settingsTabIndex = 1

    let getPlayerInCommunityIfAddingResult (pageType: Page) =
        match pageType with
        | AddResult playerInCommunity -> Some(playerInCommunity)
        | _ -> None

    let isAddingResultFor (model: Model): PlayerInCommunity option =
        model.PageStack
        |> List.choose getPlayerInCommunityIfAddingResult
        |> List.tryHead

    let init () =
        let applicationSettings = getApplicationSettings ()

        let opponentListModel, opponentListCmdMsgs = applicationSettings.Communities |> OpponentList.initModel
        let addResultModel, addResultCmdMsgs = AddResult.initModel ()
        let cmdMsgs = (opponentListCmdMsgs |> List.map OpponentListCmdMsg) @ (addResultCmdMsgs |> List.map AddResultCmdMsg)

        { PageStack = []
          SelectedTabIndex = if applicationSettings |> areSet then selectOpponentTabIndex else settingsTabIndex
          OpponentListModel = opponentListModel
          AddResultModel = addResultModel
          TimerModel = Timer.initModel
          ApplicationSettings = applicationSettings
          FirstRunModel = FirstRun.initModel ()
          SettingsModel = Settings.initModel true }, cmdMsgs

    let pushPage (page: Page) (model: Model): Model * CmdMsg list =
        let modelWithNewPage = { model with PageStack = page :: model.PageStack }
        match page with
        | AddResult _ ->
            let addResultModel, addResultCmdMsgs = AddResult.initModel ()
            { modelWithNewPage with AddResultModel = addResultModel }, addResultCmdMsgs |> List.map AddResultCmdMsg
        | Timer ->
            { modelWithNewPage with TimerModel = Timer.initModel }, []

    let popPage (model: Model): Model * CmdMsg list =
        match model.PageStack with
        | [] -> model, []
        | pageToPop :: restOfPages ->
            let newModel = { model with PageStack = restOfPages }
            match pageToPop with
            | Timer -> { newModel with TimerModel = Timer.initModel }, []
            | AddResult _ ->
                let addResultModel, addResultCmdMsgs = AddResult.initModel ()
                { newModel with AddResultModel = addResultModel }, addResultCmdMsgs |> List.map AddResultCmdMsg

    let addResultPageShouldBePopped (addResultMsg: AddResult.Msg): bool =
        match addResultMsg with
        | AddResult.ResultAddedSuccess -> true
        | _ -> false

    let update msg (model: Model) =
        match msg with
        | OpponentListMsg opponentListMsg ->
            let opponentListModel, opponentListCmdMsgs, opponentListOutMsg = OpponentList.update model.OpponentListModel opponentListMsg
            match opponentListOutMsg with
            | Some (OpponentList.PlayerSelectedOutMsg playerInCommunity) ->
                let pushedPage, cmdMsgs = model |> pushPage (AddResult playerInCommunity)
                pushedPage, (opponentListCmdMsgs |> List.map OpponentListCmdMsg) @ cmdMsgs
            | None ->
                { model with OpponentListModel = opponentListModel }, opponentListCmdMsgs |> List.map OpponentListCmdMsg

        | AddResultMsg addResultMsg ->
            // TODO: Refactor/decouple the logic
            let maybeOpponentInCommunity = model |> isAddingResultFor
            let currentPlayerInCommunity = model.ApplicationSettings |> getCurrentPlayerInCommunity model.OpponentListModel.currentCommunityName
            match maybeOpponentInCommunity with
            | Some opponentInCommunity ->
                let addResultModel, addResultCmdMsgs = AddResult.update model.AddResultModel addResultMsg currentPlayerInCommunity.CommunityName currentPlayerInCommunity.PlayerName opponentInCommunity.PlayerName
                { model with AddResultModel = addResultModel }, (addResultCmdMsgs |> List.map AddResultCmdMsg) @ [if addResultPageShouldBePopped addResultMsg then yield PopLastPageCmdMsg]
            | _ -> model, []
        | TimerMsg timerMsg ->
            let timerModel, timerCmdMsgs, timerOutMsg = Timer.update model.TimerModel timerMsg
            match timerOutMsg with
            | Some (Timer.OutMsg.TimerExpiredOutMsg timerWasRunInExtraTime) ->
                let updatedResultModel = { model.AddResultModel.resultModel with ExtraTime = timerWasRunInExtraTime }
                let updatedAddResultModel = { model.AddResultModel with resultModel = updatedResultModel }
                let updatedModel =
                    { model with
                          TimerModel = Timer.initModel
                          AddResultModel = updatedAddResultModel }

                updatedModel, (timerCmdMsgs |> List.map TimerCmdMsg) @ [PopLastPageCmdMsg]
            | _ ->
                { model with TimerModel = timerModel }, timerCmdMsgs |> List.map TimerCmdMsg
        | SettingsMsg settingsMsg ->
            let settingsModel, settingsCmdMsgs, settingsOutMsg = Settings.update model.SettingsModel model.ApplicationSettings settingsMsg
            let updatedModel = { model with SettingsModel = settingsModel }
            let appCmdMsgsFromSettings = settingsCmdMsgs |> List.map SettingsCmdMsg

            match settingsOutMsg with
            | Some (Settings.OutMsg.SettingsUpdated newSettings) ->
                updatedModel, (UpdateApplicationSettings newSettings) :: appCmdMsgsFromSettings
            | None -> updatedModel, appCmdMsgsFromSettings

        | FirstRunMsg firstRunMsg ->
            let firstRunModel, firstRunCmdMsgs, maybeFirstRunOutMsg = FirstRun.update model.FirstRunModel firstRunMsg
            let updatedModel = { model with FirstRunModel = firstRunModel }
            let appCmdMsgsFromFirstRun = firstRunCmdMsgs |> List.map FirstRunCmdMsg

            match maybeFirstRunOutMsg with
            | None -> updatedModel, appCmdMsgsFromFirstRun
            | Some (FirstRun.OutMsg.FirstRunComplete (communityName, playerName)) ->
                updatedModel, [ { Communities = [{ CommunityName = communityName; PlayerName = playerName } ] } |> UpdateApplicationSettings]

        | SettingsUpdated newSettings ->
            let opponentListModel, opponentListCmdMsgs = newSettings.Communities |> OpponentList.initModel
            let cmdMsgs = opponentListCmdMsgs |> List.map OpponentListCmdMsg

            { model with ApplicationSettings = newSettings; OpponentListModel = opponentListModel }, cmdMsgs

        | SetCurrentPage tabIndex -> { model with    SelectedTabIndex = tabIndex }, []
        | PushPage page -> model |> pushPage page
        | PopPage -> model |> popPage
        | PopBackToHome -> { model with PageStack = [] }, []

    let getPageTitle (tabIndex: int) =
        match tabIndex with
        | 0 -> "Select Opponent"
        | 1 -> "Settings"
        | 2 -> "About"
        | _ -> failwith (sprintf "No tab with index %d" tabIndex)

    let timerIsShown (model: Model) = model.PageStack |> List.contains Timer

    let renderPage (model: Model) dispatch (page: Page) =
        match page with
        | AddResult playerInCommunityToAddResultsFor ->
            let selectedCommunityName = playerInCommunityToAddResultsFor.CommunityName
            let currentPlayerInCommunity = model.ApplicationSettings |> getCurrentPlayerInCommunity selectedCommunityName

            (AddResult.view model.AddResultModel (Msg.AddResultMsg >> dispatch) currentPlayerInCommunity.PlayerName playerInCommunityToAddResultsFor.PlayerName playerInCommunityToAddResultsFor.CommunityName)
                .ToolbarItems([ View.ToolbarItem( text = "Timer", command = (fun () -> dispatch <| PushPage Timer)) ])
        | Timer ->
            Timer.view model.TimerModel (Msg.TimerMsg >> dispatch)

    let view (model: Model) (dispatch: Msg -> unit): ViewElement =
        let pagesInTheStack: ViewElement seq =
            model.PageStack |> Seq.map (renderPage model dispatch) |> Seq.rev

        match model.ApplicationSettings.Communities with
        | [] ->
            FirstRun.view model.FirstRunModel (Msg.FirstRunMsg >> dispatch)
        | communities ->
            View.NavigationPage(
                ref = navigationPageRef,
                barBackgroundColor = navigationPrimaryColor,
                barTextColor = barTextColor,
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
                        unselectedTabColor = unselectedTabColor,
                        selectedTabColor = Color.White,
                        barTextColor = Color.White,
                        children = [
                            yield OpponentList.view communities model.OpponentListModel (Msg.OpponentListMsg >> dispatch)

                            yield (Settings.view
                                model.SettingsModel
                                communities
                                (Msg.SettingsMsg >> dispatch))

                            yield About.view ()
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


