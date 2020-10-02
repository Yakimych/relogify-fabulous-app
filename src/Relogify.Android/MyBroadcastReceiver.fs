namespace Relogify.Android

open Android.Content
open Android.Support.V4.App
open Relogify

[<BroadcastReceiver(Enabled = true, Exported = false)>]
type MyBroadcastReceiver() =
    inherit BroadcastReceiver()

    override this.OnReceive(context: Context, intent: Intent) =
        // TODO: Special template, so that we don't have to parse the string
        let challengeFrom = intent.GetStringExtra("CHALLENGE_FROM")
        let communityName = intent.GetStringExtra("COMMUNITY_NAME")

        ApplicationSettings.removeChallengeFromLocalStorage { PlayerName = challengeFrom; CommunityName = communityName }
        |> Async.RunSynchronously
        |> ignore

        match intent.Action with
        | "ACTION_ACCEPT" ->
            ChallengeManager.respondToChallenge challengeFrom communityName |> Async.RunSynchronously
        | "ACTION_DECLINE" ->
            // TODO: Send back response
            ()
        | _ -> ()

        let notificationManagerCompat = NotificationManagerCompat.From(context.ApplicationContext)
        notificationManagerCompat.Cancel(MainActivity.NOTIFICATION_ID)