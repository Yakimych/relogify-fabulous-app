namespace Relogify.Android

open Android.Content
open Android.Support.V4.App

[<BroadcastReceiver(Enabled = true, Exported = false)>]
type MyBroadcastReceiver() =
    inherit BroadcastReceiver()

    override this.OnReceive(context: Context, intent: Intent) =
        // TODO: extract player name and community name in order to respond
        let challengeFrom = intent.GetStringExtra("CHALLENGE_FROM")
        let communityName = intent.GetStringExtra("COMMUNITY_NAME")

        Relogify.ApplicationSettings.removeChallengeFromLocalStorage { PlayerName = challengeFrom; CommunityName = communityName } |> Async.RunSynchronously |> ignore

        match intent.Action with
        | "ACTION_ACCEPT" ->
            // TODO: Send back response
            ()
        | "ACTION_DECLINE" ->
            // TODO: Send back response
            ()
        | _ -> ()

        let notificationManagerCompat = NotificationManagerCompat.From(context.ApplicationContext)
        notificationManagerCompat.Cancel(MainActivity.NOTIFICATION_ID)