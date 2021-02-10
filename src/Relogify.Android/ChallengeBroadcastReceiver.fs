namespace Relogify.Android

open Android.Content
open Android.Support.V4.App
open Relogify.ChallengeManager

[<BroadcastReceiver(Enabled = true, Exported = false)>]
type ChallengeBroadcastReceiver() =
    inherit BroadcastReceiver()

    override this.OnReceive(context: Context, intent: Intent) =
        let notificationId = intent.GetIntExtra("EXTRA_NOTIFICATION_ID", 0)
        let challengeFrom = intent.GetStringExtra("EXTRA_CHALLENGE_FROM")
        let communityName = intent.GetStringExtra("EXTRA_COMMUNITY_NAME")

        removeChallengeFromLocalStorage { PlayerName = challengeFrom; CommunityName = communityName }
        |> Async.RunSynchronously
        |> ignore

        match intent.Action with
        | "ACTION_ACCEPT" ->
            respondToChallengeApiCall ChallengeResponse.Accept notificationId
            |> Async.Ignore
            |> Async.StartImmediate
            |> ignore
        | "ACTION_DECLINE" ->
            respondToChallengeApiCall ChallengeResponse.Decline notificationId
            |> Async.Ignore
            |> Async.StartImmediate
            |> ignore
        | _ -> ()

        let notificationManagerCompat = NotificationManagerCompat.From(context.ApplicationContext)
        notificationManagerCompat.Cancel(notificationId)
