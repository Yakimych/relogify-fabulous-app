namespace Relogify.Android

open Android.Content
open Android.Support.V4.App

[<BroadcastReceiver(Enabled = true, Exported = false)>]
type MyBroadcastReceiver() =
    inherit BroadcastReceiver()

    override this.OnReceive(context: Context, intent: Intent) =
        // TODO: extract player name and community name in order to respond

        match intent.Action with
        | "ACTION_ACCEPT" -> ()
        | "ACTION_DECLINE" -> ()
        | _ -> ()

        let notificationManagerCompat = NotificationManagerCompat.From(context.ApplicationContext)
        notificationManagerCompat.Cancel(MainActivity.NOTIFICATION_ID)