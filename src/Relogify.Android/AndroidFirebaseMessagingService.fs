namespace Relogify.Android

open Android.Util
open Firebase.Messaging
open Android.Support.V4.App
open Relogify.ApplicationSettings
open WindowsAzure.Messaging
open Android.App
open Android.Content
open Xamarin.Forms

type ResourceAlias = Resource

[<Service>]
[<IntentFilter([| "com.google.firebase.MESSAGING_EVENT" |])>]
[<IntentFilter([| "com.google.firebase.INSTANCE_ID_EVENT" |])>]
type AndroidFirebaseMessagingService() =
    inherit FirebaseMessagingService()

    let TAG: string = "RelogifyFirebaseMsgService"
    let androidTokenPropertyKey: string = "android_notification_hub_token"

    interface Relogify.IMessagingService with
        member this.SendRegistrationToServer (): Async<unit> =
            async {
                let savedToken = Application.Current.Properties.[androidTokenPropertyKey] :?> string

                let listenConnectionString =
                    Relogify.ConfigManager.getListenConnectionString ()

                let notificationHubName =
                    Relogify.ConfigManager.getNotificationHubName ()

                let hub = new NotificationHub(notificationHubName, listenConnectionString, Android.App.Application.Context)

                let applicationSettings = getApplicationSettings ()
                let tags = applicationSettings.Communities |> List.map (fun c -> sprintf "%s_%s" c.CommunityName c.PlayerName)

                let regID = hub.Register(savedToken, tags |> Array.ofList).RegistrationId

                Log.Debug(TAG, sprintf "Successful registration of ID %s" regID) |> ignore
            }

    member private this.SendNotification(messageBody: string) =
        let playerInCommunity = messageBody |> Relogify.MessageUtils.parsePlayerInCommunity

        let challenges = getChallenges ()
        match challenges |> List.tryFind (fun c -> c.PlayerInCommunity = playerInCommunity) with
        | Some existingChallenge ->
            match existingChallenge.Type with
            | Outgoing ->
                // If a challenge is received, and an outgoing challenge is in the list, remove the challenge and show "Accepted" notification
                removeChallengeFromLocalStorage playerInCommunity |> Async.RunSynchronously |> ignore

                // TODO: Break into smaller functions
                // Show "Accepted" notification
                let intent =
                    new Intent(this, typedefof<MainActivity>)

                intent.AddFlags(ActivityFlags.ClearTop) |> ignore

                let pendingIntent =
                    PendingIntent.GetActivity(this, 0, intent, PendingIntentFlags.OneShot)

                let notificationBuilder =
                    new NotificationCompat.Builder(this, MainActivity.CHANNEL_ID)

                notificationBuilder
                    .SetSmallIcon(ResourceAlias.Drawable.ic_launcher)
                    .SetContentText(sprintf "%s: %s has accepted your challenge!" playerInCommunity.CommunityName playerInCommunity.PlayerName)
                    .SetAutoCancel(true)
                    .SetShowWhen(false)
                    .SetContentIntent(pendingIntent)
                |> ignore

                let notificationManager = NotificationManager.FromContext(this)
                notificationManager.Notify(MainActivity.NOTIFICATION_ID, notificationBuilder.Build())
            | Incoming ->
                // Do nothing if an incoming challenge already exists
                ()
        | None ->
            // If a challenge is received, and no outgoing challenge is in the list, add an incoming challenge and show "Accept/Decline" choice
            addChallengeToLocalStorage playerInCommunity ChallengeType.Incoming |> Async.RunSynchronously |> ignore

            let intent =
                new Intent(this, typedefof<MainActivity>)

            intent.AddFlags(ActivityFlags.ClearTop) |> ignore

            let pendingIntent =
                PendingIntent.GetActivity(this, 0, intent, PendingIntentFlags.OneShot)

            let acceptIntent = new Intent(this, typedefof<MyBroadcastReceiver>)
            acceptIntent.SetAction("ACTION_ACCEPT") |> ignore
            acceptIntent.PutExtra("EXTRA_NOTIFICATION_ID", 0) |> ignore
            acceptIntent.PutExtra("CHALLENGE_FROM", playerInCommunity.PlayerName) |> ignore
            acceptIntent.PutExtra("COMMUNITY_NAME", playerInCommunity.CommunityName) |> ignore

            let acceptPendingIntent = PendingIntent.GetBroadcast(this, 0, acceptIntent, PendingIntentFlags.OneShot)

            let notificationBuilder =
                new NotificationCompat.Builder(this, MainActivity.CHANNEL_ID)

            notificationBuilder
                .SetSmallIcon(ResourceAlias.Drawable.ic_launcher)
                .SetContentText(messageBody)
                .SetAutoCancel(true)
                .SetShowWhen(false)
                .SetContentIntent(pendingIntent)
                .AddAction(ResourceAlias.Drawable.ic_launcher, "Accept", acceptPendingIntent)
            |> ignore

            let notificationManager = NotificationManager.FromContext(this)
            notificationManager.Notify(MainActivity.NOTIFICATION_ID, notificationBuilder.Build())

    override this.OnMessageReceived(message: RemoteMessage) =
        Log.Debug(TAG, sprintf "From: %s" message.From)
        |> ignore
        let notification = message.GetNotification()
        if notification <> null then
            Log.Debug(TAG, sprintf "Notification Message Body: %A" notification.Body)
            |> ignore
            this.SendNotification(notification.Body)
        else
            this.SendNotification(message.Data.Values |> Seq.head)
        ()

    override this.OnNewToken (token: string) =
        Log.Debug(TAG, sprintf "FCM token: %s" token)
        |> ignore

        async {
            Application.Current.Properties.[androidTokenPropertyKey] <- token
            do! Application.Current.SavePropertiesAsync() |> Async.AwaitTask

            do! (this :> Relogify.IMessagingService).SendRegistrationToServer ()
        } |> Async.StartAsTask |> ignore

[<assembly: Dependency(typeof<AndroidFirebaseMessagingService>)>]
do ()
