namespace Relogify.Android

open Android.Util
open Firebase.Messaging
open Android.Support.V4.App
open Relogify
open Relogify.ApplicationSettings
open Android.App
open Android.Content
open Xamarin.Forms
open ChallengeManager
open System.Collections.Generic

type ResourceAlias = Resource

[<Service>]
[<IntentFilter([| "com.google.firebase.MESSAGING_EVENT" |])>]
[<IntentFilter([| "com.google.firebase.INSTANCE_ID_EVENT" |])>]
type AndroidFirebaseMessagingService() =
    inherit FirebaseMessagingService()

    let TAG: string = "RelogifyFirebaseMsgService"
    let androidTokenPropertyKey: string = "android_notification_hub_token"

    interface IMessagingService with
        member this.SendRegistrationToServer (): Async<unit> =
            async {
                let applicationSettings = getApplicationSettings ()
                let topics = applicationSettings.Communities |> List.map (fun c -> sprintf "%s_%s" c.CommunityName c.PlayerName)

                try
                    // TODO: Unsubscribe from all topics before resubscribing
                    topics
                    |> List.map MessageUtils.base64UrlEncode
                    |> List.iter (fun encodedTopic ->
                        FirebaseMessaging.Instance.SubscribeToTopic(encodedTopic) |> ignore
                    )

                    // TODO: Fetch the token (alternatively via FirebaseInstance) and register via our own notification Hub
                    // NOTE: Topic subscriptions can be handled server-side too
    //                let savedToken = Application.Current.Properties.[androidTokenPropertyKey] :?> string
    //                let regID = hub.Register(savedToken, tags |> Array.ofList).RegistrationId
                with
                    | ex -> Log.Error(TAG, ex.Message) |> ignore
            }

        member this.CancelNotification (notificationId: int): unit =
            let notificationManagerCompat = NotificationManagerCompat.From(Application.Context)
            notificationManagerCompat.Cancel(notificationId)

    member private this.GetNotificationBuilder (notificationTitle: string) (notificationText: string) =
        let intent = (new Intent(this, typedefof<MainActivity>)).AddFlags(ActivityFlags.ClearTop)
        let pendingIntent = PendingIntent.GetActivity(this, 0, intent, PendingIntentFlags.OneShot)

        (new NotificationCompat.Builder(this, MainActivity.CHANNEL_ID))
            .SetSmallIcon(Resources.Drawable.relogify_icon_32)
            .SetContentTitle(notificationTitle)
            .SetContentText(notificationText)
            .SetAutoCancel(true)
            .SetShowWhen(false)
            .SetContentIntent(pendingIntent)

    member private this.GetButtonPendingIntent (action: string) (playerInCommunity: PlayerInCommunity) (notificationId: int) =
        let intent =
            (new Intent(this, typedefof<ChallengeBroadcastReceiver>))
                .SetAction(action)
                .PutExtra("EXTRA_NOTIFICATION_ID", notificationId)
                .PutExtra("EXTRA_CHALLENGE_FROM", playerInCommunity.PlayerName)
                .PutExtra("EXTRA_COMMUNITY_NAME", playerInCommunity.CommunityName)

        PendingIntent.GetBroadcast(this, notificationId, intent, PendingIntentFlags.OneShot)

    member private this.SendNotification (messageData: IDictionary<string, string>) =
        let notificationId = messageData.["challengeId"] |> int // TODO: Error handling

        let communityName = messageData.["communityName"] // TODO: Error handling
        let notificationFrom = messageData.["fromPlayer"] // TODO: Error handling
        let notificationTitle = messageData.["title"] // TODO: Error handling
        let notificationText = messageData.["message"] // TODO: Error handling
        let playerInCommunity = { CommunityName = communityName; PlayerName = notificationFrom }

        let isIncomingChallenge = messageData.ContainsKey("responseType") |> not

        if isIncomingChallenge then
            addChallengeToLocalStorage playerInCommunity (ChallengeType.Incoming notificationId) |> Async.RunSynchronously |> ignore

            let acceptPendingIntent = this.GetButtonPendingIntent "ACTION_ACCEPT" playerInCommunity notificationId
            let declinePendingIntent = this.GetButtonPendingIntent "ACTION_DECLINE" playerInCommunity notificationId

            let notificationBuilder =
                (this.GetNotificationBuilder notificationTitle notificationText)
                    .AddAction(0, "Accept", acceptPendingIntent)
                    .AddAction(0, "Decline", declinePendingIntent)

            let notificationManager = NotificationManager.FromContext(this)
            notificationManager.Notify(notificationId, notificationBuilder.Build())
        else
            // If a challenge is received, and an outgoing challenge is in the list, remove the challenge and show "Accepted" notification
            removeChallengeFromLocalStorage playerInCommunity |> Async.RunSynchronously |> ignore

            let notificationBuilder = this.GetNotificationBuilder notificationTitle notificationText

            let notificationManager = NotificationManager.FromContext(this)
            notificationManager.Notify(notificationId, notificationBuilder.Build())

    override this.OnMessageReceived(message: RemoteMessage) =
        Log.Debug(TAG, sprintf "From: %s" message.From) |> ignore
        let notification = message.GetNotification()
        if notification <> null then
            Log.Debug(TAG, sprintf "Notification Message Body: %A" notification.Body) |> ignore
        else
            Log.Debug(TAG, sprintf "Notification is null. Message data values head: %A" (message.Data.Values |> Seq.head))
            |> ignore

        this.SendNotification message.Data

    override this.OnNewToken (token: string) =
        Log.Debug(TAG, sprintf "FCM token: %s" token)
        |> ignore

        async {
            Application.Current.Properties.[androidTokenPropertyKey] <- token
            do! Application.Current.SavePropertiesAsync() |> Async.AwaitTask

            do! (this :> IMessagingService).SendRegistrationToServer ()
        } |> Async.StartAsTask |> ignore

[<assembly: Dependency(typeof<AndroidFirebaseMessagingService>)>]
do ()
