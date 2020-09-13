namespace Relogify

type IMessagingService =
    abstract SendRegistrationToServer: unit -> Async<unit>