module Relogify.Graphql

open FSharp.Data.GraphQL

type MyProvider = GraphQLProvider<"schema.json">

let getPlayersOperation =
    MyProvider.Operation<"""query q($communityName: String!) {
        players(where: { community: { name: { _eq: $communityName } } }) {
            id
            name
        }
    }
    """>()

let runtimeContext: GraphQLProviderRuntimeContext =
    { ServerUrl = ConfigManager.getApiUrl ()
      HttpHeaders = [] }
