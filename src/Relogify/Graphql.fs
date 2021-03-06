module Relogify.Graphql

open FSharp.Data.GraphQL

type MyProvider = GraphQLProvider<"schema.json">

let getPlayersOperation =
    MyProvider.Operation<"""query getPlayers($communityName: String!) {
        players(where: { community: { name: { _eq: $communityName } } }) {
            id
            name
        }
    }
    """>()

let getPlayersForCommunitiesOperation =
    MyProvider.Operation<"""query getPlayersForCommunities($communityNames: [String!]!) {
        players(where: { community: { name: { _in: $communityNames } } }) {
            id
            name
            community {
                id
                name
            }
        }
    }
    """>()

type PlayersForCommunities = MyProvider.Operations.GetPlayersForCommunities.Types.Query_root

let addResultOperation =
    MyProvider.Operation<"""mutation addResult(
        $communityName: String!
        $player1Name: String!
        $player2Name: String!
        $date: timestamptz!
        $player1Goals: Int!
        $player2Goals: Int!
        $extraTime: Boolean!) {
          insert_results(
            objects: [{
              community: {
                data: { name: $communityName }
                on_conflict: {
                  constraint: communities_name_key
                  update_columns: [name]
                }
              }
              date: $date
              player1: {
                data: {
                  name: $player1Name
                  community: {
                    data: { name: $communityName }
                    on_conflict: {
                      constraint: communities_name_key
                      update_columns: [name]
                    }
                  }
                }
                on_conflict: {
                  constraint: players_name_communityId_key
                  update_columns: [name]
                }
              }
              player2: {
                data: {
                  name: $player2Name
                  community: {
                    data: { name: $communityName }
                    on_conflict: {
                      constraint: communities_name_key
                      update_columns: [name]
                    }
                  }
                }
                on_conflict: {
                  constraint: players_name_communityId_key
                  update_columns: [name]
                }
              }
              player2goals: $player2Goals
              player1goals: $player1Goals
              extratime: $extraTime
            }]
          ) {
            returning {
              id
            }
          }
        }""">()

let runtimeContext: GraphQLProviderRuntimeContext =
    { ServerUrl = ConfigManager.getApiUrl ()
      HttpHeaders = [] }
