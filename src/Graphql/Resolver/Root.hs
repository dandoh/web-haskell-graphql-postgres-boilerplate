module Graphql.Resolver.Root where

import Data.Morpheus.Types
import Graphql
import Graphql.Resolver.User

rootResolver :: GQLRootResolver Web () Query Mutation Undefined
rootResolver =
    GQLRootResolver {queryResolver, mutationResolver, subscriptionResolver}
  where
    queryResolver = Query {login = loginResolver}
    mutationResolver = Mutation {register = registerResolver}
    subscriptionResolver = Undefined
