# Modern Haskell Webserver Boilerplate
- Graphql API
  - morpheus-graphql (https://github.com/morpheusgraphql/morpheus-graphql)
  - Schema is in `schema.graphql`
  ```graphql
  type User {
    id: Int!
    email: String!
    name: String!
  }

  type Session {
    token: String!
    user: User!
  }

  type Query {
    login(email: String!, password: String!): Session!
  }

  type Mutation {
    register(email: String!, password: String!, name: String!): Session!
  }
  ```
- Database:
  - Postgresql + Opaleye
  - Migration using `dbmate` (https://github.com/amacneil/dbmate)
  - Pooling using `Data.Pool`

This boilerplate wires up:
- Reading .env using `envy`
- Database
- Graphql API
  - `login(email: String, password: String) { token user { id } }`
  - `register(email: String, password: String, name: String) { token user { id } }`
- Authentication using JWT
- Monad transformers
