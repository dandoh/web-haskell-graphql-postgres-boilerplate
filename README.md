# Haskell Webserver Skeleton
- Graphql API
  - morpheus-graphql (https://github.com/morpheusgraphql/morpheus-graphql)
  - Schema is in `schema/schema.graphql`
- Database:
  - Postgresql + Opaleye
  - Migration using `dbmate` (https://github.com/amacneil/dbmate)
  - Pooling using `Data.Pool`

The skeleton wires up:
- Reading .env using `envy`
- Database
- Graphql API
  - `login(email: String, password: String) { token user { id } }`
  - `register(email: String, password: String, name: String) { token user { id } }`
- Authentication using JWT
- Monad transformers
