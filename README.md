# Modern Haskell Webserver Boilerplate
- Graphql API
  - morpheus-graphql (https://github.com/morpheusgraphql/morpheus-graphql)
  - Schema is in `schema.graphql`
  ```graphql
    type User {
        id: Int!
        email: String!
        name: String!
        updatedAt: String!
        createdAt: String!
    }

    type Session {
        token: String!
        user: User!
    }

    type Query {
        login(email: String!, password: String!): Session!
        myUserInfo: User!
    }

    type Mutation {
        register(email: String!, password: String!, name: String!): Session!
        changePassword(oldPassword: String!, newPassword: String!): Boolean!
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
- Authorization using JWT
- Monad transformers

## Running
- Feed in you database & secret in `.env`:
  ```terminal
  $ cp .env.default .env
  ```
  ```env
  DATABASE_URL="postgres://Dandoh:dandoh@127.0.0.1:5432/webhaskell?sslmode=disable"
  JWT_SECRET="my_jwt_secret"
  ```
- Migrations
  ```terminal
  $ dbmate up
  ```
  - More uses refer https://github.com/amacneil/dbmate
- Run webserver
  ```terminal
  $ stack run
  ```
  
Now GraphQL API is at [http://localhost:8080/api](http://localhost:8080/api)

You can also access Graphql Playground at [http://localhost:8080/graphiql](http://localhost:8080/graphiql)

![Playground](images/playground.png)



## Running on Docker
- Feed in you database & secret in `.env`:
  ```terminal
  $ cp .env.default .env
  ```
- (Optional) Edit anything you need in the .env file
- Create and start docker containers
  ```terminal
  $ docker-compose up -d
  ```
- Enter inside the docker workspace container
  ```terminal
  $ docker exec -it web-haskell-graphql-postgres-boilerplate_workspace_1 /bin/bash
  ```
- Migrations
  ```terminal
  $ dbmate up
  ```
- Run webserver
  ```terminal
  $ stack run
  ```
- Now you can visit: http://localhost:8080/ in your local machine.



## Contributors

PR are more than welcome. The only note is we use `ormolu` to format codes. 

- [Nhan Thai](https://github.com/dandoh)
- [Pacific01](https://github.com/Pacific01)
