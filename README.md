# Non-Deterministic

A PoC of non-deterministic effects (assumes effect is referentially transparent is sth).

(Not tested, just a draft)

## Idea

```scala
def createUser: CreateUser => F[User]
def logout: User => F[Unit]
def checkSomePage: User => F[Page]

def ndUserBehavior: User => NonDeterministic[F, Unit] = { user =>

 val ndLogout = NonDeterministic.lift(logout) // weight = 1
 val ndCheckPage = NonDeterministic.lift(checkSomePage).scale(9) // weight = 9

  ndCheckPage.flatMap(_ => ndUserBehavior(user)) <+> ndLogout
}

// should create user and then randomly decides between
// checking page and logging out - once logged out, it should finish
NonDeterministic.lift(createUser)
  .flatMap(ndUserBehavior)
  .execute(Decider.random()) // F[Unit]
```
