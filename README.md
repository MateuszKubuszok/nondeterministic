# Non-Deterministic

A PoC of non-deterministic effects (assumes effect is referentially transparent is sth).

(Not tested, just a draft)

## Idea

```scala
// these are deterministic effects
def createUser(newUser: CreateUser): F[User]
def logout(user: User): F[Unit]
def checkSomePage(user: User): F[Page]

// this makes effects non-deterministic
// we can combine 2 branches and only one of them will be followed
def ndLogout(user: User): NonDeterministicT[F, Unit] = NonDeterministicT.liftF(logout(user))
def ndCheckPage(user: User): NonDeterministicT[F, Unit] => NonDeterministicT.liftF(checkSomePage(user))
def ndUserBehavior(user: User): NonDeterministicT[F, Unit] =
  // The first path will have its likelihood increased 9 times,
  // the second path will have its original weight (1),
  // which should result in branch picking the first branch 9 times out of 9+1=10,
  // and the second branch 1 time out of 10.
  ndCheckPage(user).flatMap(_ => ndUserBehavior(user)).scale(9) <+> ndLogout(user)

// this should create user and then randomly decide between
// checking page and logging out - once logged out, it should finish
NonDeterministicT.liftF(createUser(newUser)) // NonDeterministicT[F, User]
  .flatMap(ndUserBehavior)                 // NonDeterministicT[F, Unit]
  .execute(Decider.random())               // F[Unit]
```
