import cats.{ ~>, Applicative, Functor, Monad, SemigroupK }
import cats.data.NonEmptyChain
import cats.free.Free

import scala.annotation.tailrec
import scala.util.Random

package object nondeterministic {

  /** Represents a node with possible branches and their weights */
  type Choices[F[_], A] = NonEmptyChain[Choice[F, A]]

  object Choices {

    def one[F[_], A](fa: F[A]): Choices[F, A] = NonEmptyChain.one(Choice(fa))

    type Scaler[F[_]] = Choices[F, *] ~> Choices[F, *]

    def scale[F[_]](multiplier: Int): Scaler[F] = new Scaler[F] {

      def apply[A](choices: Choices[F, A]): Choices[F, A] = choices.map(_ * multiplier)
    }
  }

  /** Represents a computation that might involve a non-deterministic choice */
  type NonDeterministic[F[_], A] = Free[Choices[F, *], A]

  object NonDeterministic {

    def apply[F[_], A](choices: Choices[F, A]): NonDeterministic[F, A] = Free.liftF(choices)

    def lift[F[_], A](fa: F[A]): NonDeterministic[F, A] = Free.liftF(Choices.one(fa))

    def pure[F[_], A](value: A): NonDeterministic[F, A] = Free.pure(value)
  }

  /** Picks branch when we run the computation */
  type Decider[F[_]] = Choices[F, *] ~> F

  object Decider {

    def random[F[_]](random: Random = new Random()): Decider[F] = new Decider[F] {

      @tailrec
      private def byWeight[A](choices: Choices[F, A], weight: Int): F[A] =
        if (choices.head.weight >= weight) choices.head.fa
        else {
          NonEmptyChain.fromChain(choices.tail) match {
            case Some(tail) => byWeight[A](tail, weight - choices.head.weight)
            case None       => choices.head.fa
          }
        }

      def apply[A](choices: Choices[F, A]): F[A] = byWeight(choices, random.nextInt(choices.map(_.weight).reduce))
    }
  }

  // required to run half the things in Free
  implicit def choicesApplicative[F[_]: Functor]: Functor[Choices[F, *]] =
    new Functor[Choices[F, *]] {

      def map[A, B](choices: Choices[F, A])(f: A => B): Choices[F, B] = choices.map[Choice[F, B]] {
        (choice: Choice[F, A]) => Choice.choiceFunctor[F].map[A, B](choice)(f): Choice[F, B]
      }
    }

  /** Allow ndfa <+> ndfa if F and A matches (concatenate inner Chains) */
  implicit def nonDeterministicMonoidK[F[_]: Applicative]: SemigroupK[NonDeterministic[F, *]] =
    new SemigroupK[NonDeterministic[F, *]] {

      def combineK[A](x: NonDeterministic[F, A], y: NonDeterministic[F, A]): NonDeterministic[F, A] =
        NonDeterministic[F, NonDeterministic[F, A]](x.toChoices ++ y.toChoices).flatMap(identity)
    }

  /** Needed for building and running computations */
  implicit class NonDeterministicOps[F[_], A](private val ndfa: NonDeterministic[F, A]) extends AnyVal {

    /** Returns list of possible choices by their weights */
    def toChoices(implicit F: Applicative[F]): Choices[F, NonDeterministic[F, A]] = ndfa.fold(
      (a:       A) => Choices.one[F, NonDeterministic[F, A]](F.pure(NonDeterministic.pure[F, A](a))),
      (choices: Choices[F, NonDeterministic[F, A]]) => choices
    )

    /** Scale up the weight of each Choice (useful if you want to combine NonDeterministic using <+>) */
    def scale(multiplier: Int)(implicit F: Applicative[F]): NonDeterministic[F, A] =
      NonDeterministic[F, NonDeterministic[F, A]](Choices.scale(multiplier)(toChoices)).flatMap(identity)

    /** Run computations picking branches using provided Decider */
    def execute(decider: Decider[F])(implicit M: Monad[F]): F[A] = ndfa.foldMap(decider)
  }
}
