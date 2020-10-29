import cats.{ ~>, Applicative, Functor, Monad, SemigroupK }
import cats.data.NonEmptyChain
import cats.free.Free
import cats.implicits._
import io.estatico.newtype.macros.newtype

import scala.annotation.tailrec
import scala.util.Random

package object nondeterministic {

  /** Represents a node with possible branches and their weights */
  @newtype final case class Choices[F[_], A](toNEC: NonEmptyChain[Choice[F, A]])

  object Choices {

    def one[F[_], A](fa: F[A]): Choices[F, A] = Choices(NonEmptyChain.one(Choice(fa)))

    type Scaler[F[_]] = Choices[F, *] ~> Choices[F, *]

    def scale[F[_]](multiplier: Int): Scaler[F] = new Scaler[F] {

      def apply[A](choices: Choices[F, A]): Choices[F, A] = Choices(choices.toNEC.map(_ * multiplier))
    }

    /** required to run half the things in Free */
    implicit def choicesApplicative[F[_] : Functor]: Functor[Choices[F, *]] =
      new Functor[Choices[F, *]] {

        def map[A, B](choices: Choices[F, A])(f: A => B): Choices[F, B] =
          Choices(choices.toNEC.map[Choice[F, B]] { (choice: Choice[F, A]) =>
            Choice.choiceFunctor[F].map[A, B](choice)(f): Choice[F, B]
          })
      }

    implicit def choicesSemigroupK[F[_]]: SemigroupK[Choices[F, *]] = new SemigroupK[Choices[F, *]] {

      def combineK[A](x: Choices[F, A], y: Choices[F, A]): Choices[F, A] = Choices(x.toNEC <+> y.toNEC)
    }
  }

  /** Represents a computation that might involve a non-deterministic choice */
  @newtype final case class NonDeterministic[F[_], A](toFree: Free[Choices[F, *], A]) {

    /** Returns list of possible choices by their weights */
    def toChoices(implicit F: Applicative[F]): Choices[F, NonDeterministic[F, A]] =
      toFree
        .fold(
          (a: A) => Choices.one(F.pure(Free.pure[Choices[F, *], A](a))),
          (choices: Choices[F, Free[Choices[F, *], A]]) => choices
        )
        .map(NonDeterministic(_))

    /** Scale up the weight of each Choice (useful if you want to combine NonDeterministic using <+>) */
    def scale(multiplier: Int)(implicit F: Applicative[F]): NonDeterministic[F, A] =
      NonDeterministic.suspend[F, NonDeterministic[F, A]](Choices.scale(multiplier)(toChoices)).flatten

    /** Run computations picking branches using provided Decider */
    def execute(decider: Decider[F])(implicit M: Monad[F]): F[A] = toFree.foldMap(decider)
  }

  object NonDeterministic {

    def suspend[F[_], A](choices: Choices[F, A]): NonDeterministic[F, A] = NonDeterministic(Free.liftF(choices))

    def lift[F[_], A](fa: F[A]): NonDeterministic[F, A] = NonDeterministic(Free.liftF(Choices.one(fa)))

    def pure[F[_], A](value: A): NonDeterministic[F, A] = NonDeterministic(Free.pure(value))

    /** Required since we wrapped up Free */
    @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
    implicit def nonDeterministicMonad[F[_]]: Monad[NonDeterministic[F, *]] =
      implicitly[Monad[Free[Choices[F, *], *]]].asInstanceOf[Monad[NonDeterministic[F, *]]] // Coercible failed me :(

    /** Allow ndfa <+> ndfa if F and A matches (concatenate inner Chains) */
    implicit def nonDeterministicMonoidK[F[_] : Applicative]: SemigroupK[NonDeterministic[F, *]] =
      new SemigroupK[NonDeterministic[F, *]] {

        def combineK[A](x: NonDeterministic[F, A], y: NonDeterministic[F, A]): NonDeterministic[F, A] =
          NonDeterministic.suspend[F, NonDeterministic[F, A]](x.toChoices <+> y.toChoices).flatMap(identity)
      }
  }

  /** Picks branch when we run the computation */
  type Decider[F[_]] = Choices[F, *] ~> F

  object Decider {

    def random[F[_]](random: Random = new Random()): Decider[F] = new Decider[F] {

      @tailrec
      private def byWeight[A](choices: NonEmptyChain[Choice[F, A]], weight: Int): F[A] =
        if (choices.head.weight >= weight) choices.head.fa
        else {
          NonEmptyChain.fromChain(choices.tail) match {
            case Some(tail) => byWeight[A](tail, weight - choices.head.weight)
            case None => choices.head.fa
          }
        }

      def apply[A](choices: Choices[F, A]): F[A] =
        byWeight(choices.toNEC, random.nextInt(choices.toNEC.map(_.weight).reduce))
    }
  }

}
