package nondeterministic

import cats.Functor
import cats.syntax.functor._

/** Represents a pairing: possible branch - its weight */
final case class Choice[F[_], A](fa: F[A], weight: Int = 1) {
  assert(weight > 0, "Weight must be positive")
  def *(multiplier: Int): Choice[F, A] = copy(weight = weight * multiplier) // scalastyle:ignore method.name
}
object Choice {

  implicit def choiceFunctor[F[_]: Functor]: Functor[Choice[F, *]] =
    new Functor[Choice[F, *]] {

      def map[A, B](choice: Choice[F, A])(f: A => B): Choice[F, B] =
        choice.copy(fa = choice.fa.map(f))
    }
}
