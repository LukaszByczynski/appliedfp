package appliedfp

import scalaz._
import Scalaz._

import scala.language.higherKinds

class Main {
}

object higher_order {
  type Parser[E, A] = String => E \/ (String, A)

  def alt[E, A](l: Parser[E, A], r: Parser[E, A]): Parser[E, A] = {
    s => l(s) orElse r(s)
  }
}

object polymorphic {
  def square(x: Int): Int = x * x

  // How to fake parametric polymorphism in Scala (since we don't have polymorphic functions, only methods)
  object identity {
    // [A] => ((a: A) => A)
    // We take a type A and return a function that takes a value of type A and returns a value of type A
    def apply[A](a: A): A = a
  }

  identity(1) // identity.apply[Int](1)
  identity("foo") // identity.apply[String]("foo")

  // second((1, 2)) == 2
  // second((true, "false")) == "false"
  object second {
    def apply[A](tuple: (_, A)): A = tuple._2 // Note: You usually don't want to use _ for types
  }
}

object types {
  object products {
    type Point = (Int, Int) // unnamed terms (just indexed access)

    final case class Person(name: String, age: Int) // named terms

    // The following are isomorphic, which means you can go between one and the other without losing information.
    // This is because the only value available for Unit is an instance of unit.
    final case class A(foo: Int, unit: Unit)
    final case class B(foo: Int)

    val to: A => B = (a: A) => B(a.foo)
    val from: B => A = (b: B) => A(b.foo, ())

    val isomorphicIdentity: A => A = to andThen from // This is equivalent to identity
  }

  object sums {
    sealed trait Currency
    case object USD extends Currency
    case object EUR extends Currency
    case object ILS extends Currency

    // True sum types will always have the type of the sum, not of any of the terms. Scala does not have true sum types.
  }

  object zero {
    // This is an empty domain of values
    final abstract class Zero {
      def absurd[A]: A
    }

    // Can't create an instance of this
    case class Person(name: String, zero: Zero)

    // But we can create an instance of this:
    type Yup = Either[String, Zero]

    // This is a type-safe way of returning the left hand side since the right hand side can never exist
    def simplifyL[A](e: Either[A, Zero]): A = {
      e.fold(identity, _.absurd[A])
    }

    // Zero is usually called Void
    // List[Void] is an empty list (since no actual values can exist in it)
    // Future[Void] will never complete (since it can't create a value)
  }

  object kinds {
//    sealed trait List[+A]
//    case class Cons[+A](head: A, tail: List[A]) extends List[A]
//    case object Nil extends List[Nothing]
//    val myList: List[Int] = ???
//    val f: Int => Int = (v: Int) => v + 1
//    f(1)
//    case class Person(name: String, age: Int)

    // Type    Kind
    // Int     *
    // Option  * => *
    // Either  [*, *] => *

    // Scala does not have a kind annotation that looks like this: def foo[F: * => *, A: *](a: A).
    // All types default to kind of * (star), so we use this:
    def foo[F[_], A](fa: F[A]): F[A] = ???
    def bar[F[_, _], A, B](fa: F[A, B]): F[A, B] = ???

    trait StackModule[F[_]] {
      def empty[A]: F[A]
      def push[A](a: A, as: F[A]): F[A]
      def pop[A](as: F[A]): Option[(A, F[A])]
    }

    val listStack: StackModule[List] = new StackModule[List] {
      override def empty[A]: List[A] = Nil
      override def push[A](a: A, as: List[A]): List[A] = a :: as
      override def pop[A](as: List[A]): Option[(A, List[A])] = as.headOption.map((_, as.tail))
    }

    trait NaturalTransformation[F[_], G[_]]
    // Kind => [* => *, * => *] => *

    trait Example[F[_[_, _], _], G[_[_[_]]], H]
    // Kind: [[[*, *] => *, *] => *, [[* => *] => *] => *, *] => *
  }

  object partial_application {
    trait Sized[F[_]] {
      def size[A](fa: F[A]): Int
    }

    val SizedList: Sized[List] = new Sized[List] {
      override def size[A](fa: List[A]): Int = fa.length
    }

    SizedList.size(1 :: 2 :: 3 :: Nil)

    // Partial application of the Map type, so that its kind turns from [*, *] => * into * => *
    def SizedMap[K]: Sized[Map[K, ?]] = new Sized[Map[K, ?]] {
      override def size[A](fa: Map[K, A]): Int = fa.size
    }

    def SizedTuple2[A]: Sized[(A, ?)] = new Sized[(A, ?)] {
      // We are looking for the number of B's in the tuple (A, B). It's always 1, since we already taken away the A with
      // the partial application.
      override def size[B](fa: (A, B)): Int = 1
    }
  }

  object type_classes {
    // Monomorphic version
    def repeat(n: Int, s: String): String =
      if (n <= 0) "" else s + repeat(n - 1, s)

    // Polymorphic version
    def repeat2[A](n: Int, a: A, empty: A, combine: (A, A) => A): A =
      if (n <= 0) empty else combine(a, repeat2(n - 1, a, empty, combine))

    // Instance of polymorphic function for String
    val repeatString: (Int, String) => String = repeat2[String](_, _, "", _ + _)

    // That's ugly. With type classes tho:
    object algebra {
      trait Semigroup[A] {
        // Associative Law:
        // append(a, append(b, c)) === append(append(a, b), c)
        def append(l: A, r: A): A
      }

      trait Monoid[A] extends Semigroup[A] {
        // Left Identity Law: append(zero, a) === a
        // Right Identity Law: append(a, zero) === a
        def zero: A
      }

      implicit val monoidOfString: Monoid[String] = new Monoid[String] {
        override val zero: String = ""

        override def append(l: String, r: String): String = l + r
      }

      implicit val monoidOfInt: Monoid[Int] = new Monoid[Int] {
        override def zero: Int = 0

        override def append(l: Int, r: Int): Int = l + r
      }

      implicit def monoidOfList[A]: Monoid[List[A]] = new Monoid[List[A]] {
        override def zero: List[A] = Nil

        override def append(l: List[A], r: List[A]): List[A] = l ++ r
      }

      implicit def monoidOfOption[A: Semigroup]: Monoid[Option[A]] = new Monoid[Option[A]] {
        override def zero: Option[A] = None

        override def append(l: Option[A], r: Option[A]): Option[A] = {
          val appended = for {
            lv <- l
            rv <- r
          } yield lv |+| rv

          appended orElse zero
        }
      }

      implicit def monoidOfMap[K, V: Semigroup]: Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
        override def zero: Map[K, V] = Map.empty

        override def append(l: Map[K, V], r: Map[K, V]): Map[K, V] = {
          (l.keySet ++ r.keySet)
            .map(k => k -> (l.get(k) |+| r.get(k)))
            .toMap
            .collect { case (k, Some(v)) => k -> v }
        }
      }
    }

    import algebra._

    def repeat3[A](n: Int, a: A, m: Monoid[A]): A =
      if (n <= 0) m.zero else m.append(a, repeat3(n - 1, a, m))

    // Or better yet
    def repeat4[A](n: Int, a: A)(implicit m: Monoid[A]): A =
      if (n <= 0) m.zero else m.append(a, repeat4(n - 1, a))

    repeat4(4, "a")

    // But we like the object oriented syntax

    implicit class SemigroupSyntax[A](l: A) {
      // We place the implicit on the method level because then we would get a nicer error message if A does not have
      // a type class of Monoid defined for it.
      def |+|(r: A)(implicit m: Semigroup[A]): A = m.append(l, r)
    }

    def zero[A](implicit m: Monoid[A]): A = m.zero

    // And then

    def repeat5[A: Monoid](n: Int, a: A): A =
      if (n <= 0) zero[A] else a |+| repeat5(n - 1, a)
  }

  object ct {
    // Imagine F is a program you're building. Here you'll build the continuation of the program from an existing one
    trait Functor[F[_]] {
      // Identity Law:
      //   fmap(fa, identity) === fa
      // Composition Law:
      //   fmap(fmap(fa, g), f) === fmap(fa, f compose g)
      def fmap[A, B](fa: F[A], f: A => B): F[B]
    }

    object Functor {
      def apply[F[_]](implicit ff: Functor[F]): Functor[F] = ff

      implicit val ListFunctor: Functor[List] = new Functor[List] {
        override def fmap[A, B](fa: List[A], f: A => B): List[B] = fa map f
      }

      implicit val OptionFunctor: Functor[Option] = new Functor[Option] {
        override def fmap[A, B](fa: Option[A], f: A => B): Option[B] = fa map f
      }

      implicit def F1Functor[K]: Functor[K => ?] = new Functor[K => ?] {
        override def fmap[A, B](fa: K => A, f: A => B): K => B = fa andThen f
      }
    }

    implicit class FunctorSyntax[F[_], A](fa: F[A]) {
      def map[B](f: A => B)(implicit ff: Functor[F]): F[B] = ff.fmap(fa, f)
    }

    trait Contravariant[F[_]] {
      def contramap[A, B](fa: F[A], f: B => A): F[B]
    }

    object Contravariant {
      // The definition of T1 => R is Function1[-T1, +R]. This is a good hint that the functor is covariant on R and
      // contravariant on T1. Also see F1Functor which is the covariant functor on Function1.
      implicit def ContravariantFunction[K]: Contravariant[? => K] = new Contravariant[? => K] {
        override def contramap[A, B](fa: A => K, f: B => A): B => K = f andThen fa
      }
    }

    trait Invariant[F[_]] {
      // This signature requires isomorphism between A and B (can go from a to b and back to a without losing
      // information). This is useful for both A and B being both input and output (in the covariant and contravariant
      // positions).
      def xmap[A, B](fa: F[A], ab: A => B, ba: B => A): F[B]
    }

    object Invariant {
      type Json = String

      trait JsonCodec[A] {
        def encode(a: A): Json
        def decode(j: Json): A
      }

      implicit val codecInvariantFunctor: Invariant[JsonCodec] = new Invariant[JsonCodec] {
        override def xmap[A, B](fa: JsonCodec[A], ab: A => B, ba: B => A): JsonCodec[B] = new JsonCodec[B] {
          override def encode(b: B): Json = fa.encode(ba(b))
          override def decode(json: Json): B = ab(fa.decode(json))
        }
      }
    }

    trait Apply[F[_]] extends Functor[F] {
      // Apply is used to combine functors
      def ap[A, B](ff: F[A => B], fa: F[A]): F[B]

      def zip[C, D](fa: F[C], fb: F[D]): F[(C, D)] = {
        val value: F[D => (C, D)] = fmap[C, D => (C, D)](fa, (a: C) => (b: D) => (a, b))
        ap[D, (C, D)](value, fb)
      }
    }

    object Apply {
      implicit val OptionApply: Apply[Option] = new Apply[Option] {
        override def ap[A, B](ff: Option[A => B], fa: Option[A]): Option[B] =
          for {
            f <- ff
            a <- fa
          } yield f(a)

        override def fmap[A, B](fa: Option[A], f: A => B): Option[B] =
          Functor.OptionFunctor.fmap(fa, f)
      }

      implicit val ListApply: Apply[List] = new Apply[List] {
        // There is an alternative implementations, like zipping the two lists and applying the function
        // Also, Apply provides no guarantee of ordering
        override def ap[A, B](ff: List[A => B], fa: List[A]): List[B] =
          for {
            f <- ff
            a <- fa
          } yield f(a)

        override def fmap[A, B](fa: List[A], f: A => B): List[B] =
          Functor.ListFunctor.fmap(fa, f)
      }

      type Parser[E, A] = String => Either[E, (String, A)]

      implicit def ParserApply[E]: Apply[Parser[E, ?]] = new Apply[Parser[E, ?]] {
        override def ap[A, B](ff: Parser[E, A => B], fa: Parser[E, A]): Parser[E, B] = {
          s: String => {
            ff(s) match {
              case Left(e) => Left(e)
              case Right((afterF, f)) =>
                fa(afterF) match {
                  case Left(e) => Left(e)
                  case Right((afterA, a)) => Right((afterA, f(a)))
                }
            }
          }
        }

        override def fmap[A, B](fa: Parser[E, A], f: A => B): Parser[E, B] = {
          s => fa(s) match {
            case Left(e) => Left(e)
            case Right((str, a)) => Right(str, f(a))
          }
        }
      }
    }

    trait Applicative[F[_]] extends Apply[F] {
      def point[A](a: A): F[A]
    }

    object Applicative {
      type Parser[E, A] = String => Either[E, (String, A)]

      implicit def ParserApplicative[E]: Applicative[Parser[E, ?]] = new Applicative[Parser[E, ?]] {
        override def point[A](a: A): Parser[E, A] = {
          s => Right(s, a)
        }

        override def ap[A, B](ff: Parser[E, A => B], fa: Parser[E, A]): Parser[E, B] =
          Apply.ParserApply[E].ap(ff, fa)

        override def fmap[A, B](fa: Parser[E, A], f: A => B): Parser[E, B] =
          Apply.ParserApply[E].fmap(fa, f)
      }
    }

    trait Monad[F[_]] extends Applicative[F] {
      // This is very powerful because we're using a value to determine how our program in the effect continues to build
      // which marks the path as sequential

      def bind[A, B](fa: F[A], afb: A => F[B]): F[B] // aka flatMap
    }

    object Monad {
      def apply[F[_]](implicit m: Monad[F]): Monad[F] = m

      implicit val MonadOption: Monad[Option] = new Monad[Option] {
        override def bind[A, B](fa: Option[A], afb: A => Option[B]): Option[B] = {
          // fa flatMap afb
          fa match {
            case Some(a) => afb(a)
            case None => None
          }
        }

        override def point[A](a: A): Option[A] = None

        override def ap[A, B](ff: Option[A => B], fa: Option[A]): Option[B] = Apply.OptionApply.ap(ff, fa)

        override def fmap[A, B](fa: Option[A], f: A => B): Option[B] = Apply.OptionApply.fmap(fa, f)
      }

      type Parser[E, A] = String => Either[E, (String, A)]

      implicit def MonadParser[E]: Monad[Parser[E, ?]] = new Monad[Parser[E, ?]] {
        override def bind[A, B](fa: Parser[E, A], afb: A => Parser[E, B]): Parser[E, B] = {
          s => fa(s) match {
            case Left(e) => Left(e)
            case Right((str, a)) => afb(a)(s)
          }
        }

        override def point[A](a: A): Parser[E, A] = Applicative.ParserApplicative.point(a)

        override def ap[A, B](ff: Parser[E, A => B], fa: Parser[E, A]): Parser[E, B] =
          Applicative.ParserApplicative.ap(ff, fa)

        override def fmap[A, B](fa: Parser[E, A], f: A => B): Parser[E, B] =
          Applicative.ParserApplicative.fmap(fa, f)
      }

      implicit class MonadSyntax[F[_], A](fa: F[A]) extends FunctorSyntax(fa) {
        def map[B](f: A => B)(implicit m: Monad[F]): F[B] = m.fmap(fa, f)
        def flatMap[B](f: A => F[B])(implicit m: Monad[F]): F[B] = m.bind(fa, f)
        def zip[B](fb: F[B])(implicit m: Monad[F]): F[(A, B)] = m.zip(fa, fb)
      }
    }
  }

  object effects {
    import appliedfp.types.zero.Zero
    import ct.Monad

    final case class IO[E, A](unsafePerformIO: () => Either[E, A]) { self =>
      def map[B](f: A => B): IO[E, B] =
        IO[E, B](() => self.unsafePerformIO().map(f))

      def flatMap[B](f: A => IO[E, B]): IO[E, B] =
        IO[E, B] { () =>
          self.unsafePerformIO() match {
            case Left(e) => Left(e)
            case Right(a) => f(a).unsafePerformIO()
          }
        }

      // Note: Attempt is not executing the code, only exposing the error to be flatMapped over
      def attempt: IO[Zero, Either[E, A]] = {
        IO(() => Right(self.unsafePerformIO()))
      }
    }

    object IO {
      def point[E, A](a: A): IO[E, A] = IO(() => Right(a))
      def fail[E, A](e: E): IO[E, A] = IO(() => Left(e))

      implicit def MonadIO[E]: Monad[IO[E, ?]] = new Monad[IO[E, ?]] {
        override def point[A](a: A): IO[E, A] =
          IO.point(a)

        override def bind[A, B](fa: IO[E, A], f: A => IO[E, B]): IO[E, B] =
          fa.flatMap(f)

        override def ap[A, B](ff: IO[E, A => B], fa: IO[E, A]): IO[E, B] =
          for {
            f <- ff
            a <- fa
          } yield f(a)

        override def fmap[A, B](fa: IO[E, A], f: A => B): IO[E, B] =
          fa.map(f)
      }
    }
  }

  object console {
    import effects._

    def println(line: String): IO[Void, Unit] = IO(() => Right(println(line)))
    val readLine: IO[Void, String] = IO(() => Right(scala.io.StdIn.readLine())) // Note that this can be val
  }

  object app {
    import effects._
    import ct.Monad._

    sealed trait Selection
    case object PlayGame extends Selection
    case object Exit extends Selection

    def printMenu: IO[Void, Unit] = {
      console.println("Press 1 to start the game, press 2 to exit")
    }

    def parseInput(input: String): Option[Selection] = {
      input.trim.toLowerCase match {
        case "1" => Some(PlayGame)
        case "2" => Some(Exit)
        case _ => None
      }
    }

    def invalidChoice: IO[Void, Unit] = {
      console.println("Your choice was invalid")
    }

    final case class GameState(guessed: Set[Char], word: String)

    def chooseWord: IO[Void, String] =
      IO { () =>
        val list = "monad" :: "functor" :: "israel" :: Nil

        Right(list(scala.util.Random.nextInt(list.length)))
      }

    def printGame(state: GameState): IO[Void, Unit] = {
      val word = state.word.toList
        .map {
          case c if state.guessed.contains(c) => c
          case _ => "_"
        }
        .mkString

      for {
        _ <- console.println("Word: " + word)
        _ <- console.println("Already guessed: " + state.guessed.mkString(", "))
      } yield ()
    }

    def parseLetter(input: String): Option[Char] = {
      input.trim.toLowerCase.toList match {
        case c :: Nil if c.isLetter => Some(c)
        case _ => None
      }
    }

    def updateState(state: GameState, c: Char, guessesAllowed: Int): IO[Void, Option[GameState]] = {
      val newState = state.copy(guessed = state.guessed + c)

      if ((newState.word.toSet -- newState.guessed).isEmpty) {
        for {
          _ <- printGame(newState)
          _ <- console.println((guessesAllowed - newState.guessed.size) + " guesses left")
          _ <- console.println("Way to go! You guessed right!")
        } yield None
      } else if (newState.guessed.size > guessesAllowed) {
        for {
          _ <- printGame(newState)
          _ <- console.println("Sorry, you lost.")
        } yield None
      } else {
        for {
          _ <- printGame(newState)
          _ <- console.println((guessesAllowed - newState.guessed.size) + " guesses left")
        } yield Option(newState)
      }
    }

    def playGameLoop(state: GameState, guessesAllowed: Int): IO[Void, Unit] = {
      for {
        _ <- printGame(state)
        input <- console.readLine
        o <- parseLetter(input) match {
          case None => console.println("Please enter just one letter").map(_ => Option(state))
          case Some(c) => updateState(state, c, guessesAllowed)
        }
        _ <- o.fold(console.println("Good game!"))((state: GameState) => playGameLoop(state, guessesAllowed))
      } yield ()
    }

    def playGame(guessesAllowed: Int): IO[Void, Unit] =
      chooseWord.flatMap(word => playGameLoop(GameState(Set.empty, word), guessesAllowed))

    def mainLoop: IO[Void, Unit] = {
      val executeOnSelection: Selection => IO[Void, Unit] = {
        case Exit => console.println("Goodbye!")
        case PlayGame => playGame(6).flatMap(_ => mainLoop)
      }

      for {
        _      <- printMenu
        choice <- console.readLine
        _      <- parseInput(choice).fold(invalidChoice)(executeOnSelection)
      } yield ()
    }

    def main(args: Array[String]) {
      val program: IO[Void, Unit] = for {
        _ <- console.println("Hello! Welcome to the game!")
        _ <- mainLoop
      } yield ()

      program.unsafePerformIO()
    }
  }
}