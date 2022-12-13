import scala.annotation.tailrec

package EX2 {
	sealed trait Liste[+A] {
		def map[B](f: A => B): Liste[B] = this match {
			case Vide => Vide
			case NonVide(t, q) => NonVide(f(t), q.map(f))
		}

		// merge deux Listes
		def concat[B >: A](other: Liste[B]): Liste[B] = this match {
			case Vide => other
			case NonVide(t, q) => NonVide(t, q.concat(other))
		}

		def flatMap[B](f: A => Liste[B]): Liste[B] = this match {
			case Vide => Vide
			case NonVide(t, q) => f(t).concat(q.flatMap(f))
		}

		@tailrec
		final def fold[B >: A](default: B, f: (B, B) => B): B = this match {
			case Vide => default
			case NonVide(t, q) => q.fold(f(default, t), f)
		}
	}

	case class NonVide[+A](tete: A, queue: Liste[A]) extends Liste[A]
	object Vide extends Liste[Nothing]

	object Liste {
		def apply[A](first: A, others: A*): Liste[A] = (first, others.toList) match {
			case (Nil, _) => Vide
			case (f, Nil) => NonVide(f, Vide)
			case (f, o) => NonVide(f, apply(o.head, o.tail*))
		}
	}

	@main
	def Main: Unit = 
		println(Liste(1, 2, 3, 4, 5, 6) == NonVide(1, NonVide(2, NonVide(3, NonVide(4, NonVide(5, NonVide(6, Vide)))))))
		println(Liste(1) == NonVide(1, Vide))
		println(Liste(1, 2, 3) == NonVide(1, NonVide(2, NonVide(3, Vide))))

		println((Vide: Liste[Int]).map[Int](x => x + 1) ==  Vide)
		println(Liste(1, 2, 3, 4).map(x => x + 1) == Liste(2, 3, 4, 5))

		val l = Liste(1, 2, 3)
		println(l.concat(Liste(4, 5, 6)))

		println((Vide: Liste[Int]).flatMap[Int](x => Liste(x + 1)) ==  Vide)
		println(Liste(1, 2, 3, 4).flatMap(x => Liste(x + 1)) == Liste(2, 3, 4, 5))

		println((Vide: Liste[Int]).fold(0, (a, b) => a + b) == 0)
		println(Liste(1, 2, 3, 4).fold(0, (a, b) => a + b) == 10)
}
