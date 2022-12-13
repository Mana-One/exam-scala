import scala.annotation.tailrec

package EX1 {
	// Question 1
	@tailrec
	def egaux(a: List[Int], b: List[Int]): Boolean = (a, b) match {
		case (Nil, Nil) => true
		case (Nil, _) => false
		case (_, Nil) => false
		case (ha::ta, hb::tb) if (ha != hb) => false
		case (ha::ta, hb::tb) => egaux(ta, tb)
	}

	// Question 2
	@tailrec
	def min(a: List[Int]): Int = a match {
		case Nil => throw IllegalArgumentException()
		case (x::Nil) => x
		case (x::y::tail) if (x <= y) => min(x::tail) 
		case (x::y::tail) => min(y::tail)
	}

	// Question 3
	def isPrime(a: Int): Boolean = innerIsPrime(a, 2)

	@tailrec
	def innerIsPrime(value: Int, current: Int): Boolean = current match {
		case c if (c == value) => innerIsPrime(value, c + 1)
		case c if (c > (value / 2 + 1)) => true
		case c if (value % c == 0) => false
		case c => innerIsPrime(value, c + 1)
	}

	@main
	def Main: Unit = 
		println(egaux(1 :: 2 :: 3 :: Nil, 1 :: 2 :: 3 :: Nil))
		println(egaux(1 :: 2 :: 3 :: Nil, 1 :: 2 :: 4 :: Nil))
		println(egaux(1 :: 2 :: 3 :: Nil, 1 :: 2 :: 3 :: 4 :: Nil))

		println(min(9 :: 2 :: 3 :: Nil))

		println(isPrime(7))
		println(isPrime(9))
}