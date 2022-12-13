package EX3 {
	sealed trait Article
	final case class Regular(name: String, category: String, price: Double) extends Article
	final case class Discounted(name: String, category: String, price: Double, discount: Double) extends Article

	def applyDiscount(article: Discounted): Double = article.price * (1 - article.discount)

	def price(article: Article): Double = article match {
		case Regular(_, _, price) => price
		case d: Discounted => applyDiscount(d)
	}

	// IMPLEM Question 3
	def cartAmount(articles: List[Article]): Double =
		articles.foldLeft(0.0){(acc, a) => acc + price(a)}

	def applyCoupon(coupon: Float, category: String): Article => Double = {
		a => a match {
			case Regular(_, c, _) if (c == category) => price(a) * (1 - coupon)
			case Discounted(_, c, _, _) if (c == category) => price(a) * (1 - coupon)
			case _ => price(a)
		}		
	}

	// IMPLEM Question 5
	def cartAmount2(articles: List[Article], coupon: Article => Double): Double = 
		articles.foldLeft(0.0){(acc, a) => acc + coupon(a)}

	@main
	def Main: Unit =
		//Question 3 => PRIX FINAL: 180.291
		val articles: List[Article] = List(
			Regular(name = "Biscuits", category = "food", price = 2.0),
			Discounted(name = "Monitor", category = "tech", price = 119.99, discount = 0.1),
			Discounted(name = "Mouse", category = "tech", price = 25.50, discount = 0.2),
			Regular(name = "dress", category = "clothes", price = 49.90)
		)

		println(cartAmount(articles))

		//QUESTION 6 => PRIX FINAL: 197.491
		val articles2: List[Article] = List(
			Regular(name = "Rice", category = "food", price = 10.0),
			Discounted(name = "Chocolate", category = "food", price = 8.0, discount = 0.1),
			Regular(name = "Biscuits", category = "food", price = 2.0),
			Discounted(name = "Monitor", category = "tech", price = 119.99, discount = 0.1),
			Discounted(name = "Mouse", category = "tech", price = 25.50, discount = 0.2),
			Regular(name = "dress", category = "clothes", price = 49.90)
		)
		println(cartAmount2(articles2, applyCoupon(0.5, "Food")))
}
