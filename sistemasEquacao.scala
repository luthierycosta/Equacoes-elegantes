import scala.collection.mutable.Map
import scala.math._

abstract class Equation {
	val a = 0.9259;	 val b1 = 0.15;		val b2 = 1.5;
	val g = 0.1;	 val rl = 0.002;	val bp = 1;
	val m1 = -1.4357;val m0 = -0.7879;	val h = 0.02;

    val valsArray = Map.empty[Int,Double]
    def apply(n: Int): Double
}

object x extends Equation {
	def apply(n: Int): Double = {
		if (!valsArray.contains(n))
			valsArray += (n -> {
				if (n == 0) 0.2
				else if (n == 1) x(n+1)
				else x(n-2) + h * a * (y(n-2) - x(n-2)) * pow(b1,-1) - f(n-2) * pow(b1,-1)
			})
		return valsArray(n)
	}
}

object y extends Equation {
	def apply(n: Int): Double = {
		if (!valsArray.contains(n))
			valsArray += (n -> {
				if (n == 0) 0.1
				else if (n == 1) y(n+1)
				else y(n-2) + h * a * (y(n-2) - x(n-2)) * pow(b2,-1) + z(n-2) * pow(b2,-1)
			})
		return valsArray(n)
	}
}

object z extends Equation {
	def apply(n: Int): Double = {
		if (!valsArray.contains(n))
			valsArray += (n -> {
				if (n == 0) 0.1
				else if (n == 1) z(n+1)
				else z(n-2) + h * (y(n-2) - rl*z(n-2)) * pow(g, -1)
			})
		return valsArray(n)
	}
}

object f extends Equation {
	def apply(x: Int) = m0 * x + 0.5 * (m1-m0) * (abs(x+bp) - abs(x-bp))
}

object Main extends App {
	println("Y(100) = " + y(100))
}