import scala.collection.mutable.Map
import scala.math._

class Equation(initial: Double, function: Int => Double) {
    val valsArray = Map(0 -> initial)

	def apply(n: Int): Double = {
		if (!valsArray.contains(n))
			valsArray(n) =
				if (n == 1) function(n+1)
				else function(n)
		return valsArray(n)
	}
}

object Main extends App {
	// Declarando constantes
	val a = 0.9259;	 val b1 = 0.15;		val b2 = 1.5;
	val g = 0.1;	 val rl = 0.002;	val bp = 1;
	val m1 = -1.4357;val m0 = -0.7879;	val h = 0.02;

	// Declarando o sistema
	// (É necessário explicitar o tipo da variável pra não dar erro de compilação)
	val x:Equation = new Equation(0.2, (n: Int) => x(n-2) + h * a * (y(n-2) - x(n-2)) * pow(b1,-1) - f(n-2) * pow(b1,-1))
	val y:Equation = new Equation(0.1, (n: Int) => y(n-2) + h * a * (y(n-2) - x(n-2)) * pow(b2,-1) + z(n-2) * pow(b2,-1))
	val z:Equation = new Equation(0.1, (n: Int) => z(n-2) + h * (y(n-2) - rl*z(n-2)) * pow(g, -1))

	def f(x:Int) = m0 * x + 0.5 * (m1-m0) * (abs(x+bp) - abs(x-bp))
	
	// O que a questão pede
	println("Y(100) = " + y(100))
}