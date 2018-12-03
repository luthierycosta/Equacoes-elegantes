// Vetores
let arrayX = [0.2]
let arrayY = [0.1]
let arrayZ = [0.1]

// Constantes
const a = 0.9259  , b1 = 0.15   , b2 = 1.5,
			g = 0.1     , rl = 0.002  , bp = 1,
			m1 = -1.4357, m0 = -0.7879, h = 0.02;

// Equação x
function x (n) {
	if (arrayX[n] !== undefined) {
		return arrayX[n]
	} else if (n === 1) {
		return arrayX[n] = x(2)
	} 
	else {
		return arrayX[n] = x(n-2) + h * a * (y(n-2) - x(n-2)) * Math.pow(b1,-1) - fun(x(n-2)) * Math.pow(b1,-1) 
	}
}

// Equação y
function y (n) {
	if (arrayY[n] !== undefined) {
		return arrayY[n]
	} else if (n === 1) {
		return arrayY[n] = y(2)
	}  else {
		return arrayY[n] = y(n-2) + h * a * (y(n-2) - x(n-2)) * Math.pow(b2,-1) + z(n-2) * Math.pow(b2,-1) 
	}
}

// Equação z
function z (n) {
	if (arrayZ[n] !== undefined) {
		return arrayZ[n]
	} else if (n === 1) {
		return arrayZ[n] = z(2)
	} else {
		return arrayZ[n] = z(n-2) + h * (y(n-2) - rl * z(n-2)) * Math.pow(g, -1) 
	}
}

// Função f(x)
const fun = num => m0 * num + 0.5 * (m1-m0) * (Math.abs(num+bp) - Math.abs(num-bp))

// Calculando o valor de Y
console.log(y(100))