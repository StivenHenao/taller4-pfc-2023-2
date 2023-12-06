/**
  * Taller 3 - Programación Funcional
  * Autores: Alex García Castañeda - 2259517
 *           Sebastián Gómez Agudelo - 2259474
 *           Stiven Henao Aricapa - 2259603
  * Profesor: Carlos A Delgado
  */
package taller4
import org.scalameter.measure
import scala.collection.parallel.CollectionConverters._
import common._
import org.scalameter.withWarmer
import org.scalameter.Warmer

import scala.concurrent.duration.Duration
import scala.util.Random


object Taller4 {

  def saludo() = "Taller 4 2023-II"

  type Matriz = Vector[Vector[Int]]

  def matrizAlAzar(long: Int, vals: Int): Matriz = {
    val random = new Random()
    val v = Vector.fill(long, long)(random.nextInt(vals))
    v
  }

  def vectorAlAzar(long: Int, vals: Int): Vector[Int] = {
    val random = new Random()
    val v = Vector.fill(long)(random.nextInt(vals))
    v
  }

  def prodPunto(v1: Vector[Int], v2: Vector[Int]): Int = {
    (v1 zip v2).map({ case (i, j) => (i * j) }).sum
  }

  def transpuesta(m: Matriz): Matriz = {
    val l = m.length
    Vector.tabulate(l, l)((i, j) => m(j)(i))
  }

  def multMatriz(m1: Matriz, m2: Matriz): Matriz = {
    val size = m1.length

    Vector.tabulate(size, size) { (i, j) =>
      val fila = m1(i)
      val columna = transpuesta(m2)(j)
      prodPunto(fila, columna)
    }
  }

  def multMatrizPar(m1: Matriz, m2: Matriz): Matriz = {
    val size = m1.length

    Vector.tabulate(size, size) { (i, j) =>
      val (fila, columna) = parallel(m1(i), transpuesta(m2)(j))
      prodPunto(fila, columna)
    }
  }

  def subMatriz(m: Matriz, i: Int, j: Int, l: Int): Matriz = {
    Vector.tabulate(l, l) { (x, y) =>
      m(i + x)(j + y)
    }
  }

  def sumMatriz(m1: Matriz, m2: Matriz): Matriz = {
    val size = m1.length

    Vector.tabulate(size, size) { (i, j) =>
      m1(i)(j) + m2(i)(j)
    }
  }

  def multMatrizRec(m1: Matriz, m2: Matriz): Matriz = {
    val size = m1.length

    if (size <= 1) {
      Vector(Vector(m1(0)(0) * m2(0)(0)))
    } else {
      val half = size / 2

      val a11 = subMatriz(m1, 0, 0, half)
      val a12 = subMatriz(m1, 0, half, half)
      val a21 = subMatriz(m1, half, 0, half)
      val a22 = subMatriz(m1, half, half, half)

      val b11 = subMatriz(m2, 0, 0, half)
      val b12 = subMatriz(m2, 0, half, half)
      val b21 = subMatriz(m2, half, 0, half)
      val b22 = subMatriz(m2, half, half, half)

      val c11 = sumMatriz(multMatrizRec(a11, b11), multMatrizRec(a12, b21))
      val c12 = sumMatriz(multMatrizRec(a11, b12), multMatrizRec(a12, b22))
      val c21 = sumMatriz(multMatrizRec(a21, b11), multMatrizRec(a22, b21))
      val c22 = sumMatriz(multMatrizRec(a21, b12), multMatrizRec(a22, b22))


      val result = Vector.tabulate(size, size) { (i, j) =>
        if (i < half) {
          if (j < half) c11(i)(j)
          else c12(i)(j - half)
        } else {
          if (j < half) c21(i - half)(j)
          else c22(i - half)(j - half)
        }
      }
      result
    }
  }

  def multMatrizRecPar(m1: Matriz, m2: Matriz): Matriz = {
    val size = m1.length

    if (size <= 1) {
      Vector(Vector(m1(0)(0) * m2(0)(0)))
    } else {
      val half = size / 2

      val (a11, a12, a21, a22) = parallel(subMatriz(m1, 0, 0, half), subMatriz(m1, 0, half, half), subMatriz(m1, half, 0, half), subMatriz(m1, half, half, half))
      val (b11, b12, b21, b22) = parallel(subMatriz(m2, 0, 0, half), subMatriz(m2, 0, half, half), subMatriz(m2, half, 0, half), subMatriz(m2, half, half, half))
      val (c11, c12, c21, c22) = parallel(sumMatriz(multMatrizRec(a11, b11), multMatrizRec(a12, b21)), sumMatriz(multMatrizRec(a11, b12), multMatrizRec(a12, b22)), sumMatriz(multMatrizRec(a21, b11), multMatrizRec(a22, b21)), sumMatriz(multMatrizRec(a21, b12), multMatrizRec(a22, b22)))


      val result = Vector.tabulate(size, size) { (i, j) =>
        if (i < half) {
          if (j < half) c11(i)(j)
          else c12(i)(j - half)
        } else {
          if (j < half) c21(i - half)(j)
          else c22(i - half)(j - half)
        }
      }
      result
    }
  }

    def restaMatriz(m1: Matriz, m2: Matriz): Matriz = {
      Vector.tabulate(m1.length, m1.length) { (i, j) =>
        m1(i)(j) - m2(i)(j)
      }
    }


    def multStrassen(m1: Matriz, m2: Matriz): Matriz = {
      val size = m1.length
      if (size <= 1) {
        Vector(Vector(m1(0)(0) * m2(0)(0)))
      } else {
        val half = size / 2

        val a11 = subMatriz(m1, 0, 0, half)
        val a12 = subMatriz(m1, 0, half, half)
        val a21 = subMatriz(m1, half, 0, half)
        val a22 = subMatriz(m1, half, half, half)

        val b11 = subMatriz(m2, 0, 0, half)
        val b12 = subMatriz(m2, 0, half, half)
        val b21 = subMatriz(m2, half, 0, half)
        val b22 = subMatriz(m2, half, half, half)

        val p1 = multStrassen(sumMatriz(a11, a22), sumMatriz(b11, b22))
        val p2 = multStrassen(sumMatriz(a21, a22), b11)
        val p3 = multStrassen(a11, restaMatriz(b12, b22))
        val p4 = multStrassen(a22, restaMatriz(b21, b11))
        val p5 = multStrassen(sumMatriz(a11, a12), b22)
        val p6 = multStrassen(restaMatriz(a21, a11), sumMatriz(b11, b12))
        val p7 = multStrassen(restaMatriz(a12, a22), sumMatriz(b21, b22))

        val c11 = sumMatriz(restaMatriz(sumMatriz(p1, p4), p5), p7)
        val c12 = sumMatriz(p3, p5)
        val c21 = sumMatriz(p2, p4)
        val c22 = sumMatriz(sumMatriz(restaMatriz(p1, p2), p3), p6)

        val result = Vector.tabulate(size, size) { (i, j) =>
          if (i < half) {
            if (j < half) c11(i)(j)
            else c12(i)(j - half)
          } else {
            if (j < half) c21(i - half)(j)
            else c22(i - half)(j - half)
          }
        }
        result
      }
    }
  def multStrassenPar(m1: Matriz, m2: Matriz): Matriz = {
    val size = m1.length
    if (size <= 1) {
      Vector(Vector(m1(0)(0) * m2(0)(0)))
    } else {
      val half = size / 2

      val a11 = task {subMatriz(m1, 0, 0, half)}
      val a12 = task {subMatriz(m1, 0, half, half)}
      val a21 = task {subMatriz(m1, half, 0, half)}
      val a22 = task{subMatriz(m1, half, half, half)}

      val b11 = task {subMatriz(m2, 0, 0, half)}
      val b12 = task {subMatriz(m2, 0, half, half)}
      val b21 = task {subMatriz(m2, half, 0, half)}
      val b22 = task {subMatriz(m2, half, half, half)}

      val p1 = task {multStrassen(sumMatriz(a11.join, a22.join), sumMatriz(b11.join, b22.join))}
      val p2 = task {multStrassen(sumMatriz(a21.join, a22.join), b11.join)}
      val p3 = task {multStrassen(a11.join, restaMatriz(b12.join, b22.join))}
      val p4 = task {multStrassen(a22.join, restaMatriz(b21.join, b11.join))}
      val p5 = task {multStrassen(sumMatriz(a11.join, a12.join), b22.join)}
      val p6 = task {multStrassen(restaMatriz(a21.join, a11.join), sumMatriz(b11.join, b12.join))}
      val p7 = task {multStrassen(restaMatriz(a12.join, a22.join), sumMatriz(b21.join, b22.join))}

      val c11 = sumMatriz(restaMatriz(sumMatriz(p1.join, p4.join), p5.join), p7.join)
      val c12 = sumMatriz(p3.join, p5.join)
      val c21 = sumMatriz(p2.join, p4.join)
      val c22 = sumMatriz(sumMatriz(restaMatriz(p1.join, p2.join), p3.join), p6.join)

      val result = Vector.tabulate(size, size) { (i, j) =>
        if (i < half) {
          if (j < half) c11(i)(j)
          else c12(i)(j - half)
        } else {
          if (j < half) c21(i - half)(j)
          else c22(i - half)(j - half)
        }
      }
      result
    }
  }

  def prodPuntoParD(v1: Vector[Int], v2: Vector[Int]): Int = {
    val resultado = v1.par.zip(v2.par).map { case (x, y) => x * y }.sum

    resultado
  }

    def compararAlgoritmos(funcion1: (Matriz, Matriz) => Matriz, funcion2: (Matriz, Matriz) => Matriz)
                          (matriz1: Matriz, matriz2: Matriz): (Double, Double, Double) = {

      val tiempoFuncion1 = withWarmer(new Warmer.Default) measure {
        funcion1(matriz1, matriz2)
      }

      val tiempoFuncion2 = withWarmer(new Warmer.Default) measure {
        funcion2(matriz1, matriz2)
      }

      val tiempo1: Double = tiempoFuncion1.value
      val tiempo2: Double = tiempoFuncion2.value

      val aceleracion = tiempo1 / tiempo2

      (tiempo1, tiempo2, aceleracion)
    }

  def compararProdPunto(tamaño: Int): (Double, Double, Double) = {
    val v1 = vectorAlAzar(tamaño, 100)
    val v2 = vectorAlAzar(tamaño, 100)

    val tiempoSecuencial = withWarmer(new Warmer.Default) measure {
      prodPunto(v1, v2)
    }

    val tiempoParalelo = withWarmer(new Warmer.Default) measure {
      prodPuntoParD(v1, v2)
    }

    val aceleracion = tiempoSecuencial.value / tiempoParalelo.value

    (tiempoSecuencial.value, tiempoParalelo.value, aceleracion)
  }

    def main(args: Array[String]): Unit = {
      println(saludo())
      /*println("Comparacion de rendimiento entre multMatriz y multMatrizPar")
      val resultados1 = for {
        i <- 1 to 7
        m1 = matrizAlAzar(math.pow(2, i).toInt, 2)
        m2 = matrizAlAzar(math.pow(2, i).toInt, 2)
      } yield (compararAlgoritmos(multMatriz, multMatrizPar)(m1, m2), "Tamano: " + math.pow(2,i))

      resultados1.foreach(println)

      println("Comparacion de rendimiento entre multMatrizRec y multMatrizRecPar")
      val resultados2 = for {
        i <- 1 to 7
        m1 = matrizAlAzar(math.pow(2, i).toInt, 2)
        m2 = matrizAlAzar(math.pow(2, i).toInt, 2)
      } yield (compararAlgoritmos(multMatrizRec, multMatrizRecPar)(m1, m2), "Tamano: " + math.pow(2, i))

      resultados2.foreach(println)

      println("Comparacion de rendimiento entre multStrassen y multStrassenPar")
      val resultados3 = for {
        i <- 1 to 7
        m1 = matrizAlAzar(math.pow(2, i).toInt, 2)
        m2 = matrizAlAzar(math.pow(2, i).toInt, 2)
      } yield (compararAlgoritmos(multStrassen, multStrassenPar)(m1, m2), "Tamano: " + math.pow(2, i))

      resultados3.foreach(println)

      println("Comparación de rendimiento entre prodPunto y prodPuntoParD")
      val resultados4 = for {
        i <- 1 to 20
        tamano = math.pow(2,i+5).toInt
      } yield (compararProdPunto(tamano), s"Tamaño: $tamano")

      resultados4.foreach(println)
*/
      /*println(saludo())

      println(
        withWarmer(new Warmer.Default) measure {
          (1 to 100000000).toArray
        }
      )
      val m1 = matrizAlAzar(5, 2)
      val m2 = matrizAlAzar(5, 2)
      println(compararAlgoritmos(multMatriz, multMatrizPar)(m1, m2))

      val matriz1: Matriz = Vector(
        Vector(1, 2),
        Vector(3, 4)
      )

      val matriz2: Matriz = Vector(
        Vector(5, 6),
        Vector(7, 8)
      )
      println(multMatriz(matriz1, matriz2))
      println(multMatrizPar(matriz1, matriz2))
      println(compararAlgoritmos(multMatriz, multMatrizPar)(matriz1, matriz2))

      println(sumMatriz(matriz1, matriz2))
      println(multMatrizRec(matriz1, matriz2))
      println(multMatrizRecPar(matriz1, matriz2))
      println(restaMatriz(matriz1, matriz2))
      println(multStrassen(matriz1, matriz2))
      println(subMatriz(matriz1, 0, 0, matriz1.size / 2))

      val m4: Matriz = Vector(
        Vector(1, 2, 3, 4),
        Vector(5, 6, 7, 8),
        Vector(9, 1, 2, 3),
        Vector(4, 5, 6, 7)
      )
      val m5: Matriz = Vector(
        Vector(4, 3, 2, 1),
        Vector(8, 7, 6, 5),
        Vector(3, 2, 1, 9),
        Vector(7, 6, 5, 4)
      )
      val m6 = matrizAlAzar(8, 8)
      println(m6)
      println(subMatriz(m6, 0, 0, m6.size / 2))
      println(multMatrizRec(m4, m5))
      println(multMatrizRecPar(m4, m5))

      println("Multstrassen:")
      println(multStrassen(m4, m5))
      println(multStrassenPar(m4,m5))
*/

    }
  }
