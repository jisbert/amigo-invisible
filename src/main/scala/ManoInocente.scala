import scala.annotation.tailrec
import scala.util.Random
import Persona._

object ManoInocente {
  def main(args: Array[String]): Unit = {
    val personas = procesaNombres(args)
    val eligen = personas.toList.sorted(ordenarPorEmparejado).reverse
    val elegidas = Random.shuffle(personas.toList)
    val amigas = emparejaAmigas(eligen, elegidas, Map[String, String]())
    println(amigas.mkString("(", ",", ")"))
  }

  /** Procesa nombres y devuelve un conjunto no ordenado de personas únicas.
    *
    * Por cada nombre genera, en función del patrón que adopta:
    *
    *  a. Si responde al patrón {{{([\p{L} ]*) y ([\p{L} ]*)}}} un conjunto de dos personas de forma que la pareja de una es la pareja de la otra y viceversa.
    *  b. Un conjunto de una persona que no tiene pareja ([[scala.Some]]).
    *
    * @example
    * {{{
    * scala> ManoInocente.procesaNombres(Array("a", "b c y d", "e y f g", "h"))
    * res0: scala.collection.immutable.Set[Persona] = Set(Persona(e,Some(f g)), Persona(f g,Some(e)), Persona(h,None), Persona(b c,Some(d)), Persona(a,None), Persona(d,Some(b c)))
    * }}}
    *
    * @param nombres conjunto de nombres.
    * @return conjunto no ordenado de personas únicas.
    */
  def procesaNombres(nombres: Array[String]) = {
    val patrón = """([\p{L} ]*) y ([\p{L} ]*)""".r
    val personas: Array[Set[Persona]] =
      for (nombre <- nombres) yield {
        nombre match {
          case patrón(una, otra) => Set(Persona(una, Some(otra)), Persona(otra, Some(una)))
          case soltera => Set(Persona(soltera, None))
      }
    }
    personas.flatten
  }

  /** Devuelve una persona candidata a ser amiga de la persona que elige.
    *
    * La persona se elige al azar aplicando las restricciones definidas en [[Persona.puedeSerMiAmiga()]].
    *
    * @param elige persona que elige amiga.
    * @param elegidas conjunto de personas entre las que puede elegir.
    * @return una persona candidata a ser amiga de la persona que elige.
    */
  @tailrec
  def eligeAmiga(elige: Persona, elegidas: List[Persona]): Persona = {
    if (elige puedeSerMiAmiga elegidas.head) elegidas.head
    else eligeAmiga(elige, Random.shuffle(elegidas))
  }

  /** Devuelve un mapa de nombres de personas combinadas de acuerdo a las reglas del juego de amigo invisible.
    *
    * Toma dos conjuntos de personas y asigna aleatoriamente a cada persona del primer conjunto una persona del segundo aplicando las restricciones definidas en [[Persona.puedeSerMiAmiga()]].
    *
    * Si los conjuntos no contienen los mismos elementos los resultados son impredecibles.
    *
    * @param eligen conjunto de las personas que eligen amiga.
    * @param elegidas conjunto de las personas entre las que se puede elegir.
    * @param combinaciones mapa en el que se registran las combinaciones.
    * @return mapa que relaciona el nombre de la persona con el de la amiga asignada.
    */
  @tailrec
  def emparejaAmigas(eligen: List[Persona], elegidas: List[Persona], combinaciones: Map[String, String]): Map[String, String] = {
    if (eligen.isEmpty || elegidas.isEmpty) {
      combinaciones
    } else {
      val elige = eligen.head
      val elegida = eligeAmiga(elige, elegidas)
      emparejaAmigas(eligen.tail, elegidas.tail, combinaciones + (elige.nombre -> elegida.nombre))
    }
  }
}