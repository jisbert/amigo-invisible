import scala.annotation.tailrec
import scala.util.Random

object ManoInocente {
  def main(args: Array[String]): Unit = {
    val personas = procesaNombres(args)
    val amigas = emparejaAmigas(personas, personas, Map[String, String]())
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
    personas.flatten.toSet
  }

  /** @todo Revisar el bucle while, hay un proceso infinito en el código y probablemente esté ahí. 
    *
    * Combina personas de acuerdo a las reglas del juego de amigo invisible.
    *
    * Toma dos conjuntos de personas y asigna aleatoriamente a cada persona del primer conjunto una persona del segundo con las siguientes restricciones (la condición de elegibilidad está implementada en [[Persona.puedeSerMiAmiga()]]):
    *
    *  - No asigna una persona a sí misma.
    *  - No asigna a una persona su pareja.
    *
    * Si no se proporcionan conjuntos idénticos (eligen, elegidas) los resultados son impredecibles.
    *
    * @param eligen conjunto de las personas que eligen amiga.
    * @param elegidas conjunto de las personas elegidas como amiga.
    * @param combinaciones mapa en el que se registran las combinaciones de amigas.
    * @return mapa que relaciona el nombre de la persona con el de la amiga asignada.
    */
  @tailrec
  def emparejaAmigas(eligen: Set[Persona], elegidas: Set[Persona], combinaciones: Map[String, String]): Map[String, String] = {
    if (eligen.isEmpty || elegidas.isEmpty) {
      combinaciones
    } else {
      val elige = eligen.head
      while (!(elige puedeSerMiAmiga elegidas.head)) {Random.shuffle(elegidas)}
      val elegida = elegidas.head
      combinaciones + (elige.nombre -> elegida.nombre)
      emparejaAmigas(eligen.tail, elegidas.tail, combinaciones)
    }
  }
}