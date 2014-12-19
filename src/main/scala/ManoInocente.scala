import scala.annotation.tailrec
import scala.util.Random

object ManoInocente {
  def main(args: Array[String]): Unit = {
    // Distingue entre solteros y parejas
    val p = """([\p{L} ]*) y ([\p{L} ]*)""".r
    for (n <- args) {
      n match {
        case p(a, b) => ???
        case s => ???
      }
    }
  }

  /** Combina personas de acuerdo a las reglas del juego de amigo invisible.
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
  def combinaAmigas(eligen: Set[Persona], elegidas: Set[Persona], combinaciones: Map[String, String]): Map[String, String] = {
    if (eligen.isEmpty || elegidas.isEmpty) {
      combinaciones
    } else {
      val elige = eligen.head
      while (!(elige puedeSerMiAmiga elegidas.head)) {Random.shuffle(elegidas)}
      val elegida = elegidas.head
      combinaciones + (elige.nombre -> elegida.nombre)
      combinaAmigas(eligen.tail, elegidas.tail, combinaciones)
    }
  }
}