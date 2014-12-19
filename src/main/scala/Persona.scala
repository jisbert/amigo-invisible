/** Una persona para el sorteo de amigo invisible.
  *
  * @constructor Crea una persona con nombre y pareja, si la tiene.
  * @param nombre nombre de la persona.
  * @param pareja nombre de la pareja si es que la tiene.
  *
  * @author Jose Gisbert <jgisbert@umh.es>
  * @version 0.1.0
  * @since 18/12/2014
  */
case class Persona(nombre: String, pareja: Option[String]) {

  /** Determina si esta persona y la otra son la misma. */
  override def equals(arg0: Any) = arg0 match {
    case Persona(suNombre, _) => nombre == suNombre
    case _ => false
  }

  /** Determina si esta persona es pareja de la otra persona. Devuelve:
    *
    *  a. Verdadero si ambas personas tienen pareja y la pareja de una es la de la otra.
    *  b. Falso si ambas tienen pareja y la de una no es la de la otra.
    *  c. Nada si alguna de las personas no tiene pareja.
    *
    * @param ella la otra persona.
    * @return booleano que indica si esta persona es pareja de la otra persona.
    */
  def esMiPareja(ella: Persona) = {
    val somosPareja =
      for {
        pareja <- pareja
        suPareja <- ella.pareja
      } yield pareja == ella.nombre || suPareja == nombre
    somosPareja getOrElse false
  }

  /** Determina si esta persona puede ser amiga de la otra persona. Si son la misma persona o son pareja no pueden ser amigas.
    *
    * @param ella la otra persona.
    * @return verdadero si las personas pueden ser amigas.
    */
  def puedeSerMiAmiga(ella: Persona) = this == ella && !esMiPareja(ella)
}



/** Define métodos de ordenación estáticos de la clase Persona. */
object Persona {

  /** Ordena las personas alfabéticamente por su nombre. */
  implicit def ordenarPorNombre[A <: Persona]: Ordering[A] = Ordering.by(_.nombre)

  /** Ordena primero a las personas solteras y a continuación las que tienen pareja alfabéticamente por el nombre de su pareja. */
  val ordenarPorEmparejado: Ordering[Persona] = Ordering.by(_.pareja)
}