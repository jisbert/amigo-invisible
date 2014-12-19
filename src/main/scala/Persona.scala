/** Persona.
  *
  * @param nombre Nombre de la persona.
  * @param pareja Opción que refleja el nombre de la pareja si es que la tiene.
  *
  * @author Jose Gisbert <jgisbert@umh.es>
  * @version 0.1.0
  * @since 18/12/2014
  */
case class Persona(nombre: String, pareja: Option[String]) {
  /** Determina si esta persona es pareja de la otra persona. Si una de las personas no considera pareja a la otra el método determina que no son pareja.
    *
    * @param ella La otra persona
    * @return Devuelve cierto si ambos tiene pareja y la pareja del uno es la del otro
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
    */
  val puedeSerMiAmiga: (Persona) => Boolean = esMiPareja _ andThen(!_)
}

object Persona {
  implicit def orderingByNombre[A <: Persona]: Ordering[A] = Ordering.by(_.nombre)
  val orderingByEmparejado: Ordering[Persona] = Ordering.by(_.pareja)
}