/** Created by Jose Gisbert on 18/12/2014.
  */
case class Persona(nombre: String, pareja: Option[String]) {
  /**
    * Determina si esta persona es pareja de la otra persona
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
  val puedeSerMiAmiga: (Persona) => Boolean = esMiPareja _ andThen(!_)
}

object Persona {
  implicit def orderingByNombre[A <: Persona]: Ordering[A] = Ordering.by(_.nombre)
  val orderingByEmparejado: Ordering[Persona] = Ordering.by(_.pareja)
}