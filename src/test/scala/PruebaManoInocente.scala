import ManoInocente._
import Persona.ordenarPorEmparejado

import scala.util.Random

class PruebaManoInocente extends UnitTest {
  // TODO: Implementar un generador de personas para las pruebas
  val args = Array("Juan", "Fran y Lucía", "María José y Tomás", "José Ramón", "Carol", "María del Mar", "Javi y Raquel", "Miguel Ángel y María Teresa", "Sergio y Carla", "Fátima y Luis Miguel", "Elsa", "Elena", "Sofía")
  val personas = procesaNombres(args)
  val eligen = personas.toList.sorted(ordenarPorEmparejado).reverse
  val elegidas = Random.shuffle(personas.toList)
  val combinaciones = emparejaAmigas(eligen, elegidas, Map())
  println(combinaciones mkString ",")

  "ManoInocente" should "no dejar que dos o más personas elijan a la misma persona como amiga" in {
    val amigas = combinaciones.values.toList
    val duplicadas = amigas diff amigas.distinct
    assert(duplicadas.isEmpty, ", hay personas que han sido elegidas dos veces")
  }

  it should "devolver un mapa en el que se apliquen las reglas del amigo invisible" in {
    for {
      (ella, amiga) <- combinaciones
      elige <- eligen
      if elige.nombre == ella
      elegida <- elegidas
      if elegida.nombre == amiga
    } {
      assert(elige puedeSerMiAmiga elegida, ", hay personas que no cumplen las reglas del amigo invisible")
    }
  }
}