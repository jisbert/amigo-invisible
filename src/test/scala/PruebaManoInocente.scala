import org.scalatest._

class PruebaManoInocente extends FlatSpec with Matchers {
  "ManoInocente" should "descomponer los nombres en solteros y parejas" in {
    val args = Array("Juan", "Fran y Lucía", "María José y Tomás", "José Ramón", "Carol", "María del Mar", "Javi y Raquel", "Miguel Ángel y María Teresa", "Sergio y Carla", "Fátima y Luis Miguel", "Elsa", "Elena", "Sofía")
    ManoInocente main args
  }
}