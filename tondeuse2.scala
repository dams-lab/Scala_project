package tondeuse_projet

object tondeuse2 extends App {
                              //// FIRST STEP: Imputation des paramètres ////

  // echec: mettre les paramètres comme dans la question, je n'arrive pas ensuite à les regrouper comme ce qui suit.
  // Doit être changer à la main, ce qui représente un défaut dans ce projet//

  val paraPos = Array(5: Int, 5: Int, 1: Int, 2: Int, 3: Int, 3: Int)  // je voulais initiallement prendre les coord
  // maximales et les coord initiales et les regrouper dans un vecteur comme ceci
  val orientation= Array("N","E") // mettre les positions dans un autre
  val Mvt_tt_tondeuses= Array(Array("G", "A", "G", "A", "G", "A", "G", "A", "A",""),Array("A","A","D","A","A","D","A","D","D","A","") )
  // Et enfin mettre l'ensemble des mvts dans un autre array, avec l'ajout d'un blanc entre deux tonseuses

                            ////SECOND STEP: calcul du nombre de Tondeuses ////

  val Nb_tondeuses = ((paraPos.length - 2) / 2)
  var b = 2  // utilisation pour référer à mes paramètres de mes variables
  var tondeuse = 1  // création de ma variable tondeuse, le chiffre correpond à la enième tondeuse à passer

           //// THIRD STEP création d'une boucle déterminant le passage de toutes les tondeuses////

  for (tondeuse <- 1 to Nb_tondeuses) {

    // J'instancie mes paramètres en fonction du numéro de la tondeuse
    val xmin = 0
    val ymin = 0
    var x = paraPos(b*tondeuse)
    var y = paraPos(b*tondeuse+1)
    val xmax = paraPos(0)
    val ymax = paraPos(1)
   // val coord_depart = Array(x, y)
    var Newposition: String = orientation(tondeuse-1)
    // Création de mes mouvements pour ma enième tondeuse

    var mvt= Mvt_tt_tondeuses(tondeuse-1)
    var mvtt: String = mvt(1)

          //// ACTION STEP: déclaration des changements des coords en fonction de l'action ////

    def coord_x_y() {
      // si on va à gauche, la tondeuse tourne, la position change donc
      if (mvtt == "G") {
        if (Newposition == "N") {
          Newposition = "W"
        }
        else if (Newposition == "E") {
          Newposition = "N"
        }
        else if (Newposition == "W") {
          Newposition = "S"
        }
        else if (Newposition == "S") {
          Newposition = "E"
        }
        else {
          Newposition = "error"
        }
      }
      else if (mvtt == "D") {
        //si on va à droite, la tondeuse tourne dans l'autre sens, on change aussi la position
        if (Newposition == "N") {
          Newposition = "E"
        }
        else if (Newposition == "E") {
          Newposition = "S"
        }
        else if (Newposition == "W") {
          Newposition = "N"
        }
        else if (Newposition == "S") {
          Newposition = "W"
        }
        else {
          Newposition = "error"
        }
      }

      else if (mvtt == "A" & y <= ymax & y >= ymin & x >= xmin & x <= xmax) {
        // on ne peut avancer que si la tondeuse est encore dans la surface..., on paramètre chacun des mvts de sorte
        // à appliquer l'action que si possible
        if (Newposition == "N" & y < ymax) {
          y = y + 1
        }
        else if (Newposition == "W" & x > xmin) {
          x = x - 1
        }
        else if (Newposition == "E" & x < xmax) {
          x = x + 1
        }
        else if (Newposition == "S" & y > ymin) {
          y = y - 1
        } else {
          println("tondeuse hors terrain: veillez à l'éteindre pour le bien de l'environnement!")
        }
      }
      else {
        println(x, y, Newposition)
      }

    }
    // application des mouvements de ma tondeuse en fonction du paramètrage déclaré
    for (a <- mvt) {
      mvtt = a
      coord_x_y()
    } //passage à la tondeuse suivante à partir d'ici
  }
}

