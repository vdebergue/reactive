package simulations

import math.random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt
  def isBelowRate(rate: Int): Boolean = randomBelow(101) <= rate
  def randomMove = randomBelow(4) match {
    case 1 => (0,1)
    case 2 => (1,0)
    case 3 => (-1, 0)
    case 0 => (0, -1)
    case _ => throw new Error("Ooopsie Daisy")
  }
  def modulo(a: Int, b: Int) = {
    (a + b) % b
  }

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    // to complete: additional parameters of simulation
    val daysBeforeMoving: Int = 5
    val transmibilityRate = 40
    val prevalenceRate = 1
    val deathRate = 25
    val daysBeforeSick = 6
    val daysBeforeDeath = 14
    val daysBeforeImmune = 16
    val daysBeforeReset = 18

    val airplane = false
    val airplaneRate = 1
  }


  import SimConfig._

  val persons: List[Person] = for (i <- (1 to population).toList) yield new Person(i)
  for(i <- 1 to population*prevalenceRate/100) {
    persons(i).getInfected
  }

  class Person (val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    afterDelay(1 + randomBelow(daysBeforeMoving)) { move }

    //
    // to complete with simulation logic
    //
    def move {
      if (!this.dead) {
        if (airplane && isBelowRate(airplaneRate)) {
          row = randomBelow(roomRows)
          col = randomBelow(roomColumns)
          movedIntoNewRoom
        } else {
          val (rowMove, colMove) = randomMove
          val newRow = modulo(row + rowMove, roomRows)
          val newCol = modulo(col + colMove, roomColumns)
          val nextRoomInfected = persons.exists(p => p.row == newRow && p.col == newCol && (p.sick || p.dead))
          if(!nextRoomInfected) {
            row = newRow
            col = newCol
            movedIntoNewRoom
          }
        }
        afterDelay(1 + randomBelow(daysBeforeMoving)) { move }
      }
    }

    def movedIntoNewRoom {
      assert(row >= 0, "no neg index")
      assert(col >= 0, "no neg index")
      if(!this.immune) {
        val roomContagious = persons.exists(p => p.row == row && p.col == col && p.infected)
        if (roomContagious) {
          if(isBelowRate(transmibilityRate) && !immune) getInfected
        }
      }
    }

    def getInfected {
      this.infected = true
      afterDelay(daysBeforeSick) { this.sick = true}
      afterDelay(daysBeforeDeath) { 
        if(isBelowRate(deathRate))  this.dead = true 
      }
      afterDelay(daysBeforeImmune) { 
        if(!this.dead) {
          this.immune = true 
          this.sick = false
        }
      }
      afterDelay(daysBeforeReset) { 
        if(!this.dead) {
          this.immune = false
          this.infected = false
          this.sick = false
        }
      }
    }

  }
}
