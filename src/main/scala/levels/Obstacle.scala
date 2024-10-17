package levels


import entities.{Creatures, Entity}
import game.GamePanel
import items.Item

abstract class Obstacle(gp : GamePanel) extends Entity(gp):
  def activate(): Unit = {}
  
  def trigger(): Unit = {}
  
  def dealDamage(entity: Creatures): Unit =
    entity.takeDamage(1)
    

