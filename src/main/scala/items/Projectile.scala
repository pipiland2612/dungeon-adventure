package items

import entities.{Creatures, Direction, State}
import game.GamePanel
import utils.Animation

import java.awt.image.BufferedImage
import java.awt.Rectangle

abstract class Projectile (gp: GamePanel) extends Item(gp):
  var pos = (0,0)
  val maxHealth: Int
  var health: Int = maxHealth
  var alive : Boolean = false
  var user: Creatures = _

  var name: String
  var damage: Int
  speed = 0
  var costMana : Int

  def flyAnimation: Map[Direction, BufferedImage]
  var solidArea: Rectangle = Rectangle()

  def getDescription: String = s"Firing $name \ncan deal $damage damages!"
  def haveEnoughMana(creatures: Creatures): Boolean = creatures.mana >= costMana
  def useMana(creatures: Creatures): Unit = creatures.mana -= costMana
  def fly (): Unit =
    if alive then
      image = flyAnimation(this.direction)

  def set(pos : (Int, Int), direction: Direction, isAlive: Boolean, creatures: Creatures): Unit =
    this.direction = direction
    this.pos = pos
    this.alive = isAlive
    this.health = maxHealth
    this.user = creatures

  def update(): Unit =
    direction match
      case Direction.UP => this.pos = (pos._1, -this.speed + pos._2)
      case Direction.DOWN => this.pos = (pos._1, this.speed + pos._2)
      case Direction.LEFT => this.pos = (pos._1 - this.speed, pos._2)
      case Direction.RIGHT => this.pos = (pos._1 + this.speed, pos._2)
      case Direction.ANY => ???
    health -= 1
    if health <= 0 then alive = false
    fly()

    if !alive then
      gp.projectileList = gp.projectileList.filterNot(_ == this)

    if user == gp.player then
      val enemyIndex = gp.cCheck.checkCollisionWithTargets(this, gp.enemyList)
      if enemyIndex != -1 then
        gp.player.attackEnemy(enemyIndex, this.damage)
        alive = false
    else
      val hasTouchPlayer = gp.cCheck.checkPlayer(this)
      if !gp.player.isInvinc && hasTouchPlayer then
        gp.player.takeDamage(this.damage)
        alive = false