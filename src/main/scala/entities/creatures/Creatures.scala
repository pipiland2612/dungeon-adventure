package entities.creatures

import enemy.Enemy
import Direction.ANY
import entities.Entity
import game.GamePanel
import utils.Animation

import java.awt.{Graphics2D, Rectangle}

abstract class Creatures(gp: GamePanel) extends Entity(gp) :

  // ----------------------------------------------------------------------------------------------
  // Creature States and Attributes

  val maxAttackCooldown: Int = 45
  var attackCooldown: Int = maxAttackCooldown
  var animationCounter = 0
  var damagePower: Int = 0
  var counter = 0

  var health: Int = 0
  var maxHealth: Int = 0
  var isAttacking = false
  var maxMana: Int = 0
  var mana: Int = 0
  var isOnPath: Boolean = false
  var isInvinc : Boolean = false
  var invincibleDuration = 0
  var maxInvincDuration: Int = 0
  var dying = false
  var dyingCounter = 0
  var shootCounter = 0

  // ----------------------------------------------------------------------------------------------
  // Animation and Rendering
  var idleAnimations: Map[Direction, Animation]
  var runAnimations: Map[Direction, Animation]
  var attackAnimations: Map[Direction, Animation]

  var attackTimeAnimation: Int = 0
  var needsAnimationUpdate = false

  def images: Map[(Direction, State), Animation] =
    if idleAnimations != null || runAnimations != null || attackAnimations != null then
      Map(
        (Direction.RIGHT, State.IDLE) -> idleAnimations(Direction.RIGHT),
        (Direction.DOWN, State.IDLE) -> idleAnimations(Direction.DOWN),
        (Direction.LEFT, State.IDLE) -> idleAnimations(Direction.LEFT),
        (Direction.UP, State.IDLE) -> idleAnimations(Direction.UP),

        (Direction.RIGHT, State.RUN) -> runAnimations(Direction.RIGHT),
        (Direction.DOWN, State.RUN) -> runAnimations(Direction.DOWN),
        (Direction.LEFT, State.RUN) -> runAnimations(Direction.LEFT),
        (Direction.UP, State.RUN) -> runAnimations(Direction.UP),

        (Direction.UP, State.ATTACK) -> attackAnimations(Direction.UP),
        (Direction.DOWN, State.ATTACK) -> attackAnimations(Direction.DOWN),
        (Direction.LEFT, State.ATTACK) -> attackAnimations(Direction.LEFT),
        (Direction.RIGHT, State.ATTACK) -> attackAnimations(Direction.RIGHT),

      )
    else null

  def attackHitBox: Rectangle = direction match
    case Direction.UP => Rectangle(pos._1 + (areaHitBox.x / 1.5).toInt , pos._2 + areaHitBox.y - attackArea.height, attackArea.width, attackArea.height)
    case Direction.DOWN => Rectangle(pos._1 + (areaHitBox.x / 1.5).toInt, pos._2 + areaHitBox.y + areaHitBox.height /2 + attackArea.height, attackArea.width, attackArea.height)
    case Direction.LEFT => Rectangle(pos._1 + areaHitBox.x - attackArea.width, pos._2 + (areaHitBox.y * 3/2), attackArea.width, attackArea.height)
    case Direction.RIGHT => Rectangle(pos._1 + areaHitBox.x - areaHitBox.width + attackArea.width, pos._2 + (areaHitBox.y * 3/2), attackArea.width, attackArea.height)
    case ANY => Rectangle(0,0,0,0)
  // ----------------------------------------------------------------------------------------------
  // Creature Actions and State Management

  def takeDamage(amount: Int): Unit =
    this.health -= amount
    if this.health < 0 then
      die()

  def isAlive: Boolean = health > 0
  def isDead: Boolean = !isAlive
  
  def usePotion(creatures: Creatures): Unit = {}
  def setAction(): Unit = {}

  def move(dx: Int, dy: Int): Unit =
    state = State.RUN
    this.pos = (pos._1 + dx, pos._2 + dy)
    needsAnimationUpdate = true

  def handleInvincibility(): Unit =
    if isInvinc then
      invincibleDuration += 1
      if invincibleDuration > maxInvincDuration then
        invincibleDuration = 0
        isInvinc = false

  def die(): Unit =
    if (state != State.DEAD) then
      state = State.DEAD

    needsAnimationUpdate = true
    checkAnimationUpdate()

  def dealDamage(damagePower: Int): Unit = {}

  def attack(): Unit =
    if state != State.ATTACK && attackCooldown <= 0 then
      state = State.ATTACK
      needsAnimationUpdate = true
      animationCounter = 0
      attackCooldown = maxAttackCooldown

      val attackAnimation = attackAnimations(direction)

      if this != gp.player then
        if gp.cCheck.checkPlayerWithOtherHitBox(this, attackHitBox) && attackAnimation.isInAttackInterval then
          this.dealDamage(this.damagePower)
      else if this == gp.player then
        val enemyIndex = gp.cCheck.checkCollisionWithOtherHitBox(this, gp.enemyList, attackHitBox)
        if attackAnimation.isInAttackInterval then
          gp.player.attackEnemy(enemyIndex, gp.player.attackDamage)

  // Animations
  def checkAnimationUpdate (): Unit =
    if(needsAnimationUpdate) then
      if currentAnimation != null then
        currentAnimation = images.getOrElse((direction, state), images((direction, State.IDLE)))
        needsAnimationUpdate = false
        currentAnimation.update()

  def continueMove (): Unit =
    if !isCollided then
      direction match
        case Direction.UP => this.move(0, -this.speed)
        case Direction.DOWN => this.move(0, this.speed)
        case Direction.LEFT => this.move(-this.speed, 0)
        case Direction.RIGHT => this.move(this.speed, 0)
        case ANY =>
    else
      this match
        case enemy: Enemy => 
          enemy.checkToAttack(enemy.attackRate, enemy.verticalScanRange, enemy.horizontalScanRange)
        case player: Player => 
          if currentAnimation != null then
            currentAnimation = images.getOrElse((direction, state), images((direction, State.IDLE)))
        case _ =>

  // ----------------------------------------------------------------------------------------------
  // Game Loop Update and Collision Handling

  def update(): Unit =
    if attackCooldown > 0 then
      attackCooldown -= 1
    setAction()

    state match
      case State.ATTACK =>
        handleAttackState()
      case State.DEAD =>
      case _ =>
        checkCollision()
        checkAnimationUpdate()
        continueMove()

  def handleAttackState(): Unit =
    animationCounter += 1
    if (animationCounter >= attackTimeAnimation) then
      animationCounter = 0
      state = State.IDLE
    needsAnimationUpdate = true
    checkAnimationUpdate()

  private def checkCollision(): Unit =
    isCollided = false
    gp.cCheck.checkTileCollision(this)
    gp.cCheck.checkObjectCollision(this, false)
    gp.cCheck.checkCollisionWithTargets(this, gp.npcList)
    gp.cCheck.checkCollisionWithTargets(this, gp.enemyList)
    val hasTouchedPlayer = gp.cCheck.checkPlayer(this)

  def findPath(goalRow: Int, goalCol: Int): Unit =
    val startCol = (this.pos._1 + this.solidArea.x) / gp.tileSize
    val startRow = (this.pos._2 + this.solidArea.y) / gp.tileSize

    gp.pFinder.setNodes(startRow, startCol, goalRow, goalCol)

    val hasFoundPath = gp.pFinder.search()
    if hasFoundPath && gp.pFinder.pathList.nonEmpty then
      val (nextX, nextY) = (gp.pFinder.pathList.head.col * gp.tileSize, gp.pFinder.pathList.head.row * gp.tileSize)
      val enLeftX = this.pos._1 + solidArea.x
      val enRightX = this.pos._1 + solidArea.x + solidArea.width
      val enTopY = this.pos._2 + solidArea.y
      val enBottomY = this.pos._2 + solidArea.y + solidArea.height

      // BASE on position, find the relative nodes, check different situations
      if enTopY > nextY && enLeftX >= nextX && enRightX < nextX + gp.tileSize then
        direction = Direction.UP
      else if enTopY < nextY && enLeftX >= nextX && enRightX < nextX + gp.tileSize then
        direction = Direction.DOWN
      else if enTopY >= nextY && enBottomY < nextY + gp.tileSize then
        if enLeftX > nextX then
          direction = Direction.LEFT
        if enLeftX < nextX then
          direction = Direction.RIGHT
      else if enTopY > nextY && enLeftX > nextX then
        direction = Direction.UP
        checkCollision()
        if collision then
          direction = Direction.LEFT
      else if enTopY > nextY && enLeftX < nextX then
        direction = Direction.UP
        checkCollision()
        if collision then
          direction = Direction.RIGHT
      else if enTopY < nextY && enLeftX > nextX then
        direction = Direction.DOWN
        checkCollision()
        if collision then
          direction = Direction.LEFT
      else if enTopY < nextY && enLeftX < nextX then
        direction = Direction.DOWN
        checkCollision()
        if collision then
          direction = Direction.RIGHT

      // STOP WHEN REACH GOALS
      val nextRow = gp.pFinder.pathList.head.row
      val nextCol = gp.pFinder.pathList.head.col
      if nextCol == goalCol && nextRow == goalRow then
        isOnPath = false


  // Rendering Methods
  override def draw(g: Graphics2D): Unit = 
    super.draw(g)

  
