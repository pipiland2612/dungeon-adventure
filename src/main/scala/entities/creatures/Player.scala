package entities.creatures

import entities.items.{Coin, InteractiveObjects, Item, Light, Potion, Projectile, Shield, Weapon}
import entities.`object`.{OBJ_Fireball, OBJ_LightCandle, OBJ_NormalHealFlask, OBJ_NormalShield, OBJ_NormalSword, OBJ_SilverKey}
import game.{GamePanel, GameState}
import system.ui.PlayerUI
import utils.{Animation, Tools}

import java.awt.image.BufferedImage
import java.awt.{AlphaComposite, Graphics2D, Rectangle}

class Player(var pos: (Int, Int), gp: GamePanel) extends Creatures(gp):
  direction = Direction.RIGHT
  currentWeapon = OBJ_NormalSword(gp)
  currentShield = OBJ_NormalShield(gp)
  currentProjectile = OBJ_Fireball(gp)
  
  // IF YOU WANT TO BEAT THE GAME ASAP, TURN THIS INTO TRUE
  private var isGodMode = false

  state = State.IDLE
  // ----------------------------------------------------------------------------------
  // Player stats
  var name = "Warrior"
  speed = 5
  maxHealth = 100
  health = maxHealth
  maxMana = 100
  mana = maxMana
  var strength = 1
  var dexterity = 1
  var exp = 0
  var level = 1
  var nextLevelExp = 5
  var attackDamage: Int = getAttackDamage
  var defense: Int = getDefense
  var coin = 25

  // other stats
  attackTimeAnimation = 30
  maxInvincDuration = 60
  var lightUpdated: Boolean = false
  private var hasCallDie = false

  var drawing: Boolean = true

  // Inventory
  setItems()

  // ----------------------------------------------------------------------------------
  // Player Position and collision
  var lastSavePoint: (Int, Int) = (gp.tileSize * 43, gp.tileSize * 46)
  var lastSavePointMap: Int = 0

  var solidAreaX = 22
  var solidAreaY = 37
  solidAreaDefaultX = solidAreaX
  solidAreaDefaultY = solidAreaY
  var solidAreaWidth: Int = (12 * 1.25).toInt
  var solidAreaHeight: Int = (12 * 1.25).toInt + 5
  var solidArea = Rectangle(solidAreaX , solidAreaY , solidAreaWidth, solidAreaHeight)

  areaDefaultX = solidAreaDefaultX
  areaDefaultY = 15
  areaHitBox = Rectangle(areaDefaultX, areaDefaultY, solidAreaWidth, solidAreaHeight * 2)

  // ----------------------------------------------------------------------------------------------
  // Screen and Rendering
  val screenX: Int = gp.screenWidth / 2 - (gp.tileSize / 2)
  val screenY: Int = gp.screenHeight / 2 - (gp.tileSize / 2)
  val playerScale: Int = (gp.tileSize * 1.25).toInt
  private val frameSize = 64
  val displayedImage: BufferedImage = Tools.loadImage("players/player_image.png")

  // ----------------------------------------------------------------------------------------------
  // ANIMATIONS
  private val spriteFrames = Tools.loadFrames("players/Player_spritesheet", frameSize, frameSize, playerScale, playerScale, 21)
  private val attackFrames = Tools.loadFrames("players/attack_spritesheet", frameSize, frameSize, playerScale, playerScale, 4)
  var idleAnimations: Map[Direction, Animation] = Map(
    Direction.UP -> Animation(Vector(spriteFrames(0)(0)), 1),
    Direction.LEFT -> Animation(Vector(spriteFrames(1)(0)), 1),
    Direction.DOWN -> Animation(Vector(spriteFrames(2)(0)), 1),
    Direction.RIGHT -> Animation(Vector(spriteFrames(3)(0)), 5),
  )

  var runAnimations: Map[Direction, Animation] = Map(
    Direction.UP -> Animation(
      Tools.getFrames(spriteFrames, 8, 8), 10),
    Direction.LEFT -> Animation(
      Tools.getFrames(spriteFrames, 9, 8), 10),
    Direction.DOWN -> Animation(
      Tools.getFrames(spriteFrames, 10, 8), 10),
    Direction.RIGHT -> Animation(
      Tools.getFrames(spriteFrames, 11, 8), 10),
  )

  var attackAnimations: Map[Direction, Animation] = Map(
    Direction.UP -> Animation(
      Tools.getFrames(attackFrames, 3, 6), 5, 0, 5),
    Direction.LEFT -> Animation(
      Tools.getFrames(attackFrames, 1, 6), 5, 0, 5),
    Direction.DOWN -> Animation(
      Tools.getFrames(attackFrames, 0, 6), 5, 0, 5),
    Direction.RIGHT -> Animation(
      Tools.getFrames(attackFrames, 2, 6), 5, 0, 5),
  )

  var deadAnimations: Map[Direction, Animation] = Map (
    Direction.UP -> Animation(
      Tools.getFrames(spriteFrames, 20, 6), 5),
    Direction.LEFT -> Animation(
      Tools.getFrames(spriteFrames, 20, 6), 5),
    Direction.DOWN -> Animation(
      Tools.getFrames(spriteFrames, 20, 6), 5),
    Direction.RIGHT -> Animation(
      Tools.getFrames(spriteFrames, 20, 6), 5),
  )

  needsAnimationUpdate = false
  currentAnimation = images((direction, state))

  // ----------------------------------------------------------------------------------------------
  // GETTER methods
  def getState: State = this.state
  def getHealth: Int = this.health
  override def getPosition: (Int, Int) = this.pos
  def getSpeed: Int = this.speed
  def getCurrentAnimation: Animation = this.currentAnimation
  def getDefense: Int = currentShield.defense * dexterity
  def getCurrentWeapon: Weapon = currentWeapon
  def getCurrentWeaponIndex: Int = inventory.indexWhere(_ == this.currentWeapon)
  def getCurrentShield: Shield = currentShield
  def getCurrentShieldIndex: Int = inventory.indexWhere(_ == this.currentShield)
  def getCurrentProjectile: Projectile = currentProjectile
  def getCurrentProjectileIndex: Int = inventory.indexWhere(_ == this.currentProjectile)
  def getCurrentLightIndex: Int = inventory.indexWhere(_ == this.currentLight)
  def getAttackDamage: Int =
    attackArea = currentWeapon.attackArea
    currentWeapon.damage * strength
  private def canShoot: Boolean = !currentProjectile.alive && shootCounter == 60 && this.currentProjectile.haveEnoughMana(this)

  // ----------------------------------------------------------------------------------------------
  // STATE AND ACTIONS

  def stop(): Unit =
    state = State.IDLE
    needsAnimationUpdate = true

  private def jump(): Unit =
    state = State.JUMP
    val newY = pos._1 + -3
    this.pos = (pos._1, newY)
    needsAnimationUpdate = true

  override def die(): Unit =
    if !hasCallDie then
      super.die()
      dyingCounter += 1
      if dyingCounter > 120 then
        gp.gui.isFinished = true
      hasCallDie = true

  private def resetCounter (): Unit =
    counter = 0
    shootCounter = 0

  def reset(): Unit =
    this.pos = (gp.tileSize * 23, gp.tileSize * 21)
    resetCounter()
    health = maxHealth
    mana = maxMana
    isInvinc = false
    lightUpdated = false

  // ----------------------------------------------------------------------------------------------
  // Input and Movement Handling

  private def handleInput(): Unit =
    if gp.keyH.attackPressed && attackCooldown == 0 then
      attack()
    else if Seq(gp.keyH.upPressed, gp.keyH.downPressed, gp.keyH.leftPressed, gp.keyH.rightPressed, gp.keyH.enterPressed).exists(identity) then
      handleMovementInput()
      gp.keyH.attackPressed = false
      gp.keyH.enterPressed = false
    else
      handleIdleState()
    if gp.keyH.shootKeyPressed && canShoot then shootProjectile()

  private def handleMovementInput(): Unit =
    updateDirection()
    checkAnimationUpdate()
    performCollisionsAndEvents()
    if !isCollided then
      continueMove()
    else
      currentAnimation = images.getOrElse((direction, state), images((direction, State.IDLE)))

  private def handleIdleState(): Unit =
    if state != State.IDLE then
      state = State.IDLE
      needsAnimationUpdate = true

  private def updateDirection(): Unit =
    if gp.keyH.upPressed then
      this.direction = Direction.UP
    else if gp.keyH.downPressed then
      this.direction = Direction.DOWN
    else if gp.keyH.leftPressed then
      this.direction = Direction.LEFT
    else if gp.keyH.rightPressed then
      this.direction = Direction.RIGHT

  // ----------------------------------------------------------------------------------------------
  // Collision and Event Handling
  private def performCollisionsAndEvents(): Unit =
    isCollided = false
    // CHECK TILE
    gp.cCheck.checkTileCollision(this)
    // CHECK OBJECT
    val objectIndex = gp.cCheck.checkObjectCollision(this, true)
    pickUpObject(objectIndex)
    // CHECK NPC
    val npcIndex = gp.cCheck.checkCollisionWithTargets(this, gp.npcList)
    interactNPC(npcIndex)
    // CHECK EVENT
    val enemyIndex = gp.cCheck.checkCollisionWithTargets(this, gp.enemyList)
//      if enemyIndex != -1 then
//        println(enemyIndex)
//        gp.enemyList(enemyIndex).attackPlayer(gp.enemyList(enemyIndex).attackPower)
    gp.eHandler.checkEvent()

  // ----------------------------------------------------------------------------------------------

  // Inventory and Items
  private def setItems(): Unit =
    this.inventory.clear()
    this.inventory += currentWeapon
    this.inventory += currentShield
    this.inventory += currentProjectile
    this.inventory += OBJ_LightCandle(gp)
    this.inventory += OBJ_NormalHealFlask(gp)
    this.inventory += OBJ_SilverKey(gp)
    this.inventory += OBJ_SilverKey(gp)

  def selectItem(): Unit =
    val itemIndex = PlayerUI.getItemIndexBySlot(PlayerUI.playerSlotCol, PlayerUI.playerSlotRow)
    if itemIndex < this.inventory.size then
      val selectedItem = this.inventory(itemIndex)
      handleSelectedItem(selectedItem, itemIndex)

  private def handleSelectedItem(item: Item, itemIndex: Int): Unit = item match
    case weapon: Weapon =>
      currentWeapon = weapon
      attackDamage = getAttackDamage
    case shield: Shield =>
      currentShield = shield
      defense = getDefense
    case coin: Coin =>
      coin.pickupCoin(this)
      inventory.remove(itemIndex)
    case potion: Potion =>
      if potion.use(this) then
        if potion.amount > 1 then potion.amount -= 1 else inventory.remove(itemIndex)
      else
        inventory.remove(itemIndex)
    case light: Light =>
      if currentLight == light then
        currentLight = null
      else
        currentLight = light
      lightUpdated = true
    case item: Item =>

      if item.use(this) then
        inventory.remove(itemIndex)

  private def searchItemInInventory(itemName: String): Int = inventory.indexWhere(_.name.equals(itemName))

  def obtainItem(item: Item): Boolean =
    val newItem = gp.entityGen.getObject(item.name).asInstanceOf[Item]
    if item.isStackable then handleStackableItem(newItem)
    else handleNewItem(newItem)

  private def handleStackableItem(item: Item): Boolean =
    val index = searchItemInInventory(item.name)
    if index != -1 then
      inventory(index).amount += 1
      true
    else handleNewItem(item)

  private def handleNewItem(item: Item): Boolean =
    if inventory.size != maxInventorySize then
      inventory += item
      true
    else
      false

  // ----------------------------------------------------------------------------------------------
  // Rendering methods

  override def draw(g: Graphics2D): Unit =
    if isInvinc then
      g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.3f))
    if isDead then if !hasCallDie then die()
    currentAnimation.getCurrentFrame match
      case frame =>
        if drawing then
          g.drawImage(frame,
            this.screenX,
            this.screenY,
            null)
    g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 1f))

  // ----------------------------------------------------------------------------------------------
  // GAME LOOP UPDATE
  override def update(): Unit =
    handleInvincibility()
    if !isGodMode then
      handleGameOver()
    if attackCooldown > 0 then
      attackCooldown -= 1

    state match
      case State.ATTACK => handleAttackState()
      case _ => handleInput()

    if shootCounter < 60 then
      shootCounter += 1
    gp.keyH.attackPressed = false

  private def handleGameOver(): Unit =
    if isDead then
      gp.gameState = GameState.GameOver
      gp.gui.commandNum = -1

  private def shootProjectile(): Unit =
    currentProjectile.set(this.pos, this.direction, true, this)
    currentProjectile.useMana(this)
    gp.projectileList += currentProjectile
    shootCounter = 0

  private def pickUpObject(index : Int): Unit =
    if index != -1 then
      var text: String = ""
      val obj = gp.obj(gp.currentMap)(index)
      obj match
        case coin: Coin =>
          coin.pickupCoin(this)
          gp.obj(gp.currentMap)(index) = null
        case item: Item =>
          if obtainItem(item) then
            gp.obj(gp.currentMap)(index) = null
            text = s"Picked up: ${item.name}"
          else text = "Your inventory is full"
        case interactiveObject :InteractiveObjects =>
          if gp.keyH.enterPressed then
            interactiveObject.interact()
        case _ =>
      gp.gui.addMessage(text)

  private def interactNPC(index : Int): Unit =
    if index != -1 then
      if gp.keyH.enterPressed then
        gp.gameState = GameState.DialogueState
        gp.npcList(gp.currentMap)(index).speak()
    gp.keyH.enterPressed = false

  def attackEnemy(enemyIndex: Int, damageDealt: Int): Unit =
    if enemyIndex != -1 then
      val currentEnemy = gp.enemyList(gp.currentMap)(enemyIndex)
      if !currentEnemy.isInvinc then
        var damage = damageDealt - currentEnemy.defense
        if damage < 0 then damage = 0
        currentEnemy.takeDamage(damage)
        currentEnemy.damageReaction()
        gp.gui.addMessage(s"$damage damage!")
        currentEnemy.isInvinc = true

        if !currentEnemy.isAlive then
          gp.gui.addMessage("Kill " + currentEnemy.name)
          exp += currentEnemy.expGet
          gp.gui.addMessage("Exp + " + exp)
          checkLevelUp()

  private def checkLevelUp(): Unit =
    if exp >= nextLevelExp then
      exp -= nextLevelExp
      level += 1
      nextLevelExp = nextLevelExp * 2
      maxHealth += 10
      strength += 1
      dexterity += 1
      attackDamage = getAttackDamage
      defense = getDefense
      // play level up sound/////
      gp.gameState = GameState.DialogueState

      dialogues(0)(0) = "You are at level " + level + " now!"
      startDialogue(this, 0)