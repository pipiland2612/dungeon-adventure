package game

import entities.Entity
import java.awt.image.BufferedImage
import entities.creatures.enemy.Enemy
import entities.creatures.{Npc, Player}
import entities.items.Projectile
import system.objectManager.ObjectManager
import system.behavior.PathFinder
import system.data.SaveLoad
import system.environment.{Area, EnvironmentManager}
import system.event.EventHandler
import system.tile.{Map, TileManager}
import system.ui.UI

import java.awt.{Color, Dimension, Font, Graphics, Graphics2D}
import javax.swing.JPanel
import utils.{CollisionChecker, Config, CutsceneManager, EntityGenerator, KeyHandler, Sound, Tools}

import scala.collection.mutable.ListBuffer

class GamePanel extends JPanel with Runnable:
  // ----------------------------------------------------------------------------------------------
  //Screen settings
  val originTileSize = 16
  val scale = 3
  val tileSize = originTileSize * scale
  val maxScreenColumn = 20
  val maxScreenRow = 12
  val screenWidth = maxScreenColumn * tileSize
  val screenHeight = maxScreenRow * tileSize

  // Worlds settings
  val maxWorldCol = 50
  val maxWorldRow = 50
  val maxMap = 3
  var currentMap = 0
  val FPS = 60

  //----------------------------------------------------------------------------------------------
  //INITIALIZATION
  val backGroundImageOverWorld: BufferedImage = Tools.loadImage("maps/overworld_background.png")
  val backGroundImageDungeon: BufferedImage   = Tools.loadImage("maps/dungeon_background.png")
  var backGroundImage: BufferedImage = backGroundImageOverWorld

  this.setPreferredSize(new Dimension(screenWidth, screenHeight))
  this.setBackground(Color.BLACK)

  // SYSTEM
  val tileManager = TileManager(this)
  val keyH = KeyHandler(this)
  val sound: Sound = new Sound()
  val se: Sound = new Sound()
  val cCheck = CollisionChecker(this)
  val oManager = ObjectManager(this)
  val gui: UI = UI(this)
  val eHandler: EventHandler =  EventHandler(this)
  val config: Config = Config(this)
  var pFinder: PathFinder = PathFinder(this)
  var environmentManager = EnvironmentManager(this)
  var miniMap: Map = new Map(this)
  var saveLoad: SaveLoad = new SaveLoad(this)
  val entityGen = new EntityGenerator(this)
  val cutSceneManager = new CutsceneManager(this)
  var gameThread: Thread = _

  //ENTITY, OBJECT, NPCS, ENEMIES
  private val playerStartingCoords = (tileSize * 43, tileSize * 46)
  var player = Player((playerStartingCoords._1, playerStartingCoords._2), this)
  val obj: Array[Array[Entity]] = Array.ofDim[Entity](maxMap, 80)
  var enemyList: Array[Array[Enemy]] = Array.ofDim[Enemy](maxMap, 40)
  var npcList : Array[Array[Npc]] = Array.ofDim[Npc](maxMap, 5)
  var projectileList: ListBuffer[Projectile] = ListBuffer[Projectile]()
  var entityList: ListBuffer[Entity] = ListBuffer[Entity]()

  //GAME STATE
  var gameState: GameState = GameState.TitleState
  // AREA
  var currentArea: Area = Area.OverWorld
  var nextArea: Area = Area.Dungeon
  var onBossBattle: Boolean = false

  //----------------------------------------------------------------------------------------------
  // SET UP METHODS
  def startGameThread(): Unit =
    gameThread = new Thread(this)
    gameThread.start()

  def setUpGame (): Unit =
    oManager.setObject()
    oManager.setEnemy()
    oManager.setNpc()
    environmentManager.setup()
    playMusic(0)

  def resetGame(restart: Boolean): Unit =
    this.currentMap = 0
    onBossBattle = false
    removeTemporaryObj()
    currentArea = Area.OverWorld
    player.reset()
    oManager.setEnemy()
    oManager.setNpc()
    projectileList.clear()
    entityList.clear()

    if restart then
      player = new Player((tileSize * 43, tileSize * 46), this)
      oManager.setObject()

  //----------------------------------------------------------------------------------------------
  // MUSICS SOUNDS methods
  def playMusic (int : Int) = this.sound.setFile(int); this.sound.play(); this.sound.loop()
  def stopMusic (): Unit = this.sound.stop()
  def playSE (int : Int) = this.se.setFile(int); this.se.play()

  def changeArea(): Unit =
    if nextArea != currentArea then
      currentArea = nextArea
    updateBackGroundImage()

  def removeTemporaryObj(): Unit =
    for i <- obj(1).indices do
      if obj(currentMap)(i) != null && obj(currentMap)(i).isTemp then
        obj(currentMap)(i) = null

  // ----------------------------------------------------------------------------------------------
  // Game Loop Update
  def update(): Unit =
    updateBackGroundImage()
    if gameState == GameState.PlayState then
      this.player.update()
      
      for i <- npcList(1).indices do
        if npcList(currentMap)(i) != null then
          npcList(currentMap)(i).update()

      for i <- enemyList(1).indices do
        val currentEnemy = enemyList(currentMap)(i)
        if currentEnemy != null then
          currentEnemy.update()
          if currentEnemy.dying then
            currentEnemy.checkDrop()
            enemyList(currentMap)(i) = null

      for projectile <- projectileList do
        if projectile != null then
          projectile.update()

      environmentManager.update()

    else if gameState == GameState.PauseState then {}

  def updateBackGroundImage(): Unit =
    currentArea match
      case Area.OverWorld =>
        if backGroundImage != backGroundImageOverWorld then
          backGroundImage = backGroundImageOverWorld
      case Area.Dungeon =>
        if backGroundImage != backGroundImageDungeon then
          backGroundImage = backGroundImageDungeon

  // RENDERING METHODS
  override def paintComponent(g: Graphics): Unit =
    super.paintComponent(g)
    val g2d = g.asInstanceOf[Graphics2D]

    // PRESS T TO OPEN DEBUG
    var startTime: Long = 0
    if(keyH.showDebugText) then
      startTime = System.nanoTime()

    //BACKGROUND
    if backGroundImage != null then
      g2d.drawImage(backGroundImage, -10, -10, 1792, 1024, null)

    if gameState == GameState.TitleState then
      gui.drawUI(g2d)
    else if gameState == GameState.MapState then
      miniMap.drawFullMapScreen(g2d)
    else
      // PLAYSTATE
      //TILE
      tileManager.drawTiles(g2d)

      // ADDING ENTITIES
      entityList += player

      // ADDING OBJs, enemy, projectile, NPCS
      for i <- npcList(1).indices do
        if npcList(currentMap)(i) != null then
          entityList += npcList(currentMap)(i)

      for i <- obj(1).indices do
        if obj(currentMap)(i) != null then
          entityList += obj(currentMap)(i)

      for i <- enemyList(1).indices do
        if enemyList(currentMap)(i) != null then
          entityList += enemyList(currentMap)(i)

      for (projectile <- projectileList) do
        if projectile != null then
          entityList += projectile

      // SORT LIST BY Y POSITION
      entityList = entityList.sortBy(entity => entity.getPosition._2 + entity.solidAreaDefaultY)

      //DRAW
      for entity <- entityList do
        entity.draw(g2d)
      //EMPTY LIST
      entityList.clear()

      // IF YOU WANT TO BE MORE CLEAR HOW'S THE GAME WORK, UNCOMMENT THE BELOW LINE
//      Tools.renderDebugInfo(g2d, player, obj, enemyList, this)

      // ENVIRONMENT
      environmentManager.draw(g2d)
      miniMap.drawMiniMap(g2d)

      //UI
      gui.drawUI(g2d)
      //
      cutSceneManager.draw(g2d)

//      Debug
      if(keyH.showDebugText) then
        val endTime = System.nanoTime()
        val passTime = endTime - startTime
        g2d.setFont(new Font("Arial",Font.PLAIN, 20))
        g2d.setColor(Color.WHITE)
        val x = 10
        var y = 400
        var lineHeight = 20

        g2d.drawString("Position x: " + player.getPosition._1, x,y);y += lineHeight
        g2d.drawString("Position y: " + player.getPosition._2, x,y);y += lineHeight
        g2d.drawString("Col: " + (player.getPosition._1 + player.solidArea.x) / tileSize, x,y);y += lineHeight
        g2d.drawString("Row: " + (player.getPosition._2 + player.solidArea.y) / tileSize, x,y);y += lineHeight
        g2d.drawString("Draw time: " + passTime, x,y)

    g2d.dispose()

  // ----------------------------------------------------------------------------------------------
  // GAME LOOP
  override def run(): Unit =
    val drawInterval: Double = 1e9 / FPS
    var delta: Double = 0
    var lastTime: Long = System.nanoTime()
    var currentTime: Long = 0
    var timer: Long = 0
    var drawCount: Int = 0

    while (gameThread != null) do
      currentTime = System.nanoTime()
      delta += (currentTime - lastTime) / drawInterval
      timer += (currentTime - lastTime)
      lastTime = currentTime

      if delta >= 1 then
        update()
        repaint()
        delta -= 1
        drawCount += 1