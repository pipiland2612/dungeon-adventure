package utils

import Enemy.Enemy
import entities.{Creatures, Direction, Entity}
import game.GamePanel

import java.awt.geom.AffineTransform
import java.awt.{Color, Graphics2D, Image}
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

object Tools:
  // Images helper methods
  def flipImageHorizontally(image: BufferedImage): BufferedImage =
    val flippedImage = new BufferedImage(image.getWidth(), image.getHeight(), image.getType)
    val g2d: Graphics2D = flippedImage.createGraphics()
  
    val at = AffineTransform.getScaleInstance(-1, 1)
    at.translate(-image.getWidth(), 0)
    g2d.drawImage(image, at, null)

    g2d.dispose()
    flippedImage

  def flipImageVertically(image: BufferedImage): BufferedImage =
    val flippedImage = new BufferedImage(image.getWidth(), image.getHeight(), image.getType)
    val g2d: Graphics2D = flippedImage.createGraphics()
    val at = AffineTransform.getScaleInstance(1, -1)
    at.translate(0, -image.getHeight())
    g2d.drawImage(image, at, null)

    g2d.dispose()
    flippedImage

  def loadImage(path: String): BufferedImage =
    try
      ImageIO.read(new File("/Users/batman/Desktop/Adventure to Greece/src/main/resources/images/" + path))
    catch
      case e: Exception =>
        throw new RuntimeException(s"Failed to load image at path: $path")

  def scaleImage (origin : BufferedImage, width: Int, height: Int): BufferedImage =
    val resized = origin.getScaledInstance(width, height, Image.SCALE_DEFAULT)
    val scaledImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    val g: Graphics2D = scaledImage.createGraphics()
    g.drawImage(resized, 0, 0, width, height, null)
    g.dispose()

    scaledImage

  def flipFrames(frames: Vector[BufferedImage], flipFunction: BufferedImage => BufferedImage): Vector[BufferedImage] =
   frames.map(flipFunction)

  def setupCommonAnimations (flipFunction: BufferedImage => BufferedImage, commonFrames :Vector[BufferedImage], framesRate: Int): Map[Direction, Animation] = Map (
    Direction.UP -> Animation(commonFrames, framesRate),
    Direction.RIGHT -> Animation(commonFrames, framesRate),
    Direction.LEFT -> Animation(flipFrames(commonFrames, flipFunction), framesRate),
    Direction.DOWN -> Animation(flipFrames(commonFrames, flipFunction), framesRate)
  )

  // Position helper methods
  def resetSolidArea(entity: Entity): Unit =
    entity.solidArea.x = entity.solidAreaDefaultX
    entity.solidArea.y = entity.solidAreaDefaultY

  def updateSolidArea(entity: Entity): Unit =
    val (x,y) = entity.getPosition
    entity.solidArea.x = x + entity.solidAreaDefaultX
    entity.solidArea.y = y + entity.solidAreaDefaultY 
    
  def updateAreaHitBox(creatures: Creatures): Unit =
    val (x,y) = creatures.getPosition
    creatures.areaHitBox.x = x + creatures.areaDefaultX
    creatures.areaHitBox.y = y + creatures.areaDefaultY
  
  def resetAreaHitBox(creatures: Creatures): Unit =
    creatures.areaHitBox.x = creatures.areaDefaultX
    creatures.areaHitBox.y = creatures.areaDefaultY

  // Frames help methods
  def loadFrames (path: String, frameSizeX: Int, frameSizeY: Int , scale: Int, numsRow: Int): Array[Array[BufferedImage]] =
    val image: BufferedImage = Tools.loadImage(path + ".png")
    val frames: Array[Array[BufferedImage]] = SpriteSheet.splitSpriteSheet(image, frameSizeX, frameSizeY, scale, numsRow)
    frames

  def getFrames (framesName: Array[Array[BufferedImage]], rowsNum : Int, colLimit: Int): Vector[BufferedImage] =
    if (rowsNum >= framesName.length) then Vector.empty
    val selectedRow = framesName(rowsNum)
    val effectiveColLimit = Math.min(colLimit, selectedRow.length - 1)
    var vector = Vector[BufferedImage]()
    for (col <- 0 to effectiveColLimit) do
      vector = vector :+ selectedRow(col)

    vector

  // Debug method
  def renderDebugInfo(g: Graphics2D, entity: Entity, objects: Array[Entity], creatures: Array[Enemy], gp: GamePanel): Unit =
    g.setColor(Color.RED)
    g.drawRect(entity.solidArea.x + gp.player.screenX , entity.solidArea.y + gp.player.screenY, entity.solidArea.width, entity.solidArea.height)

    objects.foreach ( obj =>
      if (obj != null) then
        g.setColor(Color.BLUE)
        g.drawRect(
          obj.solidArea.x + obj.pos._1 - entity.getPosition._1 + gp.player.screenX,
          obj.solidArea.y + obj.pos._2 - entity.getPosition._2 + gp.player.screenY,
          obj.solidArea.width, obj.solidArea.height)
    )

    creatures.foreach (creature =>
      if creature != null then
        g.setColor(Color.YELLOW)
        g.drawRect(
          creature.solidArea.x + creature.pos._1 - entity.getPosition._1 + gp.player.screenX,
          creature.solidArea.y + creature.pos._2 - entity.getPosition._2 + gp.player.screenY,
          creature.solidArea.width, creature.solidArea.height)
    )
    creatures.foreach (creature =>
      if creature != null then
        g.setColor(Color.BLUE)
        g.drawRect(
          creature.areaHitBox.x + creature.pos._1 - entity.getPosition._1 + gp.player.screenX,
          creature.areaHitBox.y + creature.pos._2 - entity.getPosition._2 + gp.player.screenY,
          creature.areaHitBox.width, creature.areaHitBox.height)
    )
