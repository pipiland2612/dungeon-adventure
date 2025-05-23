package utils

import java.awt.image.BufferedImage

class Animation(val frames: Vector[BufferedImage], val frameDuration: Int, var attackStartFrame: Int = -1, var attackEndFrame: Int = -1) :
  private var currentTimeFrame: Int = 0
  private var frameCount: Int = 0
  private var finished: Boolean = false

  def getCurrentFrame = frames(currentTimeFrame)

  def update(): Unit =
    frameCount += 1
    if (frameCount >= frameDuration) then
      frameCount = 0
      currentTimeFrame = (currentTimeFrame + 1) % frames.length
      if currentTimeFrame == 0 then
        finished = true

  def reset(): Unit =
    currentTimeFrame = 0
    frameCount = 0
    finished = false

  def isInAttackInterval: Boolean = currentTimeFrame >= attackStartFrame && currentTimeFrame <= attackEndFrame
