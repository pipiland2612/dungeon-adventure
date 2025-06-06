package system.data

import scala.collection.mutable.ListBuffer

class DataStorage extends Serializable:

  // Player Stats
  var level: Int = 0
  var maxHealth: Int = 0
  var health: Int = 0
  var maxMana: Int = 0
  var mana: Int = 0
  var strength: Int = 0
  var dexterity: Int = 0
  var exp: Int = 0
  var nextLevelExp: Int = 0
  var coin: Int = 0
  // Player's position at last save point
  var playerPositionX: Int = 0
  var playerPositionY: Int = 0
  var currentMap: Int = 0
  var currentArea: String = ""

  // Player INVENTORY
  var itemNames : ListBuffer[String] = new ListBuffer[String]()
  var itemAmount : ListBuffer[Int] = new ListBuffer[Int]()

  var currentWeaponSlot: Int = 0
  var currentShieldSlot: Int = 0
  var currentProjectileSlot: Int = 0
  var currentLightSlot: Int = 0

  // MAP OBJECT
  var mapObjNames: Array[Array[String]] = _
  var mapObjX: Array[Array[Int]] = _
  var mapObjY: Array[Array[Int]] = _
  var mapObjLootNames: Array[Array[String]] = _
  var chestOpened: Array[Array[Boolean]] = _

end DataStorage