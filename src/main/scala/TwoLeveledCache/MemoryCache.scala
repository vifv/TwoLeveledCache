package TwoLeveledCache

import com.typesafe.config.ConfigFactory


import scala.collection.mutable

class MemoryCache extends Cache {

  private val capacity = ConfigFactory.load.getInt("memoryCacheCapacity")
  private val cacheMap = mutable.HashMap.empty[String, ValueType[_]]
  private val frequencyMap = mutable.HashMap.empty[String, Int]

  override def add(key: String, value: ValueType[_]): Unit = {
    cacheMap += (key -> value)
    frequencyMap += (key -> 1)
  }

  override def get(key: String): ValueType[_] = {
    val frequency = frequencyMap(key)
    if (frequency > 0) frequencyMap.put(key, frequency + 1)
    cacheMap(key)
  }

  override def remove(key: String): Unit = {
    cacheMap.remove(key)
    frequencyMap.remove(key)
  }

  override def clear(): Unit = {
    cacheMap.clear()
    frequencyMap.clear()
  }

  override def getCapacity(): Int = capacity

  override def getAmount(): Int = cacheMap.size

  def frequencyOfKeys(): mutable.HashMap[String, Int] = frequencyMap

  def recacheObject(key: String): ValueType[_] = {
    val recachedObj: ValueType[_] = cacheMap(key)
    remove(key)
    recachedObj
  }

  def takeAll(): mutable.HashMap[String, (Int, ValueType[_])] = {
    val map = cacheMap.map(x => x._1 -> (frequencyMap(x._1), x._2))
    clear()
    map
  }

}
