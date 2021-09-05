package TwoLeveledCache

import com.typesafe.config.ConfigFactory

import java.io._
import java.nio.file.{Files, Path, Paths}
import scala.collection.mutable

class FileSystemCache extends Cache {

  private val capacity = ConfigFactory.load.getInt("fileSystemCacheCapacity")
  private val pathToSave = ConfigFactory.load.getString("pathToSave")
  var dir: Path = Files.createDirectory(Paths.get(pathToSave))
  val frequencyMap: mutable.HashMap[String, Int] = mutable.HashMap[String, Int]()

  override def add(key: String, value: ValueType[_]): Unit = {
    val objectWriter = new ObjectOutputStream(new FileOutputStream(pathToSave + "/" + key))
    objectWriter.writeObject(value)
    objectWriter.flush()
    objectWriter.close()
    frequencyMap += (key -> 1)
  }

  override def get(key: String): ValueType[_] = {
    val objectStream = new ObjectInputStream(new FileInputStream(dir + "/" + key))
    val result = objectStream.readObject.asInstanceOf[ValueType[_]]
    objectStream.close()
    val frequency = frequencyMap.getOrElse(key, 1)
    frequencyMap += (key -> (frequency + 1))
    result
  }

  override def remove(key: String): Unit = {
    val file = new File(dir + "/" + key)
    file.delete()
    frequencyMap.remove(key)
  }

  override def clear(): Unit = {
    frequencyMap.map(x => x._1 -> remove(x._1))
    frequencyMap.clear()
  }

  override def getCapacity(): Int = capacity

  override def getAmount(): Int = frequencyMap.size

  def frequencyOfKeys(): mutable.HashMap[String, Int] = frequencyMap

  def recacheObject(key: String): ValueType[_] = {
    val recachedObj: ValueType[_] = get(key)
    remove(key)
    recachedObj
  }

  def takeAll(): mutable.HashMap[String, (Int, ValueType[_])] = {
    val map = frequencyMap.map(x => x._1 -> (x._2, recacheObject(x._1)))
    clear()
    map
  }
}
