package TwoLeveledCache

import com.typesafe.config.ConfigFactory
import org.slf4j.LoggerFactory

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

class CacheManager {

  val strategy: String = ConfigFactory.load.getString("strategy")

  val percentage: Double = ConfigFactory.load.getInt("percentage") / 100.0

  val memoryCap: Int = ConfigFactory.load.getInt("memoryCacheCapacity")

  val fileCap: Int = ConfigFactory.load.getInt("fileSystemCacheCapacity")

  val memoryCache: MemoryCache = new MemoryCache

  val fileSystemCache: FileSystemCache = new FileSystemCache

  private val loger = LoggerFactory.getLogger("CacheManager")

  def add(key: String, value: ValueType[_]): Unit = {

    strategy match {
      case "LFU" =>
        if (memoryCap > memoryCache.getAmount()) {
          memoryCache.add(key, value)
        } else if (fileCap > fileSystemCache.getAmount()) {
          fileSystemCache.add(key, value)
        } else {
          recache(key, value)
        }
      case "Medium" =>
        if (memoryCap > memoryCache.getAmount()) {
          memoryCache.add(key, value)
        } else if (fileCap > fileSystemCache.getAmount()) {
          fileSystemCache.add(key, value)
        } else {
          recache(key, value)
        }
    }
  }

  def get(key: String): Option[ValueType[_]] = {
    Try(memoryCache.get(key)) match {
      case Success(value) => Some(value)
      case Failure(_) =>
        Try(fileSystemCache.get(key)) match {
          case Success(value) => Some(value)
          case Failure(_) =>
            loger.info(s"Element with key: $key not found.")
            None
        }
    }
  }

  def remove(key: String): Unit = {
    Try(memoryCache.remove(key)) match {
      case Success(()) => ()
      case Failure(_) => fileSystemCache.remove(key)
    }
    loger.info(s"Element with key: $key found and deleted.")
  }

  def clear(): Unit = {
    memoryCache.clear()
    fileSystemCache.clear()
    loger.info("Cache cleared.")
  }

  def getCapacity: Int = {
    memoryCap + fileCap
  }

  def getAmount: Int = {
    memoryCache.getAmount() + fileSystemCache.getAmount()
  }

  @tailrec
  private def addAll(sortedData: Seq[(String, (Int, ValueType[_]))]): Unit = {
    if (sortedData.isEmpty) loger.info("Recache complete.")
    else if (memoryCap > memoryCache.getAmount()) {
      memoryCache.add(sortedData.head._1, sortedData.head._2._2)
      addAll(sortedData.tail)
    }
    else if (fileCap > fileSystemCache.getAmount()) {
      fileSystemCache.add(sortedData.head._1, sortedData.head._2._2)
      addAll(sortedData.tail)
    }
    else addAll(Seq.empty[(String, (Int, ValueType[_]))])
  }

  private def recache(key: String, value: ValueType[_]): Unit = {

    strategy match {
      case "LFU" =>
        loger.info("LFU strategy matched.")
        val sourceData = memoryCache.takeAll()
        sourceData ++= fileSystemCache.takeAll()
        val elem = (key, (1, value))
        val sourceDataSeq = sourceData.toSeq.sortBy(_._2._1)(Ordering.Int.reverse)
        val sortedData = Seq(elem) ++ sourceDataSeq
        addAll(sortedData.take((sortedData.size * percentage).toInt))

      case "Medium" =>
        loger.info("Medium strategy matched.")
        val medium = (memoryCache.frequencyOfKeys().values.sum + fileSystemCache.frequencyOfKeys().values.sum) / getAmount
        loger.info("Clear space in file system.")
        fileSystemCache.frequencyOfKeys().filter(map => map._2 < medium).
          foreach(freqMap => fileSystemCache.remove(freqMap._1))
        loger.info("Recaching memory.")
        memoryCache.frequencyOfKeys().filter(map => map._2 < medium).
          foreach(freqMap => if (fileSystemCache.getCapacity() - fileSystemCache.getAmount() > 1)
            fileSystemCache.add(freqMap._1, memoryCache.recacheObject(freqMap._1)))
        memoryCache.add(key, value)
    }
  }
}

