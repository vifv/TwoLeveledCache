package TwoLeveledCache

trait Cache {

  def add(key: String, value: ValueType[_])

  def get(key: String): ValueType[_]

  def remove(key: String)

  def clear()

  def getCapacity(): Int

  def getAmount(): Int
}