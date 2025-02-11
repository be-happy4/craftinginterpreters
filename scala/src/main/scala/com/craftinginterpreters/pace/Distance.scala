package com.craftinginterpreters.pace

case class Distance private(meters: BigDecimal):
  val kilometers: BigDecimal = meters / 1000

  override def toString: String = kilometers.toString()

object Distance:
  private def apply(x: BigDecimal): Distance = new Distance(x)

  def ofMeters(value: Int): Distance = ofMeters(BigDecimal(value))
  def ofMeters(value: Long): Distance = ofMeters(BigDecimal(value))
  def ofMeters(value: Double): Distance = ofMeters(BigDecimal(value))
  def ofMeters(value: BigDecimal): Distance = Distance(value)

  def ofKilometers(value: Int): Distance = ofKilometers(BigDecimal(value))
  def ofKilometers(value: Long): Distance = ofKilometers(BigDecimal(value))
  def ofKilometers(value: Double): Distance = ofKilometers(BigDecimal(value))
  def ofKilometers(value: BigDecimal): Distance = ofMeters(value * 1000)
