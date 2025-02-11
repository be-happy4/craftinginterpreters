package com.craftinginterpreters.pace

import java.time.Duration
import scala.math.BigDecimal.RoundingMode

case class Speed(private val originDistance: Distance, private val originTime: Duration):
  val meterPerSecond: BigDecimal = originDistance.meters / originTime.getSeconds
  val kilometerPerHour: BigDecimal = meterPerSecond * 3.6
  val timePerKilometer: Duration = Duration.ofSeconds((originTime.getSeconds / originDistance.kilometers).longValue)

  def getDistance(time: Duration): Distance =
    Distance.ofMeters(meterPerSecond * time.getSeconds)

  def getTime(distance: Distance): Duration =
    Duration.ofSeconds((distance.kilometers * timePerKilometer.getSeconds).longValue)

  override def toString: String =
    meterPerSecond.setScale(2, RoundingMode.DOWN).toString()
