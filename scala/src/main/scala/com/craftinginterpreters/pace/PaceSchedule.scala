package com.craftinginterpreters.pace

import java.time.Duration

case class TimeTableItem private[pace](distance: Distance, time: Duration):
  override def toString: String =
    f"($distance: $time)"

case class TimeTable private[pace](table: List[TimeTableItem])

case class PaceSchedule(
  meters: Distance, expectedTime: Duration):
  private def seconds = expectedTime.getSeconds

  val speed: Speed = Speed(meters, expectedTime)

  def pace: Duration = speed.timePerKilometer

  def halfMarathonTimeTable: TimeTable =
    val milestones = (1 to 21).map(Distance.ofKilometers) appended Distance.ofKilometers(21.0975)
    val list = milestones
      .map(distance => TimeTableItem(distance, speed.getTime(distance)))
      .toList
    TimeTable(list)

  override def toString: String =
    f"""
      |speed(m/s)  : ${speed.meterPerSecond}
      |speed(km/h) : ${speed.kilometerPerHour}
      |pace(min/km): ${speed.timePerKilometer}""".stripMargin

object PaceSchedule:
  def main(args: Array[String]) =
    val distance = Distance.ofKilometers(21.0975)
    val expectedTime = Duration.ofHours(2).plusMinutes(45)
    val ps = PaceSchedule(distance, expectedTime)
    println(ps)
    ps.halfMarathonTimeTable.table.foreach(println)

