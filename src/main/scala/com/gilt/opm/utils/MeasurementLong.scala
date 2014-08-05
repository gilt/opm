package com.gilt.opm.utils

import java.util.concurrent.TimeUnit
import java.util.concurrent.TimeUnit._

import scala.language.implicitConversions

/**
 * A DSL to convert units of time. Uses TimeUnit under the hood, but is more usable.
 */
case class MeasurementLong(amount: Long, units: TimeUnit) {
  def in(convertTo: TimeUnit) = MeasurementLong(convertTo.convert(amount, units), convertTo)
}

object MeasurementLong {
  implicit def measurementLongToLong(x: MeasurementLong): Long = x.amount
}

class RichLongForMeasurement(val ref: Long) {
  def days = MeasurementLong(ref, DAYS)
  def hours = MeasurementLong(ref, HOURS)
  def minutes = MeasurementLong(ref, MINUTES)
  def seconds = MeasurementLong(ref, SECONDS)
  def milliseconds = MeasurementLong(ref, MILLISECONDS)
  def microseconds = MeasurementLong(ref, MICROSECONDS)
  def nanoseconds = MeasurementLong(ref, NANOSECONDS)
}

object RichLongForMeasurement {
  val days = DAYS
  val hours = HOURS
  val minutes = MINUTES
  val seconds = SECONDS
  val microseconds = MICROSECONDS
  val milliseconds = MILLISECONDS
  val nanoseconds = NANOSECONDS

  implicit def intToRichIntForMeasurement(x: Int): RichLongForMeasurement = new RichLongForMeasurement(x.toLong)
  implicit def longToRichIntForMeasurement(x: Long): RichLongForMeasurement = new RichLongForMeasurement(x)
}