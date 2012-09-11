package com.gilt.opm

import com.mongodb.casbah._
import com.mongodb.casbah.Implicits._
import com.mongodb.casbah.commons.Implicits.wrapDBObj
import com.mongodb.casbah.commons.MongoDBObject
import com.mongodb.DBObject
import java.util.{UUID, Date}

/**
 * Mixing to provide mongo storage for OpmObjects.
 *
 * Implementers of the trait must supply a Mongo Collection instance, and may optionally
 * override the default wavelength.
 *
 * Should be able to store any OpmObject instance, and whatever that instance aggregates,
 * as long as Casbah can persist it as-is (so, maybe needs to implement DBObject?). I
 * suspect we'll need to revisit that as we use real objects. The casbah documentation
 * is pretty weak as to how it relates to the DBObject support in the java driver.
 *
 * @author Eric Bowman
 * @since 8/22/12 12:18 PM
 */

trait OpmMongoStorage extends OpmStorage {

  import OpmFactory._
  import OpmIntrospection.{TimestampField, ClassField, MetaFields}
  import OpmMongoStorage._

  def collection: MongoCollection

  // if you want automatic storage of nested object, override this
  def nestedToStorage(clazz: Class[_]): Option[OpmStorage] = Some(this)

  def wavelength: Int = 5           // value frame + (wavelength - 1) diff frames
  def toMongoMapper: Option[PartialFunction[(String, Option[Class[_]], AnyRef), AnyRef]] = None
  def fromMongoMapper: Option[PartialFunction[(String, Option[Class[_]], AnyRef), AnyRef]] = None

  private [this] lazy val defaultToMongoMapper: PartialFunction[(String, Option[Class[_]], AnyRef), AnyRef] = {
    case (f, _, s) if s.isInstanceOf[String] => s
    case (f, _, d) if d.isInstanceOf[Date] => d
    case (f, _, u) if u.isInstanceOf[UUID] => u
    case (f, _, n) if n == None => None
    case (f, optFieldClass, some) if some.isInstanceOf[Some[_]] => Some(mapToMongo(f, optFieldClass, some.asInstanceOf[Option[AnyRef]].get))
    case (f, _, o) if o.isInstanceOf[OpmObject] =>
      val proxy = OpmFactory.recoverModel(o.asInstanceOf[OpmObject])
      val builder = MongoDBObject.newBuilder
      builder += "_nested_opm_" -> true
      builder += Classname -> proxy.clazz.getName
      builder += Timestamp -> proxy.timestamp
      builder += Key -> proxy.key
      nestedToStorage(proxy.clazz).foreach{
        storage =>
          storage.maybePut(o.asInstanceOf[OpmObject])(Manifest.classType(Class.forName(proxy.clazz.getName)))
      }
      builder.result()
  }

  private [this] lazy val defaultFromMongoMapper: PartialFunction[(String, Option[Class[_]], AnyRef), AnyRef] = {
    case (_, _, s) if s.isInstanceOf[String] => s
    case (_, _, d) if d.isInstanceOf[Date] => d
    case (_, _, u) if u.isInstanceOf[UUID] => u
    case (_, _, n) if n == None => None
    case (field, fieldClassOpt, some) if some.isInstanceOf[Some[_]] => Some(mapFromMongo(field, fieldClassOpt, some.asInstanceOf[Some[_]].get))
    case (field, fieldClassOpt, o) if o.isInstanceOf[DBObject] && o.asInstanceOf[DBObject].get("_nested_opm_") == true =>
      val mongoDbObject = wrapDBObj(o.asInstanceOf[DBObject])
      val className = mongoDbObject.as[String](Classname)
      val timestamp = mongoDbObject.as[Long](Timestamp)
      val key = mongoDbObject.as[String](Key)
      val clazz = Class.forName(className)
      val loadedOpt: Option[OpmObject] = nestedToStorage(clazz).map {
        storage =>
          storage.get(key)(Manifest.classType(clazz))
      }.getOrElse {
        sys.error("Could not find an OpmStorage instance for class %s".format(clazz))
      }
      loadedOpt.map {
        opm =>
          // do we know to adapt to a particular return type?

          val finalOpm = opm.timeline.find(_.timestamp == timestamp).getOrElse {
              sys.error("Could not load an object(%s, %s) with timestamp %s".format(className, key, timestamp))
          }

          val result = fieldClassOpt.map {
            case option if option.isAssignableFrom(classOf[Option[_]]) =>
              Some(finalOpm)
            case normal if normal.isAssignableFrom(finalOpm.getClass) =>
              finalOpm
            case unknown =>
              sys.error("Cannot assign %s to field %s with type %s".format(finalOpm, field, unknown))
          }.getOrElse {
            sys.error("Could not load an object(%s, %s) with timestamp %s".format(className, key, timestamp))
          }
          result
      }.getOrElse {
        sys.error("Could not figure out how to load (%s, %s, %s)".format(field, fieldClassOpt, o))
      }
  }

  private [this] lazy val identity: PartialFunction[(String, Option[Class[_]], AnyRef), AnyRef] = { case x => x._3 }

  private [this] def mapToMongo(field: String, fieldType: Option[Class[_]], value: Any): Any = {
    value match {
      case ref: AnyRef =>
        (toMongoMapper.map(_ orElse defaultToMongoMapper orElse identity).getOrElse(defaultToMongoMapper orElse identity))(field, fieldType, ref)
      case anyVal =>
        anyVal
    }
  }

  private [this] def mapFromMongo(field: String, fieldType: Option[Class[_]], value: Any): Any = {
    fromMongoMapper.map(_ orElse defaultFromMongoMapper orElse identity).getOrElse(defaultFromMongoMapper orElse identity)(field, fieldType, value.asInstanceOf[AnyRef])
  }

  private [this] val sortFields = MongoDBObject(Timestamp -> -1, Type -> 1)

  override def put[V <: OpmObject](obj: V)(implicit mf: Manifest[V]) {
    val model: OpmProxy = recoverModel(obj)
    if (collection.findOne(MongoDBObject(Key -> model.key)).isEmpty) {
      create(model)
    } else {
      update(model)
    }
  }


  // writes the model to the database.
  private [this] def create(model: OpmProxy)(implicit mf: Manifest[OpmObject]) {
    val history = (model #:: model.history)
    if (history.size > 1) {
      history.zip(history.tail).foreach(r => require(r._1.timestamp != r._2.timestamp, "Equal timestamps: %s".format(r)))
    }
    writeWavelets(model.key, history)
  }

  // updates the database with the latest changes to the object.  Assumes an object with this key has already
  // been passed to the create method.
  private [this] def update(model: OpmProxy)(implicit mf: Manifest[OpmObject]) {
    // This is hard. I have a picture that might help explain this, but expect to invest some time
    // forming the mental model if you really want to understand this.
    val curStream = model #:: model.history
    val mongoStream = collection.find(MongoDBObject(Key -> model.key)).sort(sortFields).toStream.map(wrapDBObj(_))
    val lastFrame  = mongoStream.take(wavelength)
    require(!lastFrame.isEmpty, "No mongo records found for key %s; did you create first?".format(model.key))
    val oldPhase = (wavelength + lastFrame.takeWhile(_.as[String](Type) == DiffType).size) % wavelength
    val updateSize = curStream.takeWhile(_.timestamp > lastFrame.head.as[Long](Timestamp)).size
    val startPhase = (wavelength - (updateSize % wavelength) + oldPhase) % wavelength
    val initialDiffCount = (wavelength - startPhase) % wavelength
    writeDiffs(model.key, curStream.zip(curStream.tail).take(initialDiffCount))
    writeWavelets(model.key, curStream.drop(initialDiffCount).take(updateSize - initialDiffCount))
  }

  // when we retrieve by a key, we always load back to the first value frame.  So that may mean
  // we load `wavelength` records, or it could mean that we load 1 record ... it all depends on what the
  // most recent record is.  Let's say that we have a wavelength of 5 and the tip is 2 diff records
  // then a value record. So we load the last 5 records so we are guaranteed to get a value record.
  // then we take the two diff records and the value record and assemble 3 fully-formed
  // OpmObjects.  We construct a stream so that if the user tries to look at past history
  // it can find and load the next wavelet. This strikes me as "complicated", and is an advanced
  // use of scala streams (?), and also has some memory risk.  We could obviate that possibly by
  // keeping softkeys and a mechanism to load on demand, but I guess we'll see.
  override def get[V <: OpmObject](key: String)(implicit mf: Manifest[V]): Option[V] = {
    val mongoStream = collection.find(MongoDBObject(Key -> key)).sort(sortFields).map(wrapDBObj(_)).toStream
    val initialDiffs = mongoStream.takeWhile(_.as[String](Type) == DiffType)
    mongoStream.dropWhile(_.as[String](Type) == DiffType).headOption.flatMap {
      lastValueObj =>
        val lastValue = toOpmProxy(key, lastValueObj)

        // We have to look forwards in time if there is a set of diffs right at the tip of the
        // mongo record stream; so special processing to assemble those from the first value object,
        // and the diffs that come after it in time.
        val initialObjs = if (initialDiffs.isEmpty) {
          Seq.empty
        } else {
          initialDiffs.reverse.foldLeft(Seq(lastValue)) {
            (objs: Seq[OpmProxy], dbObj: MongoDBObject) =>
              val changes: Set[Diff] = objToDiffSet(dbObj, Forward)
              OpmProxy(key, OpmFactory.evolve(objs.head.fields, changes)) +: objs
          }
        }

        // this is fairly magic, leveraging streams & recursion.
        // This defines a new stream which maps the stream of OpmProxy
        // instances (which are in turn being lazy-loaded from mongo)
        // into a stream of final objects. A final object is tricky,
        // since each instance needs a reference to the stream itself,
        // as its timeline. A lot of staring at the screen and scratching
        // my head were required to get this amazingly short, deep piece
        // of code in place.
        def assembleFinalObjects(stream: Stream[OpmProxy]): Stream[V] = {
          if (stream.isEmpty) {
            Stream.empty
          } else {
            lazy val tail = assembleFinalObjects(stream.tail)
            val head = stream.head.copy(history = tail.map(OpmFactory.recoverModel(_)))
            OpmFactory.newProxy(head).asInstanceOf[V] #:: tail
          }
        }

        assembleFinalObjects(initialObjs.toStream #::: loadStream(key, lastValue, mongoStream.drop(initialObjs.size))).headOption
    }
  }


  // deletes all records with the given key, doing nothing if the key doesn't exist.
  override def remove(key: String) {
    collection.remove(MongoDBObject(Key -> key))
  }

  private [this] def injectHistory(proxyStream: Stream[OpmProxy]): Stream[OpmProxy] = {
    if (proxyStream.isEmpty) {
      proxyStream
    } else {
      proxyStream.head.copy(history = proxyStream.tail) #:: injectHistory(proxyStream.tail)
    }
  }

  private [this] def loadStream(key: String, head: OpmProxy, cursorStream: Stream[MongoDBObject]): Stream[OpmProxy] = {
    cursorStream.headOption.map {
      prevObj =>
        if (prevObj.as[String](Type) == ValueType) {
          val prev = toOpmProxy(key, prevObj)
          prev #:: loadStream(key, prev, cursorStream.tail)
        } else {
          assert(prevObj.as[String](Type) == DiffType, "Unknown type: %s".format(prevObj))
          val changes: Set[Diff] = objToDiffSet(prevObj, Reverse)
          val prev = OpmProxy(key, OpmFactory.evolve(head.fields, changes))
          prev #:: loadStream(key, prev, cursorStream.tail)
        }
    }.getOrElse(Stream.empty)
  }

  private [this] def objToDiffSet(obj: MongoDBObject, direction: String): Set[Diff] = {
    require(direction == Forward || direction == Reverse,
      "direction must be either %s or %s; was %s".format(Forward, Reverse, direction))
    wrapDBObj(obj.as[DBObject](direction)).map(kv => Diff(kv._1.toString, Option(kv._2).map(v => mapFromMongo(kv._1.toString, None, v)))).toSet
  }

  private [this] def toOpmProxy(key: String, valueRecord: DBObject): OpmProxy = {
    require(valueRecord.get(Type) == ValueType, "Record was not value record: %s".format(valueRecord))
    val instance = wrapDBObj(valueRecord.get(Instance).asInstanceOf[DBObject])
    val fields = (for (key <- instance.keys) yield key -> instance(key)).toMap
    val record = wrapDBObj(valueRecord)
    val clazz = Class.forName(record.as[String](Classname))
    val timeStamp = record.as[Long](Timestamp)
    opmProxy(key, clazz, timeStamp, fields.map(kv => kv._1 -> mapFromMongo(kv._1, Some(clazz.getMethod(kv._1).getReturnType), kv._2)))
  }

  private [this] def opmProxy(key: String, clazz: Class[_], timeStamp: Long, fields: Map[String, Any]) = {
    OpmProxy(key, fields ++ Map(ClassField -> clazz, TimestampField -> timeStamp))
  }

  // given a sequence of phase=0 waves, writes them to the database
  private[this] def writeWavelets(key: String, stream: Seq[OpmProxy]) {
    stream.grouped(wavelength).foreach(writeWavelet(key, _))
  }

  // given a "wavelet" of models, write it to the database. this means
  // writing a single value record, followed by wavelength - 1 diff records
  private[this] def writeWavelet(key: String, models: Seq[OpmProxy]) {
    writeValue(key, models.head)
    if (models.size > 1) {
      writeDiffs(key, models.zip(models.tail))
    }
  }

  // writes a sequence of diff records. For each pair, the _1 member is expected to have
  // have been created after the _2 member
  private[this] def writeDiffs(key: String, pairs: Seq[(OpmProxy, OpmProxy)]) {
    pairs.foreach {
      (pair: (OpmProxy, OpmProxy)) =>
        require(pair._1.timestamp > pair._2.timestamp, "time ordering not maintained: %s".format(pair))
        writeDiff(key = key, later = pair._1, earlier = pair._2)
    }
  }

  private[this] def createId(key: Any, obj: OpmProxy, recordType: String): String = {
    require(recordType == ValueType || recordType == DiffType, "Unknown record type %s".format(recordType))
    "%s:%s:%s".format(key, obj.timestamp, recordType)
  }

  // writes a single bi-directional diff record
  private[this] def writeDiff(key: Any, later: OpmProxy, earlier: OpmProxy) {
    val forwardTimestamp = later.timestamp
    val reverseTimestamp = earlier.timestamp
    val forwardDiffs = diffModels(later, earlier)
    val reverseDiffs = diffModels(earlier, later)
    val builder = MongoDBObject.newBuilder
    builder += "_id" -> createId(key, later, DiffType)
    builder += Key -> key
    builder += Type -> DiffType
    builder += Timestamp -> reverseTimestamp
    builder += ForwardTimestamp -> forwardTimestamp

    // todo: encode the timestamp as a delta instead of an absolute value, to save some bytes
    val forwardDiffBuilder = MongoDBObject.newBuilder
    forwardDiffBuilder += (TimestampField -> later.timestamp)
    builder += Forward -> forwardDiffs.foldLeft(forwardDiffBuilder) {
      case (b, Diff(field, newValueType)) =>
        b += field -> mapToMongo(field, Some(later.fieldMethod(field).getReturnType), newValueType)
        b
    }.result()
    val reverseDiffBuilder = MongoDBObject.newBuilder
    reverseDiffBuilder += (TimestampField -> earlier.timestamp)
    builder += Reverse -> reverseDiffs.foldLeft(reverseDiffBuilder) {
      case (b, Diff(field, newValueType)) =>
        b += field -> mapToMongo(field, Some(later.fieldMethod(field).getReturnType), newValueType)
        b
    }.result()
    collection += builder.result()
  }

  // writes a single value record
  private def writeValue(key: Any, obj: OpmProxy) {
    val fields = obj.fields
    val builder = MongoDBObject.newBuilder
    builder += "_id" -> createId(key, obj, ValueType)
    obj.history.headOption.foreach {
      prev =>
        builder += PrevKey -> createId(key, prev, if (wavelength == 1) ValueType else DiffType)
    }
    builder += Key -> key
    builder += Type -> ValueType
    builder += Timestamp -> fields(TimestampField)
    builder += Classname -> fields(ClassField).asInstanceOf[Class[_]].getName
    builder += Instance -> fields.filterNot(f => MetaFields(f._1)).foldLeft(MongoDBObject.newBuilder) {
      case (b, f) =>
        b += f._1 -> mapToMongo(f._1, Some(obj.fieldMethod(f._1).getReturnType), f._2)
        b
    }.result
    collection += builder.result()
  }
}

object OpmMongoStorage {
  val Key = "k"
  val PrevKey = "p"
  val Timestamp = "ts"
  val ForwardTimestamp = "fts"
  val Classname = "c"
  val Instance = "i"
  val Forward = "f"
  val Reverse = "r"
  val Type = "t"

  // There are 2 types of record, a "ValueType" record and a "DiffType" record. One tricky thing is that we end up
  // with two records with the same timestamp when we store a value record: one to hold the class, and one to hold
  // the diff to the next node (assuming that ValueType record has saves after it).  So we actually depend on the fact
  // that "d" sorts before "v" when we fetch, ordering first by time stamp (descending) and then type (ascending)
  // in order to get a view that makes sense going backwards through time. This way we see the forward diff from
  // the value record the first diff after it, BEFORE we see the value record. Got it? Sorry, this is tricky stuff.
  // Turn back!
  val ValueType = "v"
  val DiffType = "d"
}
