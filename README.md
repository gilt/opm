
Object Prevalence Model (OPM)

OPM is a Scala library for managing the value of an object over time as a timeline of changes. It works either in-memory or durably atop MongoDB.

Let’s start with a simple example:

	package example

	import com.gilt.opm._

	trait User extends OpmObject {
  	  def name: String
	  def email: Option[String]
	}

OPM uses a pure trait to express a data model, and then implements the storage for that data model under the covers.  To create an instance of a User, you need to give it an ID, which is always a `String`:

	object Example extends App {

  	  import OpmFactory._

	  val user = instance[User]("ebowman").set(_.name).to("Eric Bowman")

	  val newUser = user.set(_.name).to("Eric Lennon Bowman")

	  newUser.timeline.foreach(println)
	}

Running this shows the complete history of this object:

	example.User(opmKey=ebowman,name=Eric Lennon Bowman)
	example.User(opmKey=ebowman,name=Eric Bowman)
	example.User(opmKey=ebowman,)

Note this includes the initial state of the object, before it was given a name, which might not be useful. You can avoid this by "pruning" the object to a given state, like:

	val user = instance[User]("ebowman").set(_.name).to("Eric Bowman").prune

After we call prune, this program prints:

	example.User(opmKey=ebowman,name=Eric Lennon Bowman)
	example.User(opmKey=ebowman,name=Eric Bowman)

The `OpmObject` trait provides some useful methods for understanding history and identity:

	final def opmKey: String = ""
	final def opmTimestamp: Long = 0L

The `opmKey` method returns the object's id, which gives the object an identity that persists across mutations of that object.

The `opmTimestamp` method returns the timestamp (in nanoseconds since the epoch) when this particular value was created.

For example,

	  val start = System.currentTimeMillis()
	  val user = instance[User]("ebowman").set(_.name).to("Eric Bowman").prune
	  Thread.sleep(1000)
	  val newUser = user.set(_.name).to("Eric Lennon Bowman")
	  newUser.timeline.foreach { user =>
	    println(s"user = $user, created ${user.opmTimestamp / 1000000 - start} ms after program start")
	  }
	}

Will print something like:

	user = example.User(opmKey=ebowman,name=Eric Lennon Bowman), created 1113 ms after program start
	user = example.User(opmKey=ebowman,name=Eric Bowman), created 112 ms after program start

Note that we need to divide the timestamp by 1000000 to convert it to the standard JVM "ms since the epoch".

You can also see in this example, that the head of the stream is the most recent version of the object, and you can traverse backwards in time through the stream.

To find the state of the object at some particular time, you might do something like:


This gives us a `user` instance with "no history," because we called `prune`. If we hadn't done that, then the object would hold the history of its creation (which is to say, first we had an empty object, followed by an object with a name set. That part of the history is probably not that interesting).

Objects in ppm are immutable, and they let you access their entire history. Let's look at an example:

	val modifiedUser = user.set(_.name).to("Eric Lennon Bowman")

Now we examine its "timeline", which holds the history of this object's evolution:

Suppose you wanted to find the value of an object two hours ago, you might write code like:

	import scala.concurrent.duration._
	val twoHoursAgo = System.currentTimeMillis() - 2.hours.toMillis
  
	newUser.timeline.dropWhile(_.opmTimestamp > twoHoursAgo).headOption

## OPM for builders
