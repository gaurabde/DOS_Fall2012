
import scala.actors.{OutputChannel, AbstractActor, Actor}, Actor._
import scala.collection.mutable.ArrayBuffer
import scala.collection.{immutable, mutable}


case class Start
case class StartNode(key:String)
case class Stop(key:String)
case class Error(id:Int)
case class StartMessage(msg:String)
case class BossMessage(key:String,id:Int,nextId:Int,msg:String)
case class NodeMessage(Key:String,sourceID:Int,msg:String)
case class Choice
case class PrintLog(id:Int,key:String)
case class Data(node:ArrayBuffer[Actor])

