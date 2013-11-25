
import java.io.IOException
import scala.actors.Actor
import scala.actors.Actor._
import scala.actors.OutputChannel
import scala.collection.mutable.ArrayBuffer
import scala.collection.{immutable, mutable}
import scala.util.Random
import scala.collection.mutable.HashMap

class ExceptionHandle(boss:Actor) extends Actor{
  
  var nodeHashMap=new HashMap[String,Actor]()
  
  def StatusReport(act:Actor,id:Int){
    println("Exception Handler for Gossip Protocal: Status Report")
    println("---------------------------------------------------")
    println("Boss Gossip Simulator: ")
    println("Error Node Status: "+ act.getState +" ID: "+id)
    println("---------------------------------------------------")

    //System.exit(1)
  }
   def act ={
    loop{
      react {
        case (id:String,node:Actor) =>
          //println("Exception from ID: "+id +" Actor: "+node)
          nodeHashMap+=(id->node)
        case Restart(id:String)=>
          boss!Restart(id)
        case Avg(id:String,avg:Int)=>
          //println("Avg msg: "+id)
          boss!Avg(id,avg)
        case Error(id:String)=>
          println("Error in Node ID: "+id)
          boss!Restart(id)
        case _ =>
         //notify all other relavtive ID with new addition 
          println("Unknown Error reached Error Handler")
          //exit()
        
          
      }
    }
  }
  
  
}
