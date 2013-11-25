/*
 * Author: Gaurab Dey
 * Details: This code is simulated version of Gossip and Push-Sum algorithm
 * 			also include the exception handling as the bonus section
 */
 
import scala.actors.{OutputChannel, AbstractActor, Actor}, Actor._
import scala.actors.remote.RemoteActor, RemoteActor._
import scala.math._
import java.security.MessageDigest
import scala.collection.immutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.collection.{immutable, mutable}
import scala.util.Random
import java.io.IOException


object Project3 {
   def main(args :Array[String]) {
     try{
      println("Pastry Simulator Started")
      println("========================")
      
      //var nodeNo=args(0).toInt
     // var msgNo=args(1).toInt
      var falty=false
      //if(args(2)!=null){
        //falty=true
      //}
      
      var nodeNo=500
      var msgNo=8
      var pastSim=new PastrySimulator(nodeNo,msgNo)
      pastSim.start()
     // Thread.sleep(10000)
     
        pastSim!Start     
     }
	   catch{
	     case _=> println("Enter correct Parameters")
	   }
 }
}
class PastrySimulator(nodeNos:Int,nosMsg:Int) extends Actor{
  
	//println("PastrySim")
    var b=System.currentTimeMillis
    
    val ID=new mutable.ArrayBuffer[String]()
    var pastryNodes = new mutable.ArrayBuffer[Actor]()
    var msgCount=new mutable.ArrayBuffer[Int]()
    val digest = MessageDigest.getInstance("MD5")
    var count=0
    var check=true
    var statusCheck=false
    var avgCount=0
    var rand=new util.Random
    
  def act={
	  try{
	  
      for (i<-0 until nodeNos){
      val s:String=(i).toString()//+"192.168.2.1"
      val nodeId= digest.digest(s.getBytes()).map("%02x".format(_)).mkString.toString.substring(0,8)
      var msg2:String=s.hashCode().toString()
      
      if(msg2.length()>8) msg2=msg2.substring(0,8)
      if(msg2.length()<8) while(msg2.length()<8) msg2=msg2+"0"
      ID+=nodeId
      
      var node=new PastryNode(nodeId,nodeNos,nosMsg)
      node.start()
      pastryNodes+=node
     
      
      //println(ID(i)+"  "+pastryNodes(i)+"  "+i)
    }
        
     
     var rout=new PastryTable(ID,pastryNodes)
     rout.start()
     println("Pastry table created: "+rout)
     var exp=new ExceptionHandle(this)
     exp.start()
     for(i<-0 until pastryNodes.length){
       pastryNodes(i)!exp
     }
     for(i<-0 until pastryNodes.length){
           	 pastryNodes(i)!rout
           	 //println("ID:"+i)
           	 Thread.sleep(5)

     }
     println("Nodes updated with Routing details. Time taken: "+(System.currentTimeMillis-b))
     b=System.currentTimeMillis
   
    loop{    
      react{
        case Start=>
          
          println("Triggering Nodes for Pastry Simulation ")//+t.toString().hash32())
      		//pastryNodes(2)!"Start"
      		//pastryNodes(3)!"Start"
      		//pastryNodes(100)!Start
      		for(i<-0 until pastryNodes.length){
     		  pastryNodes(i)!Start
     		  Thread.sleep(1000)
     		}
        
        case Restart(id:String)=>
          println("Pastry Node ID not reachable: "+id)
          while(check){
            if(ID(count)==id){
              check=false
              if(pastryNodes(count).getState==State.Terminated)
                pastryNodes(count).restart
              println("Node status check. "+ pastryNodes(count).getState)
              for(i<-0 until 3){
                print("..")
                Thread.sleep(5)
                if(pastryNodes(count).getState!=State.Terminated){
                  statusCheck=true
                }
              }              
            }else count+=1
            if(count>=pastryNodes.length-1) check=false
          }
          
          if(statusCheck) println("Node reachable: "+id)
          else{
            println("Node Permanently disconnected updating other nodes: "+id)
            for (i<-0 until pastryNodes.length){
              pastryNodes(i)!id
            }
          }
          check=true
          statusCheck=false
        case Avg(id:String,avg:Int)=>
          avgCount+=avg
          count+=1
          //println("AvgCount from node:"+id+" : "+avg)
          if(count>nodeNos-1){
            println("Average Hop count: "+(avgCount)/(nodeNos))//+" count:"+count)
            println("Time taken: "+(System.currentTimeMillis-b))
            System.exit(0)
          }
          
        case Fault=>
          println("Three random node will be Terminated/Suspended")
          for(i<-0 until 2){
            var r=(rand.nextInt(pastryNodes.length))
            pastryNodes(r)!Stop
          }
        case _=> println("")
      }
    }
  }catch{
    case e:IllegalArgumentException=>println(e.printStackTrace())
    
  }
 }
}

