import scala.actors.{OutputChannel, AbstractActor, Actor}, Actor._
import scala.actors.remote.RemoteActor, RemoteActor._
import scala.math._
import java.security.MessageDigest
import scala.collection.immutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.collection.{immutable, mutable}
import scala.util.Random
import java.io.IOException

class PastryNode(ID:String,size:Int,msgC:Int) extends Actor{
  var exp:Actor=null
  var IDList=new mutable.ArrayBuffer[String]()
  var nodesList=new mutable.ArrayBuffer[Actor]()
  var rand=new util.Random
  //var id:String=null
  val digest = MessageDigest.getInstance("MD5")
  var ii=0
  var msgCount=msgC
  var avgCount:Int=0
  var count:Int=0
  def routingTableCheck(id:String):Int={
    for(i<-0 until IDList.length) if(IDList(i)==id) return i
    
    var closestID=0
    for(i<-0 until IDList.length){
      if(IDList(i)=="0")  ii=0
      else{
      if(IDList(i)==id) return i
      else{
        if(IDList(i).charAt(0)==id.charAt(0)){
          closestID=i
          if(IDList(i).charAt(1)==id.charAt(1)){
            closestID=i
            if(IDList(i).charAt(2)==id.charAt(2)){
              closestID=i
              if(IDList(i).charAt(3)==id.charAt(3)){
                closestID=i
                if(IDList(i).charAt(4)==id.charAt(4)){
                  closestID=i
                  if(IDList(i).charAt(5)==id.charAt(5)){
                    closestID=i
                   if(IDList(i).charAt(6)==id.charAt(6)){
                     closestID=i
                   }
                  }
                }
              }
            }
          }
        }
       }
      }
    }
    return closestID
  }
  
  def act={
   try{
    loop{
      
      react{
        
        case expHandle:ExceptionHandle=>
          exp=expHandle
          //println("got exp details"+expHandle)
          exp!(ID,this)
        case pastryTable:PastryTable=>
           pastryTable!ID
          // println("Reached Pastry Node: "+ID + " "+pastryTable)
        
        case (id:mutable.ArrayBuffer[String],node:mutable.ArrayBuffer[Actor])=>
          id.copyToBuffer(IDList)
          node.copyToBuffer(nodesList)
          for (i<-0 until IDList.length) if(IDList(i)==ID) IDList(i)=0.toString()
          
         // println("------------Recieved ID and Node details: "+ID +" "+m.hashCode())
          
         // reply()
        
        case (idDest:String,hopCount:Int,dest:Actor)=>
         // println("Deliverd with hop:"+hopCount+" Destination: "+dest +" ID:"+id)
          if(idDest=="0"){
            count+=1
            avgCount+=hopCount
          }else{
	         // println("Updating the triggering Node with destination details: "+idDest +"Ref: "+dest)
	          IDList+=idDest
	          nodesList+=dest
	          //for(i<-0 until IDList.length) println(IDList(i))
	          count+=1
	          avgCount+=hopCount
          }
          if(count==msgC){
            avgCount=avgCount/count
            exp!Avg(ID,avgCount)
          }
         // Thread.sleep(1000)
          
        case (idDest:String,rootSender:Actor,hop:Int)=>
         // println("reached nextPoint "+ ID)
          if(idDest==ID) {
           //println("Reached Destination "+ID+" hopCount: "+hop +" Self Ref "+ this)
            rootSender!(ID,hop,this)
          }
          else{
            var check=idDest
            var nextID=routingTableCheck(check)
            //println("CheckList hop:"+hop+" "+nextID +" "+id)
           // for(i<-0 until IDList.length) println(IDList(i))
            //Thread.sleep(10)
            if(hop>20) {
            	//println("Looped issues")
            	rootSender!("0",8,this)
            }
            else
            nodesList(nextID)!(idDest,rootSender,hop+1)
          }
        case Start=>
          //println("ID :"+ID)
         	for(time<-0 until msgCount){
          	var r=(rand.nextInt(size-1)).toString()
          //	println("Triggering Node with String: "+r)
          	var id=digest.digest(r.getBytes()).map("%02x".format(_)).mkString.toString.substring(0,8)
         // id=(r).hashCode().toString()
        	  if(id.length>8) id=id.substring(0,8)
         	// if(id.length()<8) while(id.length()<8) id=id+"0"
         	// println("Random ID from PastryNode: "+id +" "+r)
          
         	 self! (id,self,0)
          		Thread.sleep(1000)
          }
      
        case Stop=>
          exp!Restart(ID)
          exit
    //Terminated Node updates from Boss to all nodes    
        case id:String=>
          for(i<-0 until IDList.length){
   //Node Removed from List to avoid error Reference 
            if(IDList(i)==id){
              IDList(i)="0"
              nodesList(i)=null
            }
          }
        case _=> println("Pastry Node: enter correct parameters --PastryID" +this)
      }
     }
    
   }catch{
    	case e:IllegalArgumentException=>
	    	println("Exception Leaf Predication "+e.printStackTrace())
	        exp!Error(ID)
    	case r:RuntimeException=>println("Unknown Exception")
   }
  }
  
}
