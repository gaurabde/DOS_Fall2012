import scala.actors.{OutputChannel, AbstractActor, Actor}, Actor._
import scala.actors.remote.RemoteActor, RemoteActor._
import scala.math._
import java.security.MessageDigest
import scala.collection.immutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.collection.{immutable, mutable}
import scala.util.Random

case class LeafID(ID:mutable.ArrayBuffer[String])
case class LeafNodes(nodes:mutable.ArrayBuffer[Actor])
case class RoutingID(ID:mutable.ArrayBuffer[String])
case class RoutingNodes(nodes:mutable.ArrayBuffer[Actor])
case class NeighborID(ID:mutable.ArrayBuffer[String])
case class NeighborNodes(nodes:mutable.ArrayBuffer[Actor])

class PastryTable(ID:mutable.ArrayBuffer[String],nodes:mutable.ArrayBuffer[Actor]) extends Actor{
  
  var nodeNos=ID.length
  //var count=0
  var IDList=new mutable.ArrayBuffer[String]()
  var NodesList=new mutable.ArrayBuffer[Actor]()
 
//LEAF TABLE Variables 
  var track=true
  var subCount=0
  var subCount2=0
  var leafSize=abs(ID.length*0.0075*2)
  if (leafSize<8) leafSize=8
  if (leafSize>100) leafSize=100
  
    
  def leafPredict(node:String){
    try{
      var count=0
      track=true
      //For Negative case
      if(node.charAt(0)=='-') subCount2=1
      else subCount2=0
      //
      //println("Starting Leaf Prediction for Node: "+node)
      count=0
	     while(track){
		      if(ID(count).toString()==node && count<=nodeNos-1){
		        count+=1
		        if(count>=nodeNos-1) track=false
		        //println("Same Value :" +count)
		        //continue
		      }else{
		        if(ID(count).charAt(0)=='-'){
		          subCount=1
		        }else subCount=0
		          
		       // println(ID(count).substring(0,4))
		        if(ID(count).toString().substring(subCount,subCount+3)==node.substring(subCount2,subCount2+3)){
		          IDList+=(ID(count))
		          NodesList+=nodes(count)
		         // println(ID(count))
		        }
		    	  if (count>=nodeNos-1 || IDList.length==leafSize){
		    		 // println("LeafNode threshold")
		    		  track=false
		    	  }else count+=1
	      
		      }
	    }
      count=0
    }catch{
      case e=>println("Exception Leaf Predication "+e.printStackTrace())
      case _=>println("Unknown Exception")
    }
    
  }
 // --------------ROUTING TABLE creation---------------------- 
  
 //ROUTING Variables
   //Limits to routing tables to avoide oversized node creation initially
  var routingSize=30
  //abs(ID.length*0.0075*6)
 // if (routingSize<8) routingSize=16
  //if (routingSize>100) routingSize=200
  
  def removeDups(s:String):String=
  {
    if ( s.length() <= 1 ) return s;
    if( s.substring(1).contains(s.substring(0,1)) ) return removeDups(s.substring(1));
    else return s.substring(0,1) + removeDups(s.substring(1));
  }


  def routingPredict(node:String){
  try{
   // println("Routing Predication: "+node)
	  track=true
      //println("Starting Routing Prediction for Node: "+node)
   //Removing Negative character for calculation and Generating Unique String (Size 4) for Routing Table creation
      if(node.charAt(0)=='-') subCount2=1
      var noDup=removeDups(node.substring(subCount2,node.length())).reverse.toString()
      //println(noDup)
      if(noDup.length()>4)noDup=noDup.substring(0,4)
    //  println("Unique string for Node:"+node +" is :"+noDup)
     
      var count=0
      var sub:String=noDup.substring(0,1)
      var subCheck=noDup.substring(0,1)
      var size:Int=1
      var length:Int=0
      var checksum=false
      var counter=0
      while(track){
        //count=0
       // while(true){
          if(ID(count).charAt(0)=='-') subCount=1 else subCount=0
          if(ID(count).substring(subCount,subCount+size)==subCheck && count+1<nodeNos){
            IDList+=ID(count)
            NodesList+=nodes(count)
            subCheck=node.substring(subCount2,subCount2+size)+sub
           // println("ROuting Details:"+ID(count)+" "+subCheck +" "+size)
            size+=1
            count=0
          }
          if(count>=nodeNos-1){
            counter+=1
            subCheck=node.substring(subCount2,subCount2+size)+sub
            size+=1
          //  println("countcase :"+subCheck+" "+size +" "+counter)
            count=0
          } 
          if(size>=noDup.length || counter==3){
            length+=1
    	    if(length<noDup.length) {
    	      sub=noDup.charAt(length).toString()
    	    }  
    	    subCheck=sub
    	   // println
    	    size=1
    	    count=0
          }
          if(length>3||IDList.length>routingSize){
            //println("LeafNode threshold")
            track=false
          }else count+=1
        //}
		
        
        }
		
	      count=0
	      subCount=0
	    
  }catch{
	      case e=>println("Exception Routing"+e.printStackTrace())
	      case _=>println("Unknown Exception Routing")
	    }
	    
	  }
  
//-------NEIGHBOR TABLE Creation-----------
  
  var neighborSize:Int=abs(ID.length*0.0075*2).toInt
  if (neighborSize<8) neighborSize=10
  if (neighborSize>100) neighborSize=50
  var rand=new util.Random
  
  def neighborPredict(node:String){
  try{
	  	//println("Neighbour Predication: "+node)
	  	for(i<-0 until neighborSize){
	  	  
	  	  var m=rand.nextInt(nodeNos-1)
	  	  IDList+=ID(m)
	  	  NodesList+=nodes(m)
	  	 // println(ID(m))
	  	  
	  	}
    
  
     }catch{
	      case e=>println("Exception Neighbour"+e.printStackTrace())
	      case _=>println("Unknown Exception Routing")
	    }
	    
	  }

 var IDcount=0
 // ACT Section
  def act ={
    loop{
      react {
        case node:String =>
          IDcount+=1
        //  println("Pastry Table Case"+node)
          leafPredict(node)
          routingPredict(node)
          neighborPredict(node)
        // println("PastryNodes for Node Hash: "+node)
          sender!(IDList,NodesList)
          IDList=new ArrayBuffer[String]()
          NodesList=new ArrayBuffer[Actor]()
         // if(IDcount==ID.length-1)
          //System.exit(0)
        case _ =>
         //notify all other relavtive ID with new addition 
          println("String reached"+notify)
          //exit()
        
      }
    }
  }
  
}