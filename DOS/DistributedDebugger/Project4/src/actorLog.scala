/*Author: Gaurab Dey
 * Desc: Contain the details of the independent Node Actors simulating sharing data for any
 * 		specific process flow. 
 * 
 */
import scala.actors.{OutputChannel, AbstractActor, Actor}, Actor._
import scala.actors.remote.RemoteActor, RemoteActor._
import scala.collection.mutable.ArrayBuffer
import scala.collection.{immutable, mutable}
import scala.util.Random
import java.io.IOException
import java.io._
import scala.xml._


class actorLog(id:Int,bossRef:Actor,size:Int) extends Actor {
  val boss=bossRef
  var rand=new util.Random
  val writer = new FileWriter(new File("./Logs/"+this+".xml" ),true)
  writeToFile("./Logs/"+this+".xml","<Log></Log>")
  val errorLog=new FileWriter(new File("./Logs/"+this.toString()+"_error.xml" ),true)
  writeToFile("./Logs/"+this+"_error.xml","<Issue></Issue>")
  var changeMsg=id.toString()
  
  def using[A <: {def close(): Unit}, B](param: A)(f: A => B): B =
	try { f(param) } finally { param.close() }
 
	def writeToFile(fileName:String, data:String) =
	  using (new FileWriter(fileName)) {
		fileWriter => fileWriter.write(data)
	  }
 
	def appendToFile(fileName:String, textData:String) =
	  using (new FileWriter(fileName, true)){
		fileWriter => using (new PrintWriter(fileWriter)) {
		  printWriter => printWriter.println(textData)
		}
	  }

	def prepLog(state:Int,key:String,cId:Int,nId:Int,desp:String):Elem={
		 if(state==0){
		 var s:Elem= <Flow Id={key}><Sid>{cId}</Sid><Did>{nId}</Did><Desp>{desp}</Desp></Flow>  
		 return s
		 }else{
		   if(state==1){
			   var s:Elem= <Start Id={key}><Sid>{cId}</Sid><Did>{nId}</Did><Desp>{desp}</Desp></Start>
			   return s
		   }else{
		     var s:Elem= <Error Id={key}><Sid>{cId}</Sid><Did>{nId}</Did><Desp>{desp}</Desp></Error>
		     return s
		   }
		 }     
	}
	
	
	def printLog(status:Int,key:String){
	  var doc=scala.xml.XML.loadFile("./Logs/"+this+".xml")
	  if(status==1){
		  doc=scala.xml.XML.loadFile("./Logs/"+this+"_error.xml")
		  println("Error Log for Node: "+id)
		  (doc \"Error").foreach { err=>
	    	if((err \ "@Id").toString()==key){
	    		println("Process Key: "+(err \ "@Id").text)
			  	println("Source  Process ID: "+(err \\ "Sid").text)
				println("Destination Process ID: "+(err \\ "Did").text)
				println((err \\ "Desp").text+"\n-----------")
	    	}		
		  }
	  }else{
		  println("Process ID: "+id+" Ref Code: "+this+" \n===============")
		  //println("Change details with Key: "+key)
		  (doc \"Flow").foreach { flow =>
	    		println("Process Key: "+(flow \ "@Id").text)
			  	println("Source  Process ID: "+(flow \\ "Sid").text)
				println("Destination Process ID: "+(flow \\ "Did").text)
				println((flow \\ "Desp").text+"\n-----------")
	    			
		  }
		}
	 }
	  
	      
	
	def addToLog(newNode: Node,root:Node):Node = root match {
       //match all the node from your model
       // and make a new one, appending old nodes and the new one
        case <Log>{oldNodes@_*}</Log> => 
          var log = <Log>{oldNodes}{newNode}</Log>
          return log
    }
	def addToError(newNode: Node,root:Node):Node = root match {
       //match all the node from your model
       // and make a new one, appending old nodes and the new one
        case <Issue>{oldNodes@_*}</Issue> => 
          var log = <Issue>{oldNodes}{newNode}</Issue>
          return log
    }
	
  def act={
    loop{
      react{
        case StartNode(key:String)=>
         // println("Starting: "+this)
          var nextId=rand.nextInt(size)
          while(nextId==id){
            nextId=rand.nextInt(size)
          }
          

          var s=prepLog(1,key,id,nextId,"Start Message: "+changeMsg)
          var log=addToLog(s,scala.xml.XML.loadFile("./Logs/"+this+".xml"))
          writeToFile("./Logs/"+this+".xml",log.toString())
          
          boss!BossMessage(key,id,nextId,changeMsg)
          
        case NodeMessage(key:String,sourceID:Int,msg:String)=> 
         println("Message Changed in Node: "+id)
          var s=prepLog(0,key,sourceID,id,"Message Changes-- old: "+changeMsg+" new: "+msg)
          //changeMsg=msg
          var log=addToLog(s,scala.xml.XML.loadFile("./Logs/"+this+".xml"))
          writeToFile("./Logs/"+this+".xml",log.toString())
          var nextId=rand.nextInt(size)
          while(nextId==id){
            nextId=rand.nextInt(size)
          }
          s=prepLog(0,key,id,nextId,"Message Changes-- old: "+changeMsg+" new: "+msg)
          changeMsg=msg
          log=addToLog(s,scala.xml.XML.loadFile("./Logs/"+this+".xml"))
          writeToFile("./Logs/"+this+".xml",log.toString())
                    
         // println("Node Message Case: "+msg)
          boss!BossMessage(key,id,nextId,changeMsg)
        case PrintLog(status:Int,key:String)=>
          printLog(status,key)
        
        case Stop(key:String)=>
          var s=prepLog(2,key,id,0,"Current Message Status-- "+changeMsg)
          var log=addToError(s,scala.xml.XML.loadFile("./Logs/"+this+"_error.xml"))
          writeToFile("./Logs/"+this+"_error.xml",log.toString())
          printLog(1,key)
          println("Message History\n------------")
          printLog(0,key)
          boss!Error(id)
          exit
          
        case _=> println("Default Case in Nodes.scala")
      }
    }
  }

}