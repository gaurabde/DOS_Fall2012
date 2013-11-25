
import scala.actors.{OutputChannel, AbstractActor, Actor}, Actor._
import java.util.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.collection.{immutable, mutable}
import scala.util.Random
import java.io.IOException
import java.io._
import scala.xml._


object example {
  
	  
	def main(args :Array[String]) {
     try{
       
       clearDir("./Logs/")
       clearDir("./BossLog")
       val node=new NodeManager(this,50)
       node.start()
       node!Start
     }catch{
       case _=>println("Error in Main")
       }
     }
	
	def clearDir(dfile : String) : Unit = { 
			  for {
				  files <- Option(new File(dfile).listFiles)
				  file <- files if file.getName.endsWith(".xml")
			  } file.delete()
	} 
     
}

class NodeManager(ref:Any,no:Int) extends Actor{
  var nodeRef = new mutable.ArrayBuffer[Actor]()
  var nodeChange=new mutable.ArrayBuffer[Int]()
  var keyTracker=new mutable.ArrayBuffer[String]()
  var rand=new util.Random()
  val writer = new FileWriter(new File("./BossLog/"+this.toString()+".xml" ),true)
  writeToFile("./BossLog/"+this+".xml","<Log></Log>")
  val errorLog=new FileWriter(new File("./BossLog/"+this.toString()+"_error.xml" ),true)
  writeToFile("./BossLog/"+this+"_error.xml","<Issue></Issue>")
 // println("Boss Id: "+this)
  private var keyList:HashMap[String,Int] = new HashMap[String,Int]()
  var checker=0
  //var logger=new LoggerClass()
  
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
		 var s:Elem= <Flow Id={key}><PD><ID>{cId}</ID><Status>{nodeRef(cId).getState}</Status></PD><DD><ID>{nId}</ID><Status>{nodeRef(nId).getState}</Status></DD><Desp>{desp}</Desp></Flow>  
		 return s
		 }else{
		     var s:Elem= <Issue><Error Id={key}><Sid>{cId}</Sid><Did>{nId}</Did><Desp>{desp}</Desp></Error></Issue>
		     return s
		 }     
	}
	
	var count=0
	def printLog(status:Int,key:String){
	  var doc = scala.xml.XML.loadFile("./BossLog/"+this.toString()+".xml")
	  if(status==1){
	    doc=scala.xml.XML.loadFile("./BossLog/"+this.toString()+"_error.xml")
		  println("Error Log for Boss \n------")
		  (doc \"Error").foreach { err=>
	    	if((err \ "@Id").toString()==key){
	    		println("Process Key: "+(err \ "@Id").text)
			  	println("Source  Process ID: "+(err \\ "Sid").text)
				println("Destination Process ID: "+(err \\ "Did").text)
				println((err \\ "Desp").text+"\n-----------")
	    	}		
		  }
	  }else{
		  println("Boss Log Directory \n===============")
		  println("Process Flow details with Key: "+key)
		  (doc \"Flow").foreach { flow =>
		    	if((flow \ "@Id").toString()==key){
				  //println("Process Key: "+(flow \ "@Id").text + "\n")
		    	  if(count==0){
		    	    println("Root Process ID:"+(flow \\ "PD"\"ID").text)
		    	    println("Message Passed: "+(flow \\ "Desp").text+"\n-----------")
		    	    count+=1
		    	  }else{
		    	    println("Source Process ID: "+(flow \\ "PD"\"ID").text)
					println("Destination Process ID: "+(flow \\ "DD"\"ID").text)
					println("Message Passed: "+(flow \\ "Desp").text+"\n-----------")
		    	  }
						
				 }
			} 
	  }
	  count=0
	    
	}
	
	
	def addToModel(newNode: Node,root:Node):Node = root match {
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
	      case BossMessage(key:String,id:Int,nextId:Int,msg:String)=> 
	        //println("Reached Boss Message Case")
	        nodeChange+=nextId
	        keyTracker+=key
	        var s=prepLog(0,key,id,nextId,msg)
	        var doc = scala.xml.XML.loadFile("./BossLog/"+this+".xml")
	        var log=addToModel(s,scala.xml.XML.loadFile("./BossLog/"+this+".xml"))
	        writeToFile("./BossLog/"+this+".xml",log.toString())
	        //for (line <-scala.io.Source.fromFile("./BossLog/"+this+".xml").getLines){
	        	//println("line from Boss file: "+line)					
	        //}
	        keyList.put(key,keyList.get(key)+1)
	        if(keyList.get(key)==5){
	          checker+=1
	          println("Process flow completed (Key): "+key)
	          //printLog(1,key)
	          
	          //System.exit(0)
	        }else{
	          nodeRef(nextId)!NodeMessage(key,id,msg)
	        }
	        if(checker==4){
	          this!Choice
	        }
	        
	      case Start=>
	        writeToFile("./BossLog/"+this+".xml","<Log> </Log>")
	        //println("Reached NodeManger Start")
	        for(i<-0 until no){
	          var node=new actorLog(i,this,no)
	          node.start
	          //println("Node Created: "+node)
	          nodeRef+=node
	        }
	        for(i<-0 until 5){
		        var randNode=rand.nextInt(no)
		        nodeRef(randNode)!StartNode(nodeRef(randNode).toString())
		        keyList.put(nodeRef(randNode).toString(),0)
		       // println("Message send to Node : "+nodeRef(randNode))
		        
		        var s=prepLog(0,nodeRef(randNode).toString(),randNode,randNode,randNode.toString())
		        var log=addToModel(s,scala.xml.XML.loadFile("./BossLog/"+this+".xml"))
		        writeToFile("./BossLog/"+this+".xml",log.toString())
	        
	        }
	        println("List of Process undergoing changes, use IDs for reference \n---------------------------")
	        
	        //for (line <-scala.io.Source.fromFile("./BossLog/"+this+".xml").getLines){
	        	//println("line from Boss file: "+line)					
	        //}
	        //exit
	        
	      
	      case Choice=>
	      	while(true){
			   println("Distributed Debugging Simulation")
		       println("================================")
		       println("1. Process Flow log details ")
		       println("2. Error Simulation and log details")
		       println("3. Log of any Process")
		       println("4. Exit")
		       println("Enter your choice: ")
		       var m=readLine().toString()
		      m match{
		        case "1"=>
		          var cond=true
		          println("Process Key List:")
		          var keys = (keyList.keySet()).toArray()
		          for(i<-0 until keys.length) println((i+1)+". "+keys(i))
		          var m:Int=0
		          while(cond){
		        	  println("Enter Process Key ID (1-5): ")
		        	  m=readLine().toInt
		        	  if(m>0 && m<=5){
		        	    cond=false
		        	  }else println("Enter Corrent ID (1-5)")
		          }
		          println("========================")
		          println("Process Flow log details")
		          println("========================")
		          printLog(0,keys(m-1).toString())
		          	Thread.sleep(1000)
		          	
		        case "2"=>
		          println("========================")
		          println("Error Simulation and log details")
		          println("========================")
		          var randNode=rand.nextInt(nodeChange.length)
		          println("Termination Random Node ID: "+nodeChange(randNode)+" "+keyTracker(randNode))
		          println("Accessing Error Logs.")
		          for(i<-0 until 4){ print("."); Thread.sleep(1000);}
		          nodeRef(nodeChange(randNode))!Stop(keyTracker(randNode))
		          var s=prepLog(1,keyTracker(randNode),randNode,randNode,randNode.toString())
		          var log=addToError(s,scala.xml.XML.loadFile("./BossLog/"+this+"_error.xml"))
		          writeToFile("./BossLog/"+this+"_error.xml",log.toString())
		          Thread.sleep(2000)
		          println("Status of the Process: "+nodeRef(nodeChange(randNode)).getState+"\n-------------")
		          	Thread.sleep(1000)
		          	
		        case "3"=>
		          var cond=true
		          var m:Int=0
		          while(cond){
		        	  println("Please Enter Process Id from the above Changed Message List for better log details")
		        	  println("Enter Process ID (1-50): ")
		        	  m=readLine().toInt
		        	  if(m>0 && m<=50){
		        	    cond=false
		        	  }else println("Enter Corrent ID (1-5)")
		          }
		          println("Accessing Process ID Logs.")
		          for(i<-0 until 4){ print("."); Thread.sleep(1000);}
		          println("========================")
		          nodeRef(m)!PrintLog(0,"")
		          Thread.sleep(3000)
		          		          	
		        case "4"=>
		          println("Exit Debugger Simulation")
		          System.exit(0)
		        case _=>
		          println("Please enter proper Simulation code")
		      }
	    	}
	      case Error(id:Int)=>
	        nodeRef(id).restart()
	      case _=> println("Default case in NodeManger")
	    }
	  }
	}
  
}

