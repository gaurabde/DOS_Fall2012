// Author: Gaurab Dey
// UFID: 7802-9140
// Email: gaurabde@ufl.edu/gdey@cise.ufl.edu
// 

//package actorproject1

import java.io.IOException
import scala.actors.Actor
import scala.actors.Actor._
//import scala.actors.OutputChannel
import scala.collection.mutable.ArrayBuffer
import scala.collection.{immutable, mutable}


//Generic case for all ActorData class completetion tracking
case object addDone
case object stopAll
case class WorkerTracker(addInfo:AddData,subBoss:Actor)

// AddData Actor for chunck data creation

class AddData(val starting:Int,val end:Int,val seqLength:Int) extends Actor{
  var tracker=false
  def act() = {
    loop {
      react {
        case addDone => tracker = true; println("Actor killed "+this);exit
      }
    }
  }
}

// Worker for square root calcution and addition in a particular range
class Worker extends Actor{
    var perSqr= List[Int]()
    var s: Double=0
    var i: Int=1
    var v: Boolean=true
  
  def sqrt(a:Double) = {
    val acc = 1e-10
    def findroot( x:Double ):Double = {
    val nextx = (a/x + x)/2
    if( (x-nextx).abs < acc*x ) nextx else findroot(nextx)
    } 
    findroot( 1 )
  }
  
  def conAdd(addInfo: AddData) { 
    //println(addInfo)
    var max= addInfo.seqLength
    var start=addInfo.starting
    var end=addInfo.end
    
    for(m<-start to (end-max+1)) {
      i=m
      for(n<-m until m+max){
      s+=(n*n)
      //println("s: "+s +" n: "+n)
      }
      //println("  i:" +i)
          v=(sqrt(s)%1==0)
          if (v) perSqr::=i     
      s=0
    } 
  }
  
  def act(){
    println("Worker Act")
    loop {
      receive {
        case WorkerTracker(addInfo,subBoss) => {
            //println("Reached worker: "+this)
            conAdd(addInfo)
           // println("After Conadd")
               //reply(i)
               subBoss!perSqr
              // subBoss!(perSqr(0)+addInfo.seqLength)
            //println(reply(perSqr))
          }
        case stopAll => {
            println("Exit Worker")
            exit()
        }
          
      }
    }
  }
}

// SubBoss for controling and creation for Workers and AddData chunks
class SubBoss(bRange:Int, bSeqLength:Int) extends Actor {
  //For storing Worker and AddData actors list  
  private val workerList=new mutable.ArrayBuffer[Worker]()
  private val actorList=new mutable.ArrayBuffer[AddData]()
  var sqrList=List[Int]()
  var sum = 0
  
 
//logic for creation of n mberof AddData Actors

    val noActors=bRange/bSeqLength
    var starting=1
    var ending=bRange
    var max=bSeqLength
    var tracker:Boolean=true
    var divFactor=0
    if ((bRange/bSeqLength)> 500) divFactor=20
    else divFactor=10
    
    Thread.sleep(200)
    
    try{
    while(tracker) {
      if(starting<ending && (starting+(divFactor*max))>ending){
        val newActors=new AddData(starting,ending,max)
        newActors.start
        actorList+=newActors
        var m=(10*max)
        //println(newActors +" "+starting+" "+ending +" "+(bRange/(divFactor*max))+" "+ m)
        tracker=false
      }else{
        val newActors=new AddData(starting,starting+(divFactor*max),max)
        val m=starting+(divFactor*max)
        newActors.start
        actorList+=newActors
        //println("secondActor: "+newActors+ " "+starting+" " +m)
        starting=starting+(divFactor*max)-1
      }
    }
    }
    catch{
     case ioe: IOException=>  
    }
 
  //Logic for number of Worker creation
    var noActor=actorList.length
    var workerCount:Int=noActor/6
    if (workerCount<=6) workerCount=6
    if (workerCount>20) workerCount=10
    //println(workerCount)

    var i=0
    for(addworker<-0 until workerCount){
      val newWorker=new Worker()
      newWorker.start
      workerList+=newWorker
      i+=1
      //println(newWorker+" "+i)
    }
    var actorCount=noActor
    i=0
    for (addActor<-actorList){
      actorCount-=1
      if(actorCount==actorList.length/2) Thread.sleep(100)
      if(i%2==0){        
        workerList(i) ! (WorkerTracker(addActor,this))
        i+=1
        //println("out of bound: "+i)
      }else {
        workerList(i)!(WorkerTracker(addActor,this))
        i+=1
       // println("out of bound: "+i)
      }
      if(i>=workerList.length) i=0
    }
 
  
  def checkActorState{
   for (i<-0 until actorList.length){
     if(actorList(i).getState==State.Suspended) {
      print(actorList(i))
      actorList(i)!stopAll
     }
    }
  }
  var wCounter=0
  var wCheck=true
  def checkWorkerState(){
    for(i<-0 until workerList.length){
      if(workerList(i).getState==State.Suspended){
          wCounter+=1 
          if(wCounter==workerList.length) wCheck=false; return;
      }else return
        
    }
  }
  var counter=0
  //var workerControl=bRange/(bSeqLength*divFactor)
  
  def act = {
    //println("Entered Boss ")
    
  while(true) {
      receive{
       case sqrList1:List[Int] => {
           println(sqrList1)
           sqrList++=sqrList1
           println("Final List :"+sqrList.distinct.sorted +" counter :"+ counter)
           counter+=1
           //checkActorState
           //System.exit(0);
           //exit;
           }
        case inte:Boolean =>{            
            if(wCheck){
              checkWorkerState()
              if(!wCheck){
                var len=0
                //println("Final List: ")
                for(len<-sqrList){
                  print(sqrList(len)+" ")
                }
                println()
                System.exit(0)
              }
            }
            }
        }
     }  
    }
  
}

// application

object project1 {
 def main(args: Array[String]): Unit = {
    println("Hello, world!")
    val range=args(0).toInt
    val seqLength=args(1).toInt
    println(range+" "+seqLength)
    val subBoss=new SubBoss(range,seqLength)
    subBoss.start()
  
    while(true){
      println("Worker check")
      val resultList=subBoss!true
      Thread.sleep(8000)
   }
    
  }
}