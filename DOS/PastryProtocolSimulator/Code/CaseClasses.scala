case class Start
case class Stop
case class Error(id:String)
case class Restart(id:String)
case class DeadNode(id:String)
case class Avg(id:String,avg:Int)
case class Fault