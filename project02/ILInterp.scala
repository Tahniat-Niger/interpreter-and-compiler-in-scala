// ImpLang Interpreter
//
// Usage: linux> scala ILInterp <source file>
//
import ImpLang._

object ILInterp {
  case class InterpException(string: String) extends RuntimeException

  def interp(e:Expr, debug:Int = 0): Int = {
    val store = collection.mutable.Map[String,Int]()

    def interpE(e:Expr): Int = {
      if (debug > 1) {
        println("  expr = " + e);
        println("  store = " + store)
      }
      e match {
        case Num(n) => n
        case Var(x) => store.getOrElse(x,0) 
        case Add(l,r) => interpE(l) + interpE(r)
        case Sub(l,r) => interpE(l) - interpE(r)
        case Mul(l,r) => interpE(l) * interpE(r)
        case Div(l,r) => {
          val vl = interpE(l)
          val vr = interpE(r)
          if (vr == 0)
            throw InterpException("divide by zero")
          else
            vl / vr
        }
        case Rem(l,r) => {
          val vl = interpE(l)
          val vr = interpE(r)
          if (vr == 0)
            throw InterpException("divide by zero")
          else
            vl % vr
        }
       case Lt(l,r)  => {
        if(interpE(l) < interpE(r))
        return 1
        else
        return 0
       }  
        case Gt(l,r)  => {
        if(interpE(l) > interpE(r))
        return 1
        else
        return 0
       }  
        case Eq(l,r)  => {
        if(interpE(l) == interpE(r))
        return 1
        else
        return 0
       }  
       case If(c,t,e)  => {
        val v = interpE(c)
        if(v!=0)
        return interpE(t)
        else
        return interpE(e)
       }
      case Assgn(x,e) => {
       val v = interpE(e)
        store.put(x,v)
        return v              
     }
     case Write(e)   => {
      val v = interpE(e)
      println(v)
      return v      
       }
        case Seq(e1,e2) => {
          val v1 = interpE(e1)
          val v2 = interpE(e2)
          return v2

        }
        case While(c,b) => {
          var v = interpE(c)       
        
          while(v!=0){              
            val v1 = interpE(b)
             v = interpE(c)      
          }
          return 0
        }
       
        case For(x,e1,e2,e3) => {
        var v1 = interpE(e1)        
        store.put(x,v1)
        var t = true
        while(t == true){
           var v2 = interpE(e2)                  
           var vx = store.getOrElseUpdate(x,0) 
           
           if(vx<=v2) {
            interpE(e3)   
            var v4=store.getOrElse(x,0)
            v4=v4+1
            store.put(x,v4) 

           } else{
            t = false
           }
        }
        0
        
       }

      }
    }
 
    val v = interpE(e)
    if (debug > 0) println("Evaluates to: " + v)
    v
  } 

    
  
  def apply(s:String, debug:Int = 0): Int = {
    if (debug > 0) println("Input:  " + s)
    val e = ILParse(s)
    if (debug > 0) println("AST:    " + e)
    interp(e,debug)
  }

  // Test driver
  import scala.io.Source
  def main(argv: Array[String]) = {
    try {
      val s = Source.fromFile(argv(0)).getLines.mkString("\n")
      val d = if (argv.length > 1) argv(1).toInt else 0
      val r = apply(s,d)
      println(r)
    } catch {
      case ex: ParseException =>  println("Parser Error: " + ex.string)
      case ex: InterpException => println("Interp Error: " + ex.string)
    }
  }
}
//
