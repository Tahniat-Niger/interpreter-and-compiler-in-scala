// ImpLang to RegIR Compiler
//
// Usage: linux> scala ILCompReg <source file>
//
import ImpLang._
import RegIR._

object ILComp {
  var nextLabel: Int = 100
  var nextTemp: Int = 0

  def compile(e:Expr): (Program,Src) = e match {
    case Num(i) => (Nil, Const(i))
    case Var(x) => (Nil, Name(x))
    case Add(e1,e2) => {
      val tmp = newTemp()                   
      val (p1,s1) = compile(e1)
      val (p2,s2) = compile(e2)
      val p = p1 ::: p2 ::: (Bop(AOP.Add,tmp,s1,s2) :: Nil)
      (p, Name(tmp))
    }
    case Sub(e1,e2) => {
      val tmp= newTemp()
      val (p1,s1) = compile(e1)
      val (p2,s2) =compile(e2)
      val p = p1 ::: p2 ::: (Bop(AOP.Sub,tmp,s1,s2) :: Nil)
      (p, Name(tmp))

    } 
    case Mul(e1,e2) => {
      val tmp= newTemp()
      val (p1,s1) = compile(e1)
      val (p2,s2) =compile(e2)
      val p = p1 ::: p2 ::: (Bop(AOP.Mul,tmp,s1,s2) :: Nil)
      (p, Name(tmp))

    }  
    case Div(e1,e2) => {
      val tmp= newTemp()
      val (p1,s1) = compile(e1)
      val (p2,s2) =compile(e2)
      val p = p1 ::: p2 ::: (Bop(AOP.Div,tmp,s1,s2) :: Nil)
     (p, Name(tmp))

    } 
    case Rem(e1,e2) => {
      val tmp= newTemp()
      val (p1,s1) = compile(e1)
      val (p2,s2) =compile(e2)
      val p = p1 ::: p2 ::: (Bop(AOP.Rem,tmp,s1,s2) :: Nil)
      (p, Name(tmp))

    } 

    case Lt(e1,e2) => {
      val tmp = newTemp()
      val lab = newLabel()
      val lab1 = newLabel()
      val lab2 = newLabel()
      val (p1,s1) = compile(e1) 
      val (p2,s2) = compile(e2)
      
      val p = p1 ::: p2 ::: (CJump(ROP.Lt,s1,s2,lab1) :: Nil) :::
      (Mov(tmp, Const(0)) :: Nil):::
      (Jump(lab2)::Nil):::
      (Label(lab1)::Nil):::
      (Mov(tmp, Const(1)) :: Nil):::
      (Jump(lab2)::Nil):::
      (Label(lab2)::Nil):::
      Nil

      (p, Name(tmp))
    }
    
    case Gt(e1,e2) => {
      val tmp = newTemp()
      val lab = newLabel()
      val lab1 = newLabel()
      val lab2 = newLabel()
      val (p1,s1) = compile(e1) 
      val (p2,s2) = compile(e2)
      val p = p1 ::: p2 :::  (CJump(ROP.Gt,s1,s2,lab1) :: Nil) :::
      (Mov(tmp, Const(0)) :: Nil):::
      (Jump(lab2)::Nil):::
      (Label(lab1)::Nil):::
      (Mov(tmp, Const(1)) :: Nil):::
      (Jump(lab2)::Nil):::
      (Label(lab2)::Nil):::
      Nil

      (p, Name(tmp))
    }
    case Eq(e1,e2) => {
      val tmp = newTemp()
      val lab = newLabel()
      val lab1 = newLabel()
      val lab2 = newLabel()
      val (p1,s1) = compile(e1) 
      val (p2,s2) = compile(e2)
      val p = p1 ::: p2 ::: (CJump(ROP.Eq,s1,s2, lab1) :: Nil) :::
       (Mov(tmp, Const(0)) :: Nil):::
      (Jump(lab2)::Nil):::
      (Label(lab1)::Nil):::
      (Mov(tmp, Const(1)) :: Nil):::
      (Jump(lab2)::Nil):::
      (Label(lab2)::Nil):::
      Nil

      (p, Name(tmp))
    }
  
    case If(c,t,f) => {
      val tmp = newTemp()   
      val lab1 = newLabel()
      val lab2 = newLabel()
      val lab3 = newLabel()
    
      val (pc,sc) = compile(c) 
      val (pt,st) = compile(t) 
      val (pf,sf) = compile(f)
      val p = pc ::: 
      (CJump(ROP.Gt,sc,Const(0), lab2) :: Nil) :::
      pf :::
      (Jump(lab3) :: Nil):::
      (Label(lab2) :: Nil):::
      pt :::
      (Label(lab3) :: Nil)


      (p, Name(tmp))
    }

    case Assgn(x,e) => {
       val (p1,s1) = compile(e)  
       val p = p1 ::: (Mov(x,s1) :: Nil)
      (p,Name(x))

    }

    case Write(e)   => {
      val (p1,s1) = compile(e)
     val p = (Print(s1) :: Nil)
      (p,s1)
     }

    case Seq(e1,e2) => {
      val (p1,s1) = compile(e1) 
      val (p2,s2) = compile(e2)
      (p1 ::: p2, s2)
    }

    case While(c,b) => {
      val lab1 = newLabel()
      val lab2 = newLabel()
      val lab3 = newLabel()    
      val (pc,sc) = compile(c)
      val (pb,sb) = compile(b)

      val p = (Label(lab2) :: Nil) :::
      pc ::: 
      (CJump(ROP.Eq,sc,Const(0), lab3) :: Nil) ::: 
      pb :::
      (Jump(lab2) :: Nil):::
      (Label(lab3) :: Nil)
      
      (p,Const(0))
    }

    case For(x,e1,e2,e3) => {
      val tmp = newTemp()
      val vx = newTemp()
      val x1 = newTemp()
      val lab = newLabel()
      val lab1 = newLabel()
      val lab2 = newLabel()
      val lab3 = newLabel()
      val (p1,v1) = compile(e1) 
      val (p2,v2) = compile(e2)
      val (p3,v3) = compile(e3)
      val p = p1 ::: (Mov(x,v1) :: Nil) ::: 
     (Label(lab2) :: Nil) :::
      p2  ::: 
      (CJump(ROP.Gt,Name(x),v2,lab3) :: Nil) ::: 
      p3 :::
      (Bop(AOP.Add,tmp,Name(x),Const(1)) :: Nil) :::
      (Mov(x,Name(tmp)) :: Nil) :::
      (Jump(lab2):: Nil) :::
      (Label(lab3) :: Nil)


      (p,Const(0))

    }
  }

  def newLabel() = {
    val next = nextLabel
    nextLabel = nextLabel + 1
    next
  }

  def newTemp() = {
    val next = "t" + nextTemp
    nextTemp = nextTemp + 1
    next
  }

  def apply(s:String, debug:Int = 0) = {
    if (debug > 0) println("Input:  " + s)
    val e = ILParse(s)
    if (debug > 0) println("AST:    " + e)
    nextLabel = 0
    val (pe,ve) = compile(e)
    val p = pe ::: (Return(ve) :: Nil)
    if (debug > 0) println("Target: " + p)
    exec(p,debug)
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
      case ex: ParseException => println("Parser Error: " + ex.string)
      case ex: ExecException =>  println("Exec Error: " + ex.string)
    }
  }
}
//
