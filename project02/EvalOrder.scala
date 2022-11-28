object EvalOrder{


def add(x: Int) : Any={
   // println("this is add")
    var s = 1+1
    return s


}
def sub(x: Int) : Any={
   // println("this is sub")
    var s = 1-1
    return s

}
 def sendvalue(a: =>Any) ={
    println("left to right")

}

def main(args: Array[String])={
sendvalue(add(1),sub(1))
} 
}
