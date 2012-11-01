package com.github.wesleybits

/* !# SICP Digitial Circuit Simulator
 This is a translation of section
 [3.3.4]
 (http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-22.html#%_sec_3.3.4) 
 of the SICP.  It's not quite as simple as the Scheme implementation, but I
 tried to conserve as much of the character of the origional example as I
 could.
 */

/* !## Wire
  Class Wire shows two methods, one to set the wire's state, and another
  to connect a component to the wire via callback procedures.
 */
class Wire {
  private var signal = false
  private var callbacks:List[() => Unit] = List.empty
  
  /*
    Sets the new signal value, and calls each of the
    callbacks if there is a change.
   */
  def setSignal(newSignal:Boolean) = {
    if (newSignal != signal) {
      signal = newSignal
      callbacks.foreach(f => f())
    }
  }
  
  /*
    Retreives the signal's value
   */
  def getSignal = signal

  /*
    Adds a callback to the callback list.
   */
  def add(f: => Unit) = {
    callbacks = (() => f) :: callbacks
    f
  }
}

/* !## Ordered Queue
  This is the ordered queue for the agenda's schedule.
 */
class OrderedQueue[A] {
  private var contents:List[Pair[Int,A]] = List.empty
  
  /* Lets us peek at the top element without messing up the queue */
  def peek:Pair[Int,A] = contents.head
  
  /* Actually removes the top element and returns it */
  def pop:Pair[Int,A] = {
    if (isEmpty) throw new Exception("POP on empty Queue.")
    else {
      val ret = contents.head
      contents = contents.tail
      ret
    }
  }

  /*
    Allows us to insert an element with the given rank.
    Elements are ordered by their rank.
   */
  def push(rank:Int,elt:A):Unit = {
    def iter(lst:List[Pair[Int,A]]):List[Pair[Int,A]] = {
      if (lst.isEmpty)
	List((rank,elt))
      else if (rank < lst.head._1) 
	(rank,elt) :: lst
      else
	lst.head :: iter(lst.tail)
    }
    contents = iter(contents)
  }

  /*
    Checks to see if the queue is empty to avoid poping from an
    empty queue.
   */
  def isEmpty = contents.isEmpty
}

/* And this is the agenda itself */
class Agenda {
  private var time = 0
  private val schedule = new OrderedQueue[() => Unit]

  /* Returns the time */
  def currentTime = time
  

  /* Inserts an action into the queue after a specified delay */
  def afterDelay(delay:Int)(action: => Unit) = {
    schedule.push(time + delay, () => action)
  }
  
  /*
    Here we roll through the whole actions queue until it's
    completely exhausted.
   */
  def propagate:Unit = {
    if (!(schedule.isEmpty)) {
      val top = schedule.pop
      time = top._1
      top._2()
      propagate
    } 
  }
}

/* !## The Probe
  So that we can check the value of a wire when it changes.
 */
object Probe {
  def apply(a:Agenda, name:String, w:Wire) = {
    w add {
      println(name + "\t@ " + a.currentTime + "\t=> " + w.getSignal)
    }
  }
}

/* !## And Gate
   Applies a logical and to it's outputs, and sends the result
   down it's output.
 */
object AndGate {
  val andDelay = 3
  def apply(agenda:Agenda, a:Wire, b:Wire, c:Wire) = {
    def f = {
      agenda.afterDelay(andDelay) {
	c.setSignal(a.getSignal && b.getSignal)
      }
    }
    a add { f }
    b add { f }
  }
}


/* !## Or Gate
 Applies a logical or operation to it's inputs and sends the result
 down it's output.
 */
object OrGate {
  val orDelay = 5
  def apply(agenda:Agenda, a:Wire, b:Wire, c:Wire) = {
    def f = {
      agenda.afterDelay(orDelay) {
	c.setSignal(a.getSignal || b.getSignal)
      }
    }
    a add { f }
    b add { f }
  }
}

/* !## Inverter
  Inverts it's input and pipes it down it's output.
 */
object Inverter {
  val inverterDelay = 2
  def apply(agenda:Agenda, in:Wire, out:Wire) = {
    in add {
      agenda.afterDelay(inverterDelay) {
	out.setSignal(!(in.getSignal))
      }
    }
  }
}

/* !## Half Adder
  As an example of a more complex circuit made simply.
 */
object HalfAdder {
  def apply(agenda:Agenda, a:Wire, b:Wire, sum:Wire, carry:Wire) = {
    val d = new Wire
    val e = new Wire
    OrGate(agenda, a, b, d)
    AndGate(agenda, a, b, carry)
    Inverter(agenda, carry, e)
    AndGate(agenda, d, e, sum)
  }
}

object main {
  /* !## Main
    The following program is the same as the one found in
    at the end of section 3.3.4 of the SICP.
   */
  def main(args:Array[String]) = {
    val theAgenda = new Agenda
    val input1 = new Wire
    val input2 = new Wire
    val sum    = new Wire
    val carry  = new Wire

    Probe(theAgenda, "Sum", sum)
    Probe(theAgenda, "Carry", carry)
    HalfAdder(theAgenda, input1, input2, sum, carry)
    println("-----End Setup-----------------\n")

    input1.setSignal(true)
    theAgenda.propagate
    println("-----End First Propagation-----\n")

    input2.setSignal(true)
    theAgenda.propagate
    println("-----End Second Propagation----")
    println("-----End Test------------------\n")
  }
}
