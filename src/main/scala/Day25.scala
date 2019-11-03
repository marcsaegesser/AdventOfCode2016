package advent

object Day25 {
  def part1(): Int = 0x16 + 0xAA  // By observing the pattern using streamFrom

  type Registers = Map[Char, Int]

  sealed trait Value { def show: String }
  case class Constant(c: Int)    extends Value { def show = c.toString }
  case class Register(r: Char) extends Value { def show = r.toString }

  sealed trait Instruction { def show: String }
  case class Cpy(src: Value, dest: Value) extends Instruction { def show = s"cpy ${src.show} ${dest.show}" }
  case class Inc(v: Value)                extends Instruction { def show = s"inc ${v.show}" }
  case class Dec(v: Value)                extends Instruction { def show = s"dec ${v.show}" }
  case class Jnz(v: Value, offset: Value) extends Instruction { def show = s"jnz ${v.show} ${offset.show}" }
  case class Tgl(v: Value)                extends Instruction { def show = s"tgl ${v.show}" }
  case class Out(v: Value)                extends Instruction { def show = s"out ${v.show}" }

  sealed trait MachineState
  case object Running extends MachineState
  case object Break   extends MachineState
  case object Stopped extends MachineState

  case class Machine(regs: Registers, output: Option[Int], ip: Int, state: MachineState, bps: List[Int], instructions: Vector[Instruction])

  def streamFrom(initial: Machine, a: Int): LazyList[Machine] =
    streamOutput(setReg(initial, 'a', a))

  def streamOutput(initial: Machine): LazyList[Machine] = {
    def loop(machine: Machine): LazyList[Machine] = {
      val next = runUntilOutput(machine)
      next #:: loop(next)
    }

    loop(initial)
  }

  def runUntilOutput(machine: Machine): Machine =
    if(machine.state != Running) machine
    else {
      val next = step(machine)
      if(next.output.isDefined) next
      else                      runUntilOutput(next)
    }

  def runMachine(machine: Machine): Machine = {
    if(machine.state != Running) machine
    else                        runMachine(step(machine))
  }

  def step(machine: Machine): Machine = {
    val next =
      machine match {
        case Machine(_, _, ip, Running, _, is) if !is.isDefinedAt(ip) => machine.copy(state=Stopped)
        case Machine(regs, _, ip, Running, _, is) =>
          is(ip) match {
            case Cpy(s, d) => machine.copy(regs=evalCpy(regs, s, d), output=None, ip=ip+1)
            case Inc(v)    => machine.copy(regs=evalInc(regs, v), output=None, ip=ip+1)
            case Dec(v)    => machine.copy(regs=evalDec(regs, v), output=None, ip=ip+1)
            case Jnz(v, o) => machine.copy(ip=evalJnz(regs, v, o, ip), output=None)
            case Tgl(v)    => machine.copy(instructions=evalTgl(regs, is, v, ip), output=None, ip=ip+1)
            case Out(v)    => machine.copy(output=Some(evalValue(regs, v)), ip=ip+1)
          }
        case Machine(_, _, o, Stopped, _, _)      => machine
        case Machine(_, _, o, Break, _, _)        => machine
      }
    if(next.bps.contains(next.ip)) next.copy(state=Break)
    else                           next
  }

  def stepAndShow(machine: Machine): Machine = {
    val m = step(machine)
    print(AnsiCodes.CursorHome)
    println(show(m))
    m
  }

  def stepAndShowN(machine: Machine, n: Int): Machine = {
    Thread.sleep(250)
    if(n == 0) machine
    else       stepAndShowN(stepAndShow(machine), n-1)
  }

  def continue(machine: Machine): Machine =
    if(machine.state == Break) machine.copy(state=Running)
    else                       machine

  def setReg(m: Machine, r: Char, v: Int): Machine =
    m.copy(regs=m.regs.updated(r, v))

  def setBreakPoint(m: Machine, addr: Int): Machine =
    m.copy(bps = addr +: m.bps)

  def clearBreakPoint(m: Machine, addr: Int): Machine =
    m.copy(bps = m.bps.filterNot(_==addr))

  def evalCpy(regs: Registers, src: Value, dest: Value): Registers =
    dest match {
      case Register(r) => regs + (r -> evalValue(regs, src))
      case Constant(_) => regs
    }

  def evalInc(regs: Registers, v: Value): Registers =
    v match {
      case Register(r) => regs.updatedWith(r) { _.map(_+1).orElse(Some(1)) }
      case Constant(_) => regs
    }

  def evalDec(regs: Registers, v: Value): Registers =
    v match {
      case Register(r) => regs.updatedWith(r) { _.map(_-1).orElse(Some(-1)) }
      case Constant(_) => regs
    }

  def evalJnz(regs: Registers, v: Value, o: Value, ip: Int): Int =
    if(evalValue(regs, v) == 0) ip + 1
    else                        ip + evalValue(regs, o)

  def evalTgl(regs: Registers, is: Vector[Instruction], v: Value, ip: Int): Vector[Instruction] = {
    val addr = ip + evalValue(regs, v)
    is.unapply(addr) match {
      case Some(Cpy(s, d)) => is.updated(addr, Jnz(s, d))
      case Some(Inc(v))    => is.updated(addr, Dec(v))
      case Some(Dec(v))    => is.updated(addr, Inc(v))
      case Some(Jnz(v, o)) => is.updated(addr, Cpy(v, o))
      case Some(Tgl(v))    => is.updated(addr, Inc(v))
      case Some(Out(v))    => is.updated(addr, Inc(v))
      case None            => is
    }
  }

  def evalValue(regs: Registers, v: Value): Int =
    v match {
      case Constant(c) => c
      case Register(r) => regs.getOrElse(r, 0)
    }

  def showRegs(regs: Registers): String =
    s"""a=${evalValue(regs, Register('a'))} b=${evalValue(regs, Register('b'))} c=${evalValue(regs, Register('c'))} d=${evalValue(regs, Register('d'))}"""

  def showInstructions(is: Vector[Instruction], ip: Int): String =
    is.zipWithIndex.map { case (i, a) =>
      val ind =
        if(a == ip) "->"
        else        "  "
      f"$ind$a%03d ${i.show}"
    }.mkString("\n")

  def show(m: Machine): String =
    s"${showRegs(m.regs)}  ip=${m.ip}\n${showInstructions(m.instructions, m.ip)}"

  val constRegex = """(-?\d+)""".r
  val regRegex   = """([a-d])""".r
  val cpyRegex   = """cpy ([a-d]|-?\d+) ([a-d])""".r
  val incRegex   = """inc ([a-d])""".r
  val decRegex   = """dec ([a-d])""".r
  val jnzRegex   = """jnz ([a-d]|-?\d+) ([a-d]|-?\d+)""".r
  val tglRegex   = """tgl ([a-d]|-?\d+)""".r
  val outRegex   = """out ([a-d]|-?\d+)""".r

  def parseValue(v: String): Value =
    v match {
      case constRegex(c) => Constant(c.toInt)
      case regRegex(r)   => Register(r.head)
    }
  def parseInstruction(l: String): Instruction =
    l match {
      case cpyRegex(s, d) => Cpy(parseValue(s), parseValue(d))
      case incRegex(r)    => Inc(Register(r.head))
      case decRegex(r)    => Dec(Register(r.head))
      case jnzRegex(r, o) => Jnz(parseValue(r), parseValue(o))
      case tglRegex(r)    => Tgl(parseValue(r))
      case outRegex(v)    => Out(parseValue(v))
    }

  def mkMachine(instructions: Vector[Instruction]): Machine =
    Machine(Map.empty[Char, Int], None, 0, Running, List.empty[Int], instructions)

  def readFile(f: String): Machine = {
    mkMachine(io.Source.fromFile(f).getLines().map(parseInstruction).toVector)
  }

  val inputFile = "data/Day25.txt"
}
