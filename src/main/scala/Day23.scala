package advent

object Day23 {

  def day23(): Unit = {
    println(s"Day23.part1 = ${part1(readFile(inputFile))}")
    println(s"Day23.part2 = ${part2()}")
  }

  def part1(machine: Machine): Int = {
    runMachine(setReg(machine, 'a', 7)).regs('a')
  }

  def part2(): Long =
    (11 to 2 by -1).foldLeft(12L) { case (a, i) => a*i } + 86*77

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

  sealed trait MachineState
  case object Running extends MachineState
  case object Break   extends MachineState
  case object Stopped extends MachineState

  case class Machine(regs: Registers, ip: Int, state: MachineState, bps: List[Int], instructions: Vector[Instruction])

  def runMachine(machine: Machine): Machine = {
    if(machine.state != Running) machine
    else                        runMachine(step(machine))
  }

  def step(machine: Machine): Machine = {
    val next =
      machine match {
        case Machine(_, ip, Running, _, is) if !is.isDefinedAt(ip) => machine.copy(state=Stopped)
        case Machine(regs, ip, Running, _, is) =>
          is(ip) match {
            case Cpy(s, d) => machine.copy(regs=evalCpy(regs, s, d), ip=ip+1)
            case Inc(v)    => machine.copy(regs=evalInc(regs, v), ip=ip+1)
            case Dec(v)    => machine.copy(regs=evalDec(regs, v), ip=ip+1)
            case Jnz(v, o) => machine.copy(ip=evalJnz(regs, v, o, ip))
            case Tgl(v)    => machine.copy(instructions=evalTgl(regs, is, v, ip), ip=ip+1)
          }
        case Machine(_, _, Stopped, _, _)      => machine
        case Machine(_, _, Break, _, _)        => machine
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
    }

  def mkMachine(instructions: Vector[Instruction]): Machine =
    Machine(Map.empty[Char, Int], 0, Running, List.empty[Int], instructions)

  def readFile(f: String): Machine = {
    mkMachine(io.Source.fromFile(f).getLines().map(parseInstruction).toVector)
  }

  val inputFile = "data/Day23.txt"
  val testInput = "data/Day23-small.txt"
}

object AnsiCodes {
  // Device Status
  final val QueryDeviceCode = "\u001b[c"
  final val ReportDeviceCode = "\u001b[{code}0c"
  final val QueryDeviceStatus = "\u001b[5n"
  final val ReportDeviceOK = "\u001b[0n"
  final val ReportDeviceFailure = "\u001b[3n"
  final val QueryCursorPosition = "\u001b[6n"
//  final val ReportCursorPosition = """\u001b\[(\d+);(\d+)R""".r
  final val ReportCursorPosition = """.*\[(\d+);(\d+)R""".r

  // Terminal Setup
  final val ResetDevice = "\u001bc"
  final val EnableLineWrap = "\u001b[7h"
  final val DisableLineWrap = "\u001b[7l"

  // Cursor Control
  final def SetCursorPos(r: Int, c: Int) = s"\u001b[$r;${c}H"
  final val CursorHome = "\u001b[;H"
  final val CursorUp = "\u001b[{COUNT}A"
  final val CursorDown = "\u001b[{COUNT}B"
  final val CursorForward = "\u001b[{COUNT}C"
  final val CursorBackward = "\u001b[{COUNT}D"
  final val SaveCursor = "\u001b[s"
  final val UnsaveCursor = "\u001b[u"
  final val SaveCursorAttrs = "\u001b7"
  final val RestoreCursorAttrs = "\u001b8"

  // Erasing
  final val EraseEndofLine = "\u001b[K"
  final val EraseStartofLine = "\u001b[1K"
  final val EraseLine = "\u001b[2K"
  final val EraseDown = "\u001b[J"
  final val EraseUp = "\u001b[1J"
  final val EraseScreen = "\u001b[2J"
}
