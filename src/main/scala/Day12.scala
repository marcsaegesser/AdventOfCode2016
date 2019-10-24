package advent

object Day12 {

  def day12(): Unit = {
    val machine = readFile(inputFile)
    println(s"Day12.part1 = ${part1(machine)}")
    println(s"Day12.part2 = ${part2(machine)}")
  }

  def part1(machine: Machine): Int =
    runMachine(machine).regs('a')

  def part2(machine: Machine): Int = {
    runMachine(machine.copy(regs = machine.regs.updated('c', 1))).regs('a')
  }

  type Registers = Map[Char, Int]

  sealed trait Value
  case class Constant(c: Int)    extends Value
  case class Register(r: Char) extends Value

  sealed trait Instruction
  case class Cpy(src: Value, dest: Register)    extends Instruction
  case class Inc(r: Register)                   extends Instruction
  case class Dec(r: Register)                   extends Instruction
  case class Jnz(r: Value, offset: Constant) extends Instruction

  sealed trait MachineState
  case object Running extends MachineState
  case object Stopped extends MachineState

  case class Machine(regs: Registers, ip: Int, state: MachineState, instructions: Vector[Instruction])

  def runMachine(machine: Machine): Machine = {
    if(machine.state == Stopped) machine
    else                        runMachine(step(machine))
  }

  def step(machine: Machine): Machine = {
    machine match {
      case Machine(_, ip, Running, is) if !is.isDefinedAt(ip) => machine.copy(state=Stopped)
      case Machine(regs, ip, Running, is) =>
        is(ip) match {
          case Cpy(s, d) => machine.copy(regs=evalCpy(regs, s, d), ip=ip+1)
          case Inc(r)    => machine.copy(regs=evalInc(regs, r), ip=ip+1)
          case Dec(r)    => machine.copy(regs=evalDec(regs, r), ip=ip+1)
          case Jnz(v, o) => machine.copy(ip=evalJnz(regs, v, o, ip))
        }
      case Machine(_, _, Stopped, _)      => machine
    }
  }

  def evalCpy(regs: Registers, src: Value, dest: Register): Registers =
    regs + (dest.r -> evalValue(regs, src))

  def evalInc(regs: Registers, r: Register): Registers =
    regs.updatedWith(r.r) { _.map(_+1).orElse(Some(1)) }

  def evalDec(regs: Registers, r: Register): Registers =
    regs.updatedWith(r.r) { _.map(_-1).orElse(Some(-1)) }

  def evalJnz(regs: Registers, v: Value, o: Constant, ip: Int): Int =
    if(evalValue(regs, v) == 0) ip + 1
    else                       ip + o.c

  def evalValue(regs: Registers, v: Value): Int =
    v match {
      case Constant(c) => c
      case Register(r) => regs.getOrElse(r, 0)
    }

  val cpyRegRegex   = """cpy ([a-d]) ([a-d])""".r
  val cpyConstRegex = """cpy (-?\d+) ([a-d])""".r
  val incRegex      = """inc ([a-d])""".r
  val decRegex      = """dec ([a-d])""".r
  val jnzRegRegex   = """jnz ([a-d]) (-?\d+)""".r
  val jnzConstRegex = """jnz (-?\d+) (-?\d+)""".r

  def parseInstruction(l: String): Instruction =
    l match {
      case cpyRegRegex(s, d)   => Cpy(Register(s.head), Register(d.head))
      case cpyConstRegex(s, d) => Cpy(Constant(s.toInt), Register(d.head))
      case incRegex(r)         => Inc(Register(r.head))
      case decRegex(r)         => Dec(Register(r.head))
      case jnzRegRegex(r, o)   => Jnz(Register(r.head), Constant(o.toInt))
      case jnzConstRegex(v, o) => Jnz(Constant(v.toInt), Constant(o.toInt))
    }

  def mkMachine(instructions: Vector[Instruction]): Machine =
    Machine(Map.empty[Char, Int], 0, Running, instructions)

 def readFile(f: String): Machine = {
    mkMachine(io.Source.fromFile(f).getLines().map(parseInstruction).toVector)
  }

  val inputFile = "data/Day12.txt"
  val testInput = "data/Day12-small.txt"
}
