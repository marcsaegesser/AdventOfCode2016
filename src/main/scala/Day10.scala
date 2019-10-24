package advent

object Day10 {

  def day10(): Unit = {
    val factory = readFile(inputFile)
    println(s"Day10.part1 = ${part1(factory)}")
    println(s"Day10.part2 = ${part2(factory)}")
  }

  def part1(factory: Factory): Int = {
    runFactory(factory).history.collect { case (b, l, h) if l == 17 && h == 61 => b }.head
  }

  def part2(factory: Factory): Int = {
    val f = runFactory(factory)
    f.outputs(0).head * f.outputs(1).head * f.outputs(2).head
  }


  type BotId = Int
  type OutputId = Int

  sealed trait Destination
  case class BotDestination(bot: BotId)       extends Destination
  case class OutputDestination(out: OutputId) extends Destination

  sealed trait Instruction
  case class Value(bot: BotId, v: Int)           extends Instruction
  case class Rule(bot: BotId, lowTo: Destination, highTo: Destination) extends Instruction

  case class Factory(
    bots: Map[BotId, List[Int]],
    outputs: Map[OutputId, List[Int]],
    history: List[(Int, Int, Int)],
    rules: Map[BotId, Rule])

  def runFactory(factory: Factory): Factory = {
    findReady(factory) match {
      case Nil => factory
      case bs  => runFactory(bs.foldLeft(factory) { case (f, b) => applyRule(f, b) })
    }
  }

  def findReady(factory: Factory): List[BotId] =
    factory.bots.collect { case (b, vs) if vs.size == 2 => b }.toList

  def applyRule(factory: Factory, bot: BotId): Factory = {
    val List(l, h) = factory.bots(bot).sorted
    val hist = (bot, l, h) +: factory.history

    factory.rules(bot) match {
      case Rule(_, BotDestination(ld), BotDestination(hd))       =>
        factory.copy(
          bots = emptyBot(addBotValue(addBotValue(factory.bots, ld, l), hd, h), bot),
          history = hist)
      case Rule(_, BotDestination(ld), OutputDestination(hd))    =>
        factory.copy(
          bots = emptyBot(addBotValue(factory.bots, ld, l), bot),
          outputs = addOutputValue(factory.outputs, hd, h),
          history = hist)
      case Rule(_, OutputDestination(ld), BotDestination(hd))    =>
        factory.copy(
          bots = emptyBot(addBotValue(factory.bots, hd, h), bot),
          outputs = addOutputValue(factory.outputs, ld, l),
          history = hist)
      case Rule(_, OutputDestination(ld), OutputDestination(hd)) =>
        factory.copy(
          bots = emptyBot(factory.bots, bot),
          outputs = addOutputValue(addOutputValue(factory.outputs, ld, l), hd, h),
          history = hist)
    }
  }

  def emptyBot(bots: Map[BotId, List[Int]], botId: BotId): Map[BotId, List[Int]] =
    bots - botId


  def addBotValue(bots: Map[BotId, List[Int]], bot: Int, v: Int): Map[BotId, List[Int]] =
    bots.updatedWith(bot) {
      case None    => Some(List(v))
      case Some(l) => Some(v +: l)
    }

  def addOutputValue(outputs: Map[OutputId, List[Int]], output: Int, v: Int): Map[OutputId, List[Int]] =
    outputs.updatedWith(output) {
      case None    => Some(List(v))
      case Some(l) => Some(v +: l)
    }

  val valueRegex = """value (\d+) goes to bot (\d+)""".r
  val ruleRegex  = """bot (\d+) gives low to (\w+) (\d+) and high to (\w+) (\d+)""".r

  def parseInstruction(l: String): Instruction =
    l match {
      case valueRegex(v, b)                       => Value(b.toInt, v.toInt)
      case ruleRegex(b, "bot", l, "bot", h)       => Rule(b.toInt, BotDestination(l.toInt), BotDestination(h.toInt))
      case ruleRegex(b, "output", l, "bot", h)    => Rule(b.toInt, OutputDestination(l.toInt), BotDestination(h.toInt))
      case ruleRegex(b, "bot", l, "output", h)    => Rule(b.toInt, BotDestination(l.toInt), OutputDestination(h.toInt))
      case ruleRegex(b, "output", l, "output", h) => Rule(b.toInt, OutputDestination(l.toInt), OutputDestination(h.toInt))
    }

  def readFile(f: String): Factory =
    mkFactory(
      io.Source.fromFile(f).getLines()
      .map(parseInstruction)
      .toList)

  def mkFactory(is: List[Instruction]): Factory ={
    val (bots, rules) =
      is.foldLeft((Map.empty[BotId, List[Int]], Map.empty[BotId, Rule])) { case ((bs, rs), i) =>
        i match {
          case Value(b, v)     => (addBotValue(bs, b.toInt, v.toInt), rs)
          case r@Rule(b, l, h) => (bs, rs + (b.toInt -> r))
        }
      }

    Factory(bots, Map.empty[BotId, List[Int]], List.empty[(Int, Int, Int)], rules)
  }

  val inputFile = "data/Day10.txt"
  val testInput = "data/Day10-small.txt"
}
