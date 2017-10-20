package com.github.parkl.sevm

object Language {
  sealed trait Op
  trait Lexers { val lexers: Seq[Lexer] }

  type Token = Int // Can't be Byte because of sign
  type Source = List[(Token, Int)]
  type Lexer = PartialFunction[Source, ((Op, Int), Source)]

  def mkLex0(code: Token)(f: => Op): Lexer = {
    { case (`code`, position):: tail => ((f, position), tail) }
  }

  def mkLexers0(offset: Int)(ops: Op*) =
    for { (op, i) <- ops.zipWithIndex } yield mkLex0(i + offset)(op)


  def mkLexN(code: Token, n: Int)(f: List[Token] => Op): Lexer = {
    { case (`code`, position) :: tail =>
      val args = tail.take(n)
      val rest = tail.drop(n)
      ((f(args.map(_._1)), position), rest)
    }
  }

  case class UNKNOWN_OPCODE(code: Int) extends Op {
    override def toString: String = f"UNKNOWN-OPCODE($code%#2x)"
  }

  val lexers = Seq(
    Arithmetic, Comparations, Hash, Environment, Block,
    MemStorageFlow, Push, Dup, Swap, Log, System
  ).flatMap(_.lexers).reduce(_ orElse _)

  val catchAll: Lexer = {
    case (code, position):: tail => ((UNKNOWN_OPCODE(code), position), tail)
  }

  val lexersWithUnknown = lexers orElse catchAll

  object Arithmetic extends Lexers {
    case object STOP        extends Op
    case object ADD         extends Op
    case object SUB         extends Op
    case object MUL         extends Op
    case object DIV         extends Op
    case object SDIV        extends Op
    case object MOD         extends Op
    case object SMOD        extends Op
    case object ADDMOD      extends Op
    case object MULMOD      extends Op
    case object EXP         extends Op
    case object SIGNEXTEND  extends Op

    val lexers = mkLexers0(0x00)(STOP, ADD, MUL, SUB, DIV, SDIV, MOD, SMOD, ADDMOD, MULMOD, EXP, SIGNEXTEND)
  }

  object Comparations extends Lexers {
    case object LT      extends Op
    case object GT      extends Op
    case object SLT     extends Op
    case object SGT     extends Op
    case object EQ      extends Op
    case object ISZERO  extends Op
    case object AND     extends Op
    case object OR      extends Op
    case object XOR     extends Op
    case object NOT     extends Op
    case object BYTE    extends Op

    val lexers = mkLexers0(0x10)(LT, GT, SLT, SGT, EQ, ISZERO, AND, OR, XOR, NOT, BYTE)
  }

  object Hash extends Lexers {
    case object SHA3 extends Op

    val lexers = mkLexers0(0x20)(SHA3)
  }

  object Environment extends Lexers {
    case object ADDRESS       extends Op
    case object BALANCE       extends Op
    case object ORIGIN        extends Op
    case object CALLER        extends Op
    case object CALLVALUE     extends Op
    case object CALLDATALOAD  extends Op
    case object CALLDATASIZE  extends Op
    case object CALLDATACOPY  extends Op
    case object CODESIZE      extends Op
    case object CODECOPY      extends Op
    case object GASPRICE      extends Op
    case object EXTCODESIZE   extends Op
    case object EXTCODECOPY   extends Op

    val lexers = mkLexers0(0x30)(
      ADDRESS, BALANCE, ORIGIN, CALLER, CALLVALUE,
      CALLDATALOAD, CALLDATASIZE, CALLDATACOPY, CODESIZE,
      CODECOPY, GASPRICE, EXTCODESIZE, EXTCODECOPY
    )
  }

  object Block extends Lexers {
    case object BLOCKHASH   extends Op
    case object COINBASE    extends Op
    case object TIMESTAMP   extends Op
    case object NUMBER      extends Op
    case object DIFFICULTY  extends Op
    case object GASLIMIT    extends Op

    val lexers = mkLexers0(0x40)(BLOCKHASH, COINBASE, TIMESTAMP, NUMBER, DIFFICULTY, GASLIMIT)
  }

  object MemStorageFlow extends Lexers {
    case object POP       extends Op
    case object MLOAD     extends Op
    case object MSTORE    extends Op
    case object MSTORE8   extends Op
    case object SLOAD     extends Op
    case object SSTORE    extends Op
    case object JUMP      extends Op
    case object JUMPI     extends Op
    case object PC        extends Op
    case object MSIZE     extends Op
    case object GAS       extends Op
    case object JUMPDEST  extends Op

    val lexers = mkLexers0(0x50)(
      POP, MLOAD, MSTORE, MSTORE8, SLOAD, SSTORE, JUMP, JUMPI, PC, MSIZE, GAS, JUMPDEST
    )
  }

  object Push extends Lexers {
    case class PUSH(args: List[Token]) extends Op {
      override def toString: String = s"PUSH${args.length}(0x${args.map(hs => f"$hs%02x").mkString})"
    }

    val lexers = for (i <- 0 until 32) yield mkLexN(0x60 + i, i + 1)(PUSH)
  }

  object Dup extends Lexers {
    case class DUP(stackItem: Int) extends Op {
      override def toString: String = s"DUP$stackItem"
    }

    val lexers = for (i <- 0 until 16) yield mkLex0(0x80 + i)(DUP(i + 1))
  }

  object Swap extends Lexers {
    case class SWAP(stackItem: Int) extends Op {
      override def toString: String = s"SWAP$stackItem"
    }

    val lexers = for (i <- 0 until 16) yield mkLex0(0x90 + i)(SWAP(i + 1))
  }

  object Log extends Lexers { // https://www.youtube.com/watch?v=-fQGPZTECYs
    case class LOG(topics: Int) extends Op {
      override def toString: String = s"LOG$topics"
    }

    val lexers = for (i <- 0 to 4) yield mkLex0(0xa0 + i)(LOG(i))
  }

  object System extends Lexers {
    case object CREATE        extends Op
    case object CALL          extends Op
    case object CALLCODE      extends Op
    case object RETURN        extends Op
    case object DELEGATECALL  extends Op
    case object SUICIDE       extends Op

    val lexers = mkLexers0(0xf0)(
      CREATE, CALL, CALLCODE, RETURN, DELEGATECALL
    ) :+ mkLex0(0xff)(SUICIDE)
  }
}
