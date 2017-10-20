package com.github.parkl.sevm

import org.scalactic._

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object Sevm extends Requirements {
  import Language._

  type Disassembly = Seq[(Op, Int)]

  type Lexers = Seq[Lexer]

  def tokenize(chars: Stream[Char]): Source Or Every[ErrorMessage] = {
    import Accumulation._
    val pb16: String => Int Or One[ErrorMessage] = s =>
      Try { Integer.parseInt(s, 16) } match {
        case Failure(_: NumberFormatException) =>
          Bad(One(s"Not a hex-number: `$s`"))
        case Failure(err) => Bad(One(s"WTF: $err"))
        case Success(value) => Good(value)
      }
    chars
      .grouped(2) // need pairs
      .map(_.mkString) // concat to strings
      .drop(1) // remove 0x
      .validatedBy(pb16) // all ok Hex-Numbers?
      .map(_.zipWithIndex) // line numbers
      .map(_.toStream) // if yes make stream
  }

  def lex(lexer: Lexer)(source: Source): Disassembly = {
    @tailrec def _lex(acc: Disassembly, current: Source): Disassembly = {
      if (current.isEmpty) acc.reverse // Anchor
      else {
        val (opAndPos, rest) = lexer(current)
        _lex(opAndPos +: acc, rest)
      }
    }
    _lex(List.empty, source)
  }

  val lexer: Source => Disassembly = lex(lexersWithUnknown)

  def disassemble(s: Stream[Char]): Disassembly Or Every[ErrorMessage] =
    tokenize(s).map(lexer)
}
