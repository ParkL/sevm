package com.github.parkl.sevm

import org.scalactic._

import scala.annotation.tailrec

object Sevm extends Requirements {

  val emptyStream = Stream.empty

  sealed trait Op

  type Token = Byte
  type Source = Stream[Token]
  type Disassembly = Seq[Op]
  type Lexer = PartialFunction[Source, (Op, Source)]
  type Lexers = Seq[Lexer]

  case object STOP extends Op
  case class ADD(p1: Byte, p2: Byte) extends Op
  case class SUB(p1: Byte, p2: Byte) extends Op
  case class MUL(p1: Byte, p2: Byte) extends Op
  case class DIV(p1: Byte, p2: Byte) extends Op
  case class SDIV(p1: Byte, p2: Byte) extends Op


  object ADD {
    def lex: Lexer = { case 0x01 #:: a1 #:: a2 #:: rest => (ADD(a1, a2), rest) }
  }

  def lex(lexers: Lexers)(source: Source): Disassembly Or ErrorMessage = {
    require(lexers.nonEmpty, "No Lexers defined")
    val combinedLexer = (
        if (lexers.size == 1) lexers.head
        else lexers.reduce { case (a, b) => a orElse b }
      ).lift

    @tailrec def _lex(acc: Disassembly, current: Source): Disassembly Or ErrorMessage = {
      if (current.isEmpty) Good(acc.reverse) // Anchor
      else {
        combinedLexer(current) match {
          case Some((op, rest)) => _lex(op +: acc, rest)
          case None => Bad(s"""Unexpected token at position: TODO""")
        }
      }
    }
    _lex(List.empty, source)
  }

  def lexer: Source => Disassembly Or ErrorMessage = lex(Seq(
    { case 0x00 #:: tail                => (STOP, tail) },
    { case 0x01 #:: a1 #:: a2 #:: tail  => (ADD(a1, a2), tail) }
  ))
}
