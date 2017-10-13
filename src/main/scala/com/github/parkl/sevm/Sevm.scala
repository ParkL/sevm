package com.github.parkl.sevm

import org.scalactic.{Bad, ErrorMessage, Good, Or}

import scala.annotation.tailrec

object Sevm {

  val emptyStream = Stream.empty

  sealed trait Op

  type Token = Byte
  type Source = Stream[Token]
  type Disassembly = Seq[Op]
  type Lexer = PartialFunction[Source, (Op, Source)]
  type Lexers = Seq[Lexer]

  case object STOP extends Op
  case class ADD(p1: Byte, p2: Byte) extends Op

  object ADD {
    def lex: Lexer = { case 0x01 #:: a1 #:: a2 #:: rest => (ADD(a1, a2), rest) }
  }

  def lex(lexers: Lexers)(source: Source): Disassembly Or ErrorMessage = {
    @tailrec def _lex(acc: Disassembly, current: Source): Disassembly Or ErrorMessage = {
      if (current.isEmpty) Good(acc.reverse)
      else {
        lexers.find(_.isDefinedAt(current)) match {
          case Some(lexer) =>
            val (op, rest) = lexer(current)
            _lex(op +: acc, rest)
          case None => Bad(s"""Unexpected sequence: TODO""")
        }
      }
    }
    _lex(List.empty, source)
  }
}
