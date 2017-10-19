package com.github.parkl.sevm

import org.scalactic._

import scala.annotation.tailrec

object Sevm extends Requirements {
  import Language._

  type Disassembly = Seq[Op]

  type Lexers = Seq[Lexer]

  def tokenize(chars: Stream[Char]): Source = {
    val parseBase16 = Integer.parseInt(_: String, 16)
    val grouped = chars.grouped(2).map(_.mkString(""))
    val skippedPreamble = grouped.dropWhile(_ != "0x").drop(1)
    skippedPreamble.map(_.mkString).map(parseBase16).toStream
  }

  def lex(lexer: Lexer)(source: Source): Disassembly Or (Disassembly, ErrorMessage) = {
    @tailrec def _lex(acc: Disassembly, current: Source): Disassembly Or (Disassembly, ErrorMessage) = {
      if (current.isEmpty) Good(acc.reverse) // Anchor
      else {
        val (op, rest) = lexer(current)
        _lex(op +: acc, rest)
      }
    }
    _lex(List.empty, source)
  }

  def lexer: Source => Disassembly Or (Disassembly, ErrorMessage) = lex(Language.lexersWithUnknown)

  def disassemble = lexer compose tokenize
}
