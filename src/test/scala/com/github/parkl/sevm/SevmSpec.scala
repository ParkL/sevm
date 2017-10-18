package com.github.parkl.sevm

import org.scalactic.Bad
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

class SevmSpec extends FlatSpec with Matchers {
  import Sevm._
  val fle = """ERC20contracts_unique.csv"""

  val programs = List(
    """0x6000609f537c010000000000000000000000000000000000000000000000000000000060003504630f2c932981141561009c5736599059016000905236600482376004356060526024356080525069d3c21bcecceda100000073bf4ed7b27f1d666546e30d74d50d173d20bca75431131561008957600060006000600030316060516000f161009a565b600060006000600030316080516000f15b505b50"""
  )

  """The tokenizer""" should """tokenize""" in {
    val fstProgram = programs.head
    val charIt = fstProgram.toCharArray
    val expSize = (fstProgram.length / 2) - 1 // minus "0x"
    val tokenized = tokenize(charIt)
    tokenized.size should ===(expSize)
    val r = lexer(tokenized).get
    println(r.mkString("\n"))
  }

  it should """parse a while file""" in {
    val source = Source.fromURL(getClass.getResource(s"/$fle")).getLines().drop(1)
    val rx = for {
      (line, i) <- source.zipWithIndex.toList
      program <- line.split(',').headOption
      tokenized = tokenize(program.toCharArray)
    } yield lexer(tokenized)

    val (goods, bads) = rx.partition(_.isGood)
    println(s"Good: ${goods.size}, Bad: ${bads.size}")
    println(bads.head.badMap(b => b._1.mkString("\n") + "\n" + b._2))
  }
}
