package com.github.parkl.sevm

import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

class SevmSpec extends FlatSpec with Matchers {
  import Sevm._
  val fle = """ERC20contracts_unique.csv"""

  val programs = List(
    """0x6000609f537c010000000000000000000000000000000000000000000000000000000060003504630f2c932981141561009c5736599059016000905236600482376004356060526024356080525069d3c21bcecceda100000073bf4ed7b27f1d666546e30d74d50d173d20bca75431131561008957600060006000600030316060516000f161009a565b600060006000600030316080516000f15b505b50""",
    """0x60606040523415600e57600080fd5b5b60fe60016000739d3f257827b17161a098d380822fa2614ff540c873ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020819055505b5b60368060776000396000f30060606040525b600080fd00a165627a7a7230582044b2b40d1e4a6cf4cf7a3f13e3d3e22e368fdf64a65a779c05080401c178618b0029"""
  )

  """The tokenizer""" should """tokenize""" in {
    val fstProgram = programs.head
    val chars = fstProgram.toStream
    val expSize = (fstProgram.length / 2) - 1 // minus "0x"
    val tokenized = tokenize(chars)
    tokenized.size should ===(expSize)
    val r = lexer(tokenized).get
    println(r.mkString("\n"))
  }

  it should "disassemble" in {
    println(disassemble(programs(0).toStream).get.mkString("\n"))
  }

  it should """parse a while file""" in {
    val source = Source.fromURL(getClass.getResource(s"/$fle")).getLines().toStream.drop(1)
    val rx = for {
      line <- source
      program <- line.split(',').headOption
      tokenized = tokenize(program.toStream)
    } yield lexer(tokenized)

    val (goods, bads) = rx.partition(_.isGood)
    println(s"Good: ${goods.size}, Bad: ${bads.size}")
    println(bads.head.badMap(b => b._1.mkString("\n") + "\n" + b._2))
  }
}
