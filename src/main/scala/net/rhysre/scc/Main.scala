package net.rhysre.scc

import scala.io.Source

import net.rhysre.scc.assemblygen.AssemblyGen
import net.rhysre.scc.lexer.Lexer
import net.rhysre.scc.parser.Parser

object Main extends App {
  val lexOutput = Lexer.lex(Source.fromFile("file.c").toStream)

  println(AssemblyGen.generate(Parser.parse(lexOutput)))
}
