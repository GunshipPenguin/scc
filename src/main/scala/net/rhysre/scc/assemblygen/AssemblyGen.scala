package net.rhysre.scc.assemblygen

import net.rhysre.scc.parser.nodes._

object AssemblyGen {
  private def genFunction(name: String): String = s".globl $name\n$name:\n"

  private def genProgram() = s".text\n"

  private def genReturn(retVal: AstNode) = retVal match {
    case IntValueNode(value) => s"\tmovl\t$value, %eax\n"
    case _ => throw new UnsupportedOperationException("Return type not implemented")
  }

  def generate(root: AstNode): String = root match {
    case ProgramNode(entryPoint) => genProgram() + generate(entryPoint)
    case FunctionNode(name, nodes) => genFunction(name) + (for(node<-nodes) yield generate(node)).mkString
    case ReturnNode(retVal) => genReturn(retVal)
  }
}
