/*
 * Copyright 2022 Creative Scala
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

// We're going to represent expressions as follows. An Expression is one of:

// A literal number, containing a Double value.
// A variable, containing a String name, which represents a variable such as  or  in our function.
// The addition of two Expressions.
// The multiplication of two Expressions.

package gradientdescent.symbolic

sealed trait Expression {
  import Expression._

  def +(that: Expression): Expression = 
    Addition(this, that)

  def *(that: Expression): Expression = 
    Multiplication(this, that)

  override def toString(): String =
    this match
      case Literal(value) => value.toString()
      case Variable(name) => name
      case Addition(operandOne, operandTwo) =>
        s"(${operandOne.toString()} + ${operandTwo.toString()})"
      case Multiplication(operandOne, operandTwo) =>
        s"(${operandOne.toString()} * ${operandTwo.toString()})"
    

// Implement a method bind that accepts both a variable name (a String) and a value for that variable (a Double),
//  and substitutes the value for all occurrences of the variable within the Expression.

  def bind(variableName: String, value: Double): Expression = 
    this match {
      case Literal(v) => Literal(v)
      case Variable(name) => 
        if (name == variableName) Literal(value) else Variable(name)
      case Addition(operandOne, operandTwo) => 
        Addition(operandOne.bind(variableName, value), operandTwo.bind(variableName, value))
      case Multiplication(operandOne, operandTwo) => 
        Multiplication(operandOne.bind(variableName, value), operandTwo.bind(variableName, value))
    }

  def simplify: Expression = 
    this match {
      case Literal(value) => Literal(value)
      case Variable(name) => Variable(name)
      case Addition(operandOne, operandTwo) => 
        (operandOne.simplify, operandTwo.simplify) match {
          case (Literal(v1), Literal(v2)) => Literal(v1 + v2)
          case (Literal(0), o2) => o2
          case (o1, Literal(0)) => o1
          case (o1, o2) => Addition(o1, o2)
        }
      case Multiplication(operandOne, operandTwo) => 
        (operandOne.simplify, operandTwo.simplify) match {
          case (Literal(v1), Literal(v2)) => Literal(v1 * v2)
          case (Literal(0), _) => Literal(0)
          case (_, Literal(0)) => Literal(0)          
          case (Literal(1), expr) => expr
          case (expr, Literal(1)) => expr
          case (o1, o2) => Multiplication(o1, o2)
        }
    }

  def differentiate(variableName: String): Expression = 
    this match
      case Literal(value) => Literal(0)
      case Variable(name) => if (name == variableName) Literal(1) else Literal(0)
      case Addition(operandOne, operandTwo) => 
        operandOne.differentiate(variableName) + operandTwo.differentiate(variableName)
      case Multiplication(operandOne, operandTwo) => 
        (operandOne.differentiate(variableName) * operandTwo) + (operandOne * operandTwo.differentiate(variableName))
    
}
object Expression {
  final case class Literal(value: Double) extends Expression
  final case class Variable(name: String) extends Expression
  final case class Addition(operandOne: Expression, operandTwo: Expression) extends Expression
  final case class Multiplication(operandOne: Expression, operandTwo: Expression) extends Expression

  /** Create a literal given its value. */
  def literal(value: Double): Expression = 
    Literal(value)

  /** Create a variable given its name. */
  def variable(name: String): Expression = 
    Variable(name)
}

@main def expressionExample() = {
  // 8x + 8
  val expr1 = Expression.literal(8) * Expression.variable("x") + Expression.literal(8)
  // 4x^2 + 8x + 16
  val x = Expression.variable("x")
  val expr2 = (Expression.literal(4) * x * x) + (Expression.literal(8) * x) + Expression.literal(16)
  println(expr1)
  println(expr1.bind("x", 4))
  println(expr1.bind("x", 4).simplify)
  println(expr1.bind("x", 4).simplify.simplify)
  println(expr1.bind("x", 4).simplify.simplify.simplify)
  println(expr1.differentiate("x").simplify)
  println(expr2.differentiate("x").simplify.simplify)
}