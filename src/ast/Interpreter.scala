package dev.toniogela.lox.ast

import dev.toniogela.lox.scanner.*
import dev.toniogela.lox.Slox

object Interpreter extends Visitor[Any]:

  override def visitBinaryExpr(expr: Binary): Any =
    val left  = evaluate(expr.left)
    val right = evaluate(expr.right)
    expr.operator.`type` match
      case TokenType.PLUS          =>
        if left.isInstanceOf[String] && right.isInstanceOf[String] then
          left.asInstanceOf[String] + right.asInstanceOf[String]
        else if left.isInstanceOf[Double] && right.isInstanceOf[Double] then
          left.asInstanceOf[Double] + right.asInstanceOf[Double]
        else
          throw new RuntimeError(
            expr.operator,
            "Operands must be two numbers or two strings.",
          )
      case TokenType.MINUS         =>
        checkNumbersOperand(expr.operator, left, right)
        left.asInstanceOf[Double] - right.asInstanceOf[Double]
      case TokenType.SLASH         =>
        checkNumbersOperand(expr.operator, left, right)
        left.asInstanceOf[Double] / right.asInstanceOf[Double]
      case TokenType.STAR          =>
        checkNumbersOperand(expr.operator, left, right)
        left.asInstanceOf[Double] * right.asInstanceOf[Double]
      case TokenType.GREATER       =>
        checkNumbersOperand(expr.operator, left, right)
        left.asInstanceOf[Double] > right.asInstanceOf[Double]
      case TokenType.GREATER_EQUAL =>
        checkNumbersOperand(expr.operator, left, right)
        left.asInstanceOf[Double] >= right.asInstanceOf[Double]
      case TokenType.LESS          =>
        checkNumbersOperand(expr.operator, left, right)
        left.asInstanceOf[Double] < right.asInstanceOf[Double]
      case TokenType.LESS_EQUAL    =>
        checkNumbersOperand(expr.operator, left, right)
        left.asInstanceOf[Double] <= right.asInstanceOf[Double]
      case TokenType.BANG_EQUAL    => !isEqual(left, right)
      case TokenType.EQUAL_EQUAL   => isEqual(left, right)
      case _                       => null // This should be unreachable

    end match

  end visitBinaryExpr

  override def visitGroupingExpr(expr: Grouping): Any = evaluate(expr.expression)

  override def visitLiteralExpr(expr: Literal): Any = expr.value

  override def visitUnaryExpr(expr: Unary): Any =
    val right = evaluate(expr.right)
    expr.operator.`type` match
      case TokenType.BANG  => !isThruthy(right)
      case TokenType.MINUS =>
        checkNumberOperand(expr.operator, right)
        -right.asInstanceOf[Double]
      case _               => null // This should be unreachable

  private def evaluate(expr: Expr): Any = expr.accept(this)

  private def isThruthy(item: Any): Boolean =
    if item == null then false
    else if item.isInstanceOf[Boolean] then item.asInstanceOf[Boolean]
    else true

  private def isEqual(left: Any, right: Any): Boolean =
    if left == null && right == null then true
    else if left == null then false
    else left.equals(right)

  def checkNumberOperand(operator: Token, operand: Any): Unit =
    if !operand.isInstanceOf[Double] then
      throw new RuntimeError(operator, "Operand must be a number.")

  def checkNumbersOperand(operator: Token, operand1: Any, operand2: Any): Unit =
    if !operand1.isInstanceOf[Double] || !operand2.isInstanceOf[Double] then
      throw new RuntimeError(operator, "Operands must be a number.")

  class RuntimeError(val token: Token, message: String) extends RuntimeException(message)

  def interpret(expression: Expr): Unit =
    try {
      val value = evaluate(expression)
      System.out.println(stringify(value))
    } catch {
      case error: RuntimeError => Slox.runtimeError(error)
    }

  private def stringify(value: Any): String =
    if value == null then "nil"
    else value.toString().stripSuffix(".0")

end Interpreter