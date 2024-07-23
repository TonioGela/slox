package dev.toniogela.lox.ast

object AstPrinter extends Visitor[String]:

  def print(expr: Expr): String = expr.accept(AstPrinter)

  override def visitBinaryExpr(expr: Binary): String =
    parenthesize(expr.operator.lexeme, expr.left, expr.right)

  override def visitVariableExpr(expr: Variable): String = ???

  override def visitAssignExpr(expr: Assign): String = ???

  override def visitGroupingExpr(expr: Grouping): String = parenthesize("group", expr.expression)

  override def visitLiteralExpr(expr: Literal): String = Option(expr.value).fold("nil")(_.toString)

  override def visitUnaryExpr(expr: Unary): String = parenthesize(expr.operator.lexeme, expr.right)

  private def parenthesize(name: String, exprs: Expr*): String =
    exprs.foldLeft(s"($name")((h, e) => s"$h ${e.accept(this)}") + ")"

end AstPrinter
