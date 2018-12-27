/*
 * MIT License
 *
 * Copyright (c) 2018 Konrad Kleczkowski
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

package com.github.repaj.kompilator.parser.ast

import com.github.repaj.kompilator.{Location, SymbolTable}

/**
  * Base class for AST.
  */
sealed abstract class ASTNode

sealed abstract class Expr extends ASTNode

sealed abstract class Stmt extends ASTNode

sealed abstract class Value extends Expr

sealed abstract class Ref extends Value {
  def entry: SymbolTable.Entry
}

sealed abstract class Cond extends ASTNode

case class Block(stmts: Stmt*) extends Stmt

case class If(cond: Cond, ifTrue: Block, ifFalse: Block) extends Stmt

case class While(cond: Cond, body: Block) extends Stmt

case class DoWhile(cond: Cond, body: Block) extends Stmt

case class For(iteratorEntry: SymbolTable.Entry, low: Value, downTo: Boolean, hi: Value, body: Block) extends Stmt

case class Assign(lhs: Ref, rhs: Expr) extends Stmt

case class Read(ref: Ref) extends Stmt

case class Write(value: Value) extends Stmt

case class Add(left: Value, right: Value) extends Expr

case class Sub(left: Value, right: Value) extends Expr

case class Mul(left: Value, right: Value) extends Expr

case class Div(left: Value, right: Value) extends Expr

case class Rem(left: Value, right: Value) extends Expr

case class Eq(left: Value, right: Value) extends Cond

case class Ne(left: Value, right: Value) extends Cond

case class Le(left: Value, right: Value) extends Cond

case class Ge(left: Value, right: Value) extends Cond

case class Lt(left: Value, right: Value) extends Cond

case class Gt(left: Value, right: Value) extends Cond

case class Constant(value: BigInt) extends Value

case class VariableRef(entry: SymbolTable.Entry) extends Ref

case class ArrayRef(entry: SymbolTable.Entry, idx: Value) extends Ref