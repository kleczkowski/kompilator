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

import com.github.repaj.kompilator.{SymbolTable, ir}
import com.github.repaj.kompilator.ir.{BasicBlock, IRBuilder, Name, Operand}

import scala.collection.mutable

class AstToTacVisitor(builder: IRBuilder, table: SymbolTable) {

  def generate(block: Block): (BasicBlock, mutable.Buffer[BasicBlock]) = {
    val entryBlock = builder.newBlock("entry")
    builder.setActiveBlock(entryBlock)
    codegenBlock(block)
    builder += ir.Halt
    (entryBlock, builder.getBlocks)
  }

  private def codegenBlock(block: Block): Unit = {
    for (stmt <- block.stmts) codegenStmt(stmt)
  }

  private def codegenStmt(stmt: Stmt): Unit = stmt match {
    case block: Block => codegenBlock(block)
    case ifStmt: If => codegenIf(ifStmt)
    case whileStmt: While => codegenWhile(whileStmt)
    case doWhile: DoWhile => codegenDoWhile(doWhile)
    case forStmt: For => codegenFor(forStmt)
    case assign: Assign => codegenAssign(assign)
    case read: Read => codegenRead(read)
    case write: Write => codegenWrite(write)
  }

  private def codegenAssign(assign: Assign): Unit = assign match {
    case Assign(ArrayRef(entry, idx), Constant(value)) =>
      builder += ir.IndexedStore(ir.Constant(value), Name(entry), codegenValue(idx))
    case Assign(ArrayRef(aEntry, idx), VariableRef(vEntry)) =>
      builder += ir.IndexedStore(ir.Name(vEntry), Name(aEntry), codegenValue(idx))
    case Assign(VariableRef(entry), src) =>
      codegenExpr(src, ir.Name(entry))
    case Assign(ArrayRef(entry, idx), src) =>
      val temp = builder.newTemp
      codegenExpr(src, temp)
      builder += ir.IndexedStore(temp, Name(entry), codegenValue(idx))
  }

  private def codegenIf(`if`: If): Unit = {
    val If(cond, ifTrue, ifFalse) = `if`

    val thenBlock = builder.newBlock("if.then")
    val elseBlock = builder.newBlock("if.else")
    val contBlock = builder.newBlock("if.cont")

    codegenCond(cond, thenBlock, elseBlock)
    builder.setActiveBlock(thenBlock)
    codegenBlock(ifTrue)
    builder += ir.Jump(contBlock)
    builder.setActiveBlock(elseBlock)
    codegenBlock(ifFalse)
    builder += ir.Jump(contBlock)
    builder.setActiveBlock(contBlock)
  }

  private def codegenWhile(`while`: While): Unit = {
    val While(cond, body) = `while`

    val testBlock = builder.newBlock("while.test")
    val bodyBlock = builder.newBlock("while.body")
    val contBlock = builder.newBlock("while.cont")

    builder += ir.Jump(testBlock)
    builder.setActiveBlock(testBlock)
    codegenCond(cond, bodyBlock, contBlock)
    builder.setActiveBlock(bodyBlock)
    codegenBlock(body)
    builder += ir.Jump(testBlock)
    builder.setActiveBlock(contBlock)
  }

  private def codegenDoWhile(doWhile: DoWhile): Unit = {
    val DoWhile(cond, body) = doWhile

    val bodyBlock = builder.newBlock("while.body")
    val testBlock = builder.newBlock("while.test")
    val contBlock = builder.newBlock("while.cont")

    builder += ir.Jump(bodyBlock)
    builder.setActiveBlock(bodyBlock)
    codegenBlock(body)
    builder += ir.Jump(testBlock)
    builder.setActiveBlock(testBlock)
    codegenCond(cond, bodyBlock, contBlock)
    builder.setActiveBlock(contBlock)
  }

  private def codegenFor(`for`: For): Unit = {
    val For(it, lo, downTo, hi, body) = `for`

    val it2Name = s"${"_"}${it.name}"
    table.newVariable(
      it.location,
      it2Name,
      iterator = true)

    val it2 = Name(table.lookup(it2Name).asVariable)
    table.remove(it2Name)
    val testBlock = builder.newBlock("for.test")
    val bodyBlock = builder.newBlock("for.body")
    val contBlock = builder.newBlock("for.cont")

    val loOp = codegenValue(lo)
    val hiOp = codegenValue(hi)
    if (!downTo) {
      builder += ir.Move(loOp, ir.Name(it))
      builder += ir.Sub(hiOp, loOp, it2)
      builder += ir.Add(it2, ir.Constant(1), it2)
      builder += ir.Jump(testBlock)
      builder.setActiveBlock(testBlock)
      builder += ir.JumpIf(ir.Gt(it2, ir.Constant(0)), bodyBlock, contBlock)
      builder.setActiveBlock(bodyBlock)
      codegenBlock(body)
      builder += ir.Sub(it2, ir.Constant(1), it2)
      builder += ir.Add(ir.Name(it), ir.Constant(1), ir.Name(it))
      builder += ir.Jump(testBlock)
      builder.setActiveBlock(contBlock)
    } else {
      builder += ir.Move(loOp, ir.Name(it))
      builder += ir.Sub(loOp, hiOp, it2)
      builder += ir.Add(it2, ir.Constant(1), it2)
      builder += ir.Jump(testBlock)
      builder.setActiveBlock(testBlock)
      builder += ir.JumpIf(ir.Gt(it2, ir.Constant(0)), bodyBlock, contBlock)
      builder.setActiveBlock(bodyBlock)
      codegenBlock(body)
      builder += ir.Sub(it2, ir.Constant(1), it2)
      builder += ir.Sub(ir.Name(it), ir.Constant(1), ir.Name(it))
      builder += ir.Jump(testBlock)
      builder.setActiveBlock(contBlock)
    }
  }

  private def codegenRead(read: Read): Unit = read match {
    case Read(VariableRef(entry)) =>
      builder += ir.Get(ir.Name(entry))
    case Read(ArrayRef(entry, idx)) =>
      val temp = builder.newTemp
      builder += ir.Get(temp)
      builder += ir.IndexedStore(temp, Name(entry), codegenValue(idx))
  }

  private def codegenWrite(write: Write): Unit = {
    builder += ir.Put(codegenValue(write.value))
  }

  private def codegenCond(cond: Cond, ifTrue: BasicBlock, ifFalse: BasicBlock): Unit = cond match {
    case Eq(left, right) =>
      builder += ir.JumpIf(ir.Eq(codegenValue(left), codegenValue(right)), ifTrue, ifFalse)
    case Ne(left, right) =>
      builder += ir.JumpIf(ir.Ne(codegenValue(left), codegenValue(right)), ifTrue, ifFalse)
    case Le(left, right) =>
      builder += ir.JumpIf(ir.Le(codegenValue(left), codegenValue(right)), ifTrue, ifFalse)
    case Ge(left, right) =>
      builder += ir.JumpIf(ir.Ge(codegenValue(left), codegenValue(right)), ifTrue, ifFalse)
    case Lt(left, right) =>
      builder += ir.JumpIf(ir.Lt(codegenValue(left), codegenValue(right)), ifTrue, ifFalse)
    case Gt(left, right) =>
      builder += ir.JumpIf(ir.Gt(codegenValue(left), codegenValue(right)), ifTrue, ifFalse)
  }

  private def codegenExpr(expr: Expr, destination: Operand): Unit = expr match {
    case value: Value =>
      builder += ir.Move(codegenValue(value), destination)
    case Add(left, right) =>
      builder += ir.Add(codegenValue(left), codegenValue(right), destination)
    case Sub(left, right) =>
      builder += ir.Sub(codegenValue(left), codegenValue(right), destination)
    case Mul(left, right) =>
      builder += ir.Mul(codegenValue(left), codegenValue(right), destination)
    case Div(left, right) =>
      builder += ir.Div(codegenValue(left), codegenValue(right), destination)
    case Rem(left, right) =>
      builder += ir.Rem(codegenValue(left), codegenValue(right), destination)
  }

  private def codegenValue(value: Value): Operand = value match {
    case VariableRef(entry) =>
      ir.Name(entry)
    case ArrayRef(entry, idx) =>
      val temp = builder.newTemp
      builder += ir.IndexedLoad(Name(entry), codegenValue(idx), temp)
      temp
    case Constant(v) =>
      ir.Constant(v)
  }

}
