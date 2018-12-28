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

package com.github.repaj.kompilator.codegen

import com.github.repaj.kompilator.SymbolTable.{ArrayEntry, VariableEntry}
import com.github.repaj.kompilator.ir._
import com.github.repaj.kompilator.vm._

import scala.language.implicitConversions

/**
  * An implementation of code generator.
  *
  * @param builder the output builder
  * @author Konrad Kleczkowski
  */
final class CodeGenerator(protected val builder: AsmBuilder) extends MemoryManager with Macros {
  /**
    * Emits a sequence of basic blocks.
    *
    * @param basicBlocks the basic block sequence
    */
  def emit(basicBlocks: BasicBlock*): Unit = basicBlocks.foreach(emitBlock)

  private def emitBlock(basicBlock: BasicBlock): Unit = {
    builder.label(basicBlock.name)
    for (instruction <- basicBlock.list) emitInstruction(instruction)
  }

  private def emitInstruction(instruction: Instruction): Unit = {
    freeRegs()
    instruction match {
      case instruction: LoadStoreInstruction => emitLoadStore(instruction)
      case instruction: BinaryInstruction => emitBinary(instruction)
      case instruction: BranchInstruction => spillAll(); emitBranch(instruction); clearRegistersState()
    }
  }

  private def emitLoadStore(instruction: LoadStoreInstruction): Unit = instruction match {
    case Get(destination) => assign(get(), destination)
    case Put(source) => put(source)
    case Move(source, destination) => copy(load(source), destination)
    case IndexedLoad(base: ArrayEntry, offset, destination) => assign(arrayLoad(base, offset), destination)
    case IndexedStore(source, base: ArrayEntry, offset) => arrayStore(source, base, offset)
    case oth => throw new IllegalArgumentException(s"instruction = $oth")
  }

  private def emitBinary(instruction: BinaryInstruction): Unit = {
    def constSub(left: BigInt, right: BigInt): BigInt = if (left <= right) 0 else left - right

    def constDiv(left: BigInt, right: BigInt): BigInt = if (right == 0) 0 else left / right

    def constRem(left: BigInt, right: BigInt): BigInt = if (right == 0) 0 else left % right

    def simplifyAdd: PartialFunction[BinaryInstruction, Unit] = {
      case Add(Constant(left), Constant(right), result) => copy(load(Constant(left + right)), result)
      case Add(left, Constant(zero), result) if zero == 0 => copy(load(left), result)
      case Add(Constant(zero), right, result) if zero == 0 => copy(load(right), result)
      case Add(left, Constant(one), result) if one == 1 => assign(inc(left), result)
      case Add(Constant(one), right, result) if one == 1 => assign(inc(right), result)
    }

    def simplifySub: PartialFunction[BinaryInstruction, Unit] = {
      case Sub(Constant(left), Constant(right), result) => copy(load(Constant(constSub(left, right))), result)
      case Sub(left, Constant(zero), result) if zero == 0 => copy(load(left), result)
      case Sub(left, Constant(one), result) if one == 1 => assign(dec(left), result)
    }

    def simplifyMul: PartialFunction[BinaryInstruction, Unit] = {
      case Mul(Constant(left), Constant(right), result) => copy(load(Constant(left * right)), result)
      case Mul(_, Constant(zero), result) if zero == 0 => copy(load(Constant(0)), result)
      case Mul(Constant(zero), _, result) if zero == 0 => copy(load(Constant(0)), result)
      case Mul(left, Constant(one), result) if one == 1 => copy(load(left), result)
      case Mul(Constant(one), right, result) if one == 1 => copy(load(right), result)
      case Mul(left, Constant(two), result) if two == 2 => assign(twice(left), result)
      case Mul(Constant(two), right, result) if two == 2 => assign(twice(right), result)
    }

    def simplifyDiv: PartialFunction[BinaryInstruction, Unit] = {
      case Div(Constant(left), Constant(right), result) => copy(load(Constant(constDiv(left, right))), result)
      case Div(_, Constant(zero), result) if zero == 0 => copy(load(Constant(0)), result)
      case Div(left, Constant(one), result) if one == 1 => copy(load(left), result)
      case Div(left, Constant(two), result) if two == 2 => assign(half(left), result)
    }

    def simplifyRem: PartialFunction[BinaryInstruction, Unit] = {
      case Rem(Constant(left), Constant(right), result) => copy(load(Constant(constRem(left, right))), result)
      case Rem(_, Constant(zero), result) if zero == 0 => copy(load(Constant(0)), result)
      case Rem(left, Constant(one), result) if one == 1 => copy(load(left), result)
      case Rem(left, Constant(two), result) if two == 2 => assign(rem2(left), result)
    }

    def plainTranslate(instruction: BinaryInstruction): Unit = instruction match {
      case Add(left, right, result) => assign(add(left, right), result)
      case Sub(left, right, result) => assign(sub(left, right), result)
      case Mul(left, right, result) => assign(longMul(left, right), result)
      case Div(left, right, result) => assign(longDiv(left, right), result)
      case Rem(left, right, result) => assign(longRem(left, right), result)
    }

    simplifyAdd
      .orElse(simplifySub)
      .orElse(simplifyMul)
      .orElse(simplifyDiv)
      .orElse(simplifyRem)
      .applyOrElse(instruction, plainTranslate)
  }

  private implicit def op2desc(operand: Operand): DescriptorEntry = operand match {
    case Name(entry: VariableEntry) => DescVar(entry)
    case Temp(id) => DescTemp(id)
    case oth => throw new IllegalArgumentException(s"operand = $oth")
  }

  private def emitBranch(instruction: BranchInstruction): Unit = {
    def simplifyCond: PartialFunction[BranchInstruction, Unit] = {
      case JumpIf(Eq(Constant(left), Constant(right)), ifTrue, _) if left == right =>
        builder += AsmJump(ifTrue.name)
      case JumpIf(Eq(Constant(left), Constant(right)), _, ifFalse) if left != right =>
        builder += AsmJump(ifFalse.name)
      case JumpIf(Le(Constant(left), Constant(right)), ifTrue, _) if left <= right =>
        builder += AsmJump(ifTrue.name)
      case JumpIf(Le(Constant(left), Constant(right)), _, ifFalse) if left > right =>
        builder += AsmJump(ifFalse.name)
      case JumpIf(Ge(Constant(left), Constant(right)), ifTrue, _) if left >= right =>
        builder += AsmJump(ifTrue.name)
      case JumpIf(Ge(Constant(left), Constant(right)), _, ifFalse) if left < right =>
        builder += AsmJump(ifFalse.name)
      case JumpIf(Lt(Constant(left), Constant(right)), ifTrue, _) if left < right =>
        builder += AsmJump(ifTrue.name)
      case JumpIf(Lt(Constant(left), Constant(right)), _, ifFalse) if left >= right =>
        builder += AsmJump(ifFalse.name)
      case JumpIf(Gt(Constant(left), Constant(right)), ifTrue, _) if left > right =>
        builder += AsmJump(ifTrue.name)
      case JumpIf(Gt(Constant(left), Constant(right)), _, ifFalse) if left <= right =>
        builder += AsmJump(ifFalse.name)
      case JumpIf(Eq(left, Constant(zero)), ifTrue, ifFalse) if zero == 0 =>
        jzero(left, ifTrue.name); builder += AsmJump(ifFalse.name)
      case JumpIf(Eq(Constant(zero), right), ifTrue, ifFalse) if zero == 0 =>
        jzero(right, ifTrue.name); builder += AsmJump(ifFalse.name)
      case JumpIf(Le(left, Constant(zero)), ifTrue, ifFalse) if zero == 0 =>
        jzero(left, ifTrue.name); builder += AsmJump(ifFalse.name)
      case JumpIf(Ge(Constant(zero), right), ifTrue, ifFalse) if zero == 0 =>
        jzero(right, ifTrue.name); builder += AsmJump(ifFalse.name)
      case JumpIf(Gt(left, Constant(zero)), ifTrue, ifFalse) if zero == 0 =>
        jzero(left, ifFalse.name); builder += AsmJump(ifTrue.name)
      case JumpIf(Lt(Constant(zero), right), ifTrue, ifFalse) if zero == 0 =>
        jzero(right, ifFalse.name); builder += AsmJump(ifTrue.name)
      case JumpIf(Gt(Constant(zero), _), _, ifFalse) if zero == 0 =>
        builder += AsmJump(ifFalse.name)
      case JumpIf(Lt(_, Constant(zero)), _, ifFalse) if zero == 0 =>
        builder += AsmJump(ifFalse.name)
      case JumpIf(Ge(_, Constant(zero)), ifTrue, _) if zero == 0 =>
        builder += AsmJump(ifTrue.name)
      case JumpIf(Le(Constant(zero), _), ifTrue, _) if zero == 0 =>
        builder += AsmJump(ifTrue.name)
    }

    def plainTranslate(instruction: BranchInstruction): Unit = instruction match {
      case Jump(block) =>
        builder += AsmJump(block.name)
      case JumpIf(Eq(left, right), ifTrue, ifFalse) =>
        jumpNe(left, right, ifFalse.name); builder += AsmJump(ifTrue.name)
      case JumpIf(Ne(left, right), ifTrue, ifFalse) =>
        jumpNe(left, right, ifTrue.name); builder += AsmJump(ifFalse.name)
      case JumpIf(Le(left, right), ifTrue, ifFalse) =>
        jumpLe(left, right, ifTrue.name); builder += AsmJump(ifFalse.name)
      case JumpIf(Ge(left, right), ifTrue, ifFalse) =>
        jumpGe(left, right, ifTrue.name); builder += AsmJump(ifFalse.name)
      case JumpIf(Lt(left, right), ifTrue, ifFalse) =>
        jumpLt(left, right, ifTrue.name); builder += AsmJump(ifFalse.name)
      case JumpIf(Gt(left, right), ifTrue, ifFalse) =>
        jumpGt(left, right, ifTrue.name); builder += AsmJump(ifFalse.name)
      case Halt =>
        builder += AsmHalt
    }

    simplifyCond.applyOrElse(instruction, plainTranslate)
  }
}
