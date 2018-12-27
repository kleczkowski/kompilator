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

  private def emitBinary(instruction: BinaryInstruction): Unit = instruction match {
    case Add(left, right, result) => assign(add(left, right), result)
    case Sub(left, right, result) => assign(sub(left, right), result)
    case Mul(left, right, result) => assign(longMul(left, right), result)
    case Div(left, right, result) => assign(longDiv(left, right), result)
    case Rem(left, right, result) => assign(longRem(left, right), result)
  }

  private implicit def op2desc(operand: Operand): DescriptorEntry = operand match {
    case Name(entry: VariableEntry) => DescVar(entry)
    case Temp(id) => DescTemp(id)
    case oth => throw new IllegalArgumentException(s"operand = $oth")
  }

  private def emitBranch(instruction: BranchInstruction): Unit = instruction match {
    case Jump(block) => builder += AsmJump(block.name)
    case JumpIf(Eq(left, right), ifTrue, ifFalse) => jumpNe(left, right, ifFalse.name); builder += AsmJump(ifTrue.name)
    case JumpIf(Ne(left, right), ifTrue, ifFalse) => jumpNe(left, right, ifTrue.name); builder += AsmJump(ifFalse.name)
    case JumpIf(Le(left, right), ifTrue, ifFalse) => jumpLe(left, right, ifTrue.name); builder += AsmJump(ifFalse.name)
    case JumpIf(Ge(left, right), ifTrue, ifFalse) => jumpGe(left, right, ifTrue.name); builder += AsmJump(ifFalse.name)
    case JumpIf(Lt(left, right), ifTrue, ifFalse) => jumpLt(left, right, ifTrue.name); builder += AsmJump(ifFalse.name)
    case JumpIf(Gt(left, right), ifTrue, ifFalse) => jumpGt(left, right, ifTrue.name); builder += AsmJump(ifFalse.name)
    case Halt => builder += AsmHalt
  }
}
