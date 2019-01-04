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

import com.github.repaj.kompilator.Main
import com.github.repaj.kompilator.codegen.analysis.{DataFlowAnalysisResult, DominatorAnalysis, LivenessAnalysis}
import com.github.repaj.kompilator.codegen.constfold._
import com.github.repaj.kompilator.codegen2.RegisterAllocator
import com.github.repaj.kompilator.ir._
import com.github.repaj.kompilator.vm.{AsmBuilder, AsmHalt, AsmJump}

/**
  * An implementation of code generator.
  *
  * @param builder the builder as an output of code
  */
class CodeGenerator(val builder: AsmBuilder)
  extends AsmOutput
    with Macros with RegisterAllocator {

  /**
    * Emits a sequence of basic blocks.
    *
    * @param blocks the sequence of basic blocks
    */
  def emit(blocks: BasicBlock*): Unit = {
    EnablingOptimalization(blocks: _*)
    val livenessMap = LivenessAnalysis(blocks: _*)
    blocks.foreach { b =>
      livenessAnalysis = livenessMap(b)
      emitBlock(b)
    }
  }

  /**
    * Emits a basic block into the builder.
    *
    * @param basicBlock a basic block to emit
    */
  def emitBlock(basicBlock: BasicBlock): Unit = {
    val localLivenessMap = InBlockLivenessAnalysis(basicBlock, currentLiveness.out)
    builder.label(basicBlock.name)
    if (Main.debug) println(basicBlock.name + ":")
    basicBlock.list.foreach { instruction =>
      localLiveness = localLivenessMap(instruction)
      println("\t" + instruction + ":")
      clearSelection()
      instruction match {
        case instruction: LoadStoreInstruction => emitLoadStore(instruction)
        case instruction: BinaryInstruction => emitBinary(instruction)
        case instruction: BranchInstruction => saveVariables(); emitBranch(instruction); resetRegistersState()
      }
    }
  }

  /**
    * Emits a load/store instruction.
    *
    * @param instruction a load/store instruction
    */
  private def emitLoadStore(instruction: LoadStoreInstruction): Unit = instruction match {
    case Get(destination) => seize(get(), destination)
    case Put(source) => put(source)
    case Move(source: Name, destination) => seize(copy(source), destination)
    case Move(source: Temp, destination) => seize(copy(source), destination)
    case Move(source: Constant, destination) => seize(load(source), destination)
    case IndexedLoad(base, offset, destination) => seize(load(base, offset), destination)
    case IndexedStore(source, base, offset) => store(base, offset, source)
  }

  /**
    * Emits a binary instruction.
    *
    * @param instruction a binary instruction
    */
  private def emitBinary(instruction: BinaryInstruction): Unit = {
    def emitIdiom: PartialFunction[BinaryInstruction, Unit] = {
      case Add(left, Constant(one), result) if one == 1 && left == result => seize(incDestructive(left), result)
      case Add(Constant(one), right, result) if one == 1 && right == result=> seize(incDestructive(right), result)
      case Add(left, Constant(one), result) if one == 1 => seize(inc(left), result)
      case Add(Constant(one), right, result) if one == 1 => seize(inc(right), result)
      case Sub(left, Constant(one), result) if one == 1 && left == result => seize(decDestructive(left), result)
      case Sub(left, Constant(one), result) if one == 1 => seize(dec(left), result)
      case Add(left, right, result) if left == result => seize(addDestructive(left, right), result)
      case Add(left, right, result) if right == result => seize(addDestructive(right, left), result)
      case Sub(left, right, result) if left == result => seize(subDestructive(left, right), result)
      case Mul(left, Constant(two), result) if two == 2 && left == result => seize(twiceDestructive(left), result)
      case Mul(Constant(two), right, result) if two == 2 && right == result => seize(twiceDestructive(right), result)
      case Rem(left, Constant(two), result) if two == 2 => seize(rem2(left), result)
      case Div(left, Constant(two), result) if two == 2 && left == result => seize(halfDestructive(left), result)
    }
    def plainEmit(instruction: BinaryInstruction): Unit = instruction match {
      case Add(left, right, result) => seize(add(left, right), result)
      case Sub(left, right, result) => seize(sub(left, right), result)
      case Mul(left, right, result) => seize(longMul(left, right), result)
      case Div(left, right, result) => seize(longDiv(left, right), result)
      case Rem(left, right, result) => seize(longRem(left, right), result)
    }
    emitIdiom.applyOrElse(instruction, plainEmit)
  }

  /**
    * Emits a branch instruction.
    *
    * @param instruction a branch instruction.
    */
  private def emitBranch(instruction: BranchInstruction): Unit = {
    def emitIdiom: PartialFunction[BranchInstruction, Unit] = {
      case JumpIf(Eq(left, Constant(zero)), ifTrue, ifFalse) if zero == 0 => jzero(left, ifTrue.name); builder += AsmJump(ifFalse.name)
      case JumpIf(Eq(Constant(zero), right), ifTrue, ifFalse) if zero == 0 => jzero(right, ifTrue.name); builder += AsmJump(ifFalse.name)
      case JumpIf(Ne(left, Constant(zero)), ifTrue, ifFalse) if zero == 0 => jzero(left, ifFalse.name); builder += AsmJump(ifTrue.name)
      case JumpIf(Ne(Constant(zero), right), ifTrue, ifFalse) if zero == 0 => jzero(right, ifFalse.name); builder += AsmJump(ifTrue.name)
      case JumpIf(Gt(left, Constant(zero)), ifTrue, ifFalse) if zero == 0 => jzero(left, ifFalse.name); builder += AsmJump(ifTrue.name)
      case JumpIf(Lt(Constant(zero), right), ifTrue, ifFalse) if zero == 0 => jzero(right, ifFalse.name); builder += AsmJump(ifTrue.name)
    }
    def plainEmit(instruction: BranchInstruction): Unit = instruction match {
      case Jump(block) => builder += AsmJump(block.name)
      case JumpIf(Eq(left, right), ifTrue, ifFalse) => jumpNe(left, right, ifFalse.name); builder += AsmJump(ifTrue.name)
      case JumpIf(Ne(left, right), ifTrue, ifFalse) => jumpNe(left, right, ifTrue.name); builder += AsmJump(ifFalse.name)
      case JumpIf(Le(left, right), ifTrue, ifFalse) => jumpLe(left, right, ifTrue.name); builder += AsmJump(ifFalse.name)
      case JumpIf(Ge(left, right), ifTrue, ifFalse) => jumpGe(left, right, ifTrue.name); builder += AsmJump(ifFalse.name)
      case JumpIf(Lt(left, right), ifTrue, ifFalse) => jumpLt(left, right, ifTrue.name); builder += AsmJump(ifFalse.name)
      case JumpIf(Gt(left, right), ifTrue, ifFalse) => jumpGt(left, right, ifTrue.name); builder += AsmJump(ifFalse.name)
      case Halt => builder += AsmHalt
    }
    emitIdiom.applyOrElse(instruction, plainEmit)
  }

  /**
    * Returns the liveness analysis for a block.
    */
  def currentLiveness: DataFlowAnalysisResult[Operand] = livenessAnalysis

  def currentLocalLiveness: Set[Operand] = localLiveness

  /**
    * Returns the current block being emitted.
    */
  def currentBlock: BasicBlock = currBlock

  private var currBlock: BasicBlock = _

  private var livenessAnalysis: DataFlowAnalysisResult[Operand] = _

  private var localLiveness: Set[Operand] = _

  private var dominatorAnalysis: Map[BasicBlock, Set[BasicBlock]] = _
}
