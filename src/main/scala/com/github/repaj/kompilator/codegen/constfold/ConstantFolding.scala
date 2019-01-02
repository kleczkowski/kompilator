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

package com.github.repaj.kompilator.codegen.constfold

import com.github.repaj.kompilator.{Main, SymbolTable}
import com.github.repaj.kompilator.codegen.analysis.{DataFlowAnalysisResult, LivenessAnalysis, ReachingDefinitions}
import com.github.repaj.kompilator.ir._

import scala.collection.mutable
import scala.language.implicitConversions

/**
  * Preforms a constant propagation with constant folding.
  *
  * Uses reaching definitions to preform
  * global optimization.
  *
  * @author Konrad Kleczkowski
  */
object ConstantFolding {
  def apply(blocks: BasicBlock*): Boolean = {
    var changed = true
    var everChanged = false
    while (changed) {
      changed = false
      val reachingDefMap = ReachingDefinitions(blocks: _*)
      for (b <- blocks) {
        changed = changed || applyBlock(b, reachingDefMap(b))
        if (changed) everChanged = true
      }
    }
    everChanged
  }

  private def applyBlock(block: BasicBlock,
                         reachingDef: DataFlowAnalysisResult[Instruction]): Boolean = {
    val oldList = block.list.toList.toBuffer
    val newList = roundRobinFold(block.list, reachingDef)
    val blockLast = block.list.last
    block.list.clear()
    block.list ++= newList
    oldList != block.list
  }

  private def roundRobinFold(blockList: Seq[Instruction],
                             reachingDef: DataFlowAnalysisResult[Instruction]): Seq[Instruction] = {
    var buffer = blockList.toBuffer
    var changed = true
    while (changed) {
      changed = false
      val constMap = mutable.Map.empty[ExtOperand, BigInt]

      // Map all constants in block.
      buffer
        .flatMap(_.uses)
        .filter(_.isInstanceOf[Constant])
        .map(_.asInstanceOf[Constant])
        .foreach(const => constMap(const) = const.value)

      // Get reaching definitions that has only one instance with lhs
      val maybeConstDefs = reachingDef
        .in
        .groupBy(_.defines.get)
        .filter(_._2.size == 1).map(_._2.head)

      // Initialize with these definitions, that are probably valuable for us.
      maybeConstDefs.foreach(updateConstMap(constMap).lift)

      // Update const map (move statements) or propagate constants and fold if possible.
      val newBuffer = (mutable.Buffer.empty[Instruction] /: blockList) { (buf, inst) =>
        buf += updateConstMap(constMap)
          .orElse(constProp(constMap).andThen(x => constFold(x, constMap)))
          .applyOrElse(inst, identity[Instruction])
      }

      if (newBuffer != buffer) {
        buffer = newBuffer
        changed = true
      }
    }
    buffer
  }

  private def updateConstMap(constMap: mutable.Map[ExtOperand, BigInt]): PartialFunction[Instruction, Instruction] = {
    case IndexedLoad(base, offset, destination) if (constMap contains offset) && (constMap contains ExtArrayRef(base, constMap(offset))) =>
      constMap(destination) = constMap(ExtArrayRef(base, constMap(offset)))
      Move(Constant(constMap(ExtArrayRef(base, constMap(offset)))), destination)
    case IndexedStore(source, base, offset) if (constMap contains source) && (constMap contains offset) =>
      constMap(ExtArrayRef(base, constMap(offset))) = constMap(source)
      IndexedStore(Constant(constMap(source)), base, Constant(constMap(offset)))
    case Move(source, destination) if constMap contains source =>
      constMap(destination) = constMap(source)
      Move(Constant(constMap(source)), destination)
    case idxLd@IndexedLoad(_, _, destination) =>
      constMap -= destination
      idxLd
    case idxSt@IndexedStore(_, base, Constant(offset)) =>
      constMap -= ExtArrayRef(base, offset)
      idxSt
    case mv@Move(_, destination) =>
      constMap -= destination
      mv
  }

  private def constProp(constMap: mutable.Map[ExtOperand, BigInt]): PartialFunction[Instruction, Instruction] = {
    def get(op: Operand): Operand = constMap.get(op).map(Constant).getOrElse(op)
    def propagate: PartialFunction[Instruction, Instruction] = {
      case IndexedStore(source, base, offset) =>
        IndexedStore(get(source), get(base), get(offset))
      case IndexedLoad(base, Constant(offset), destination) if constMap contains ExtArrayRef(base, offset) =>
        Move(Constant(constMap(ExtArrayRef(base, offset))), destination)
      case IndexedLoad(base, offset, destination) if (constMap contains offset) && (constMap contains ExtArrayRef(base, constMap(offset))) =>
        Move(Constant(constMap(ExtArrayRef(base, constMap(offset)))), destination)
      case Add(left, right, result) => Add(get(left), get(right), result)
      case Sub(left, right, result) => Sub(get(left), get(right), result)
      case Mul(left, right, result) => Mul(get(left), get(right), result)
      case Div(left, right, result) => Div(get(left), get(right), result)
      case Rem(left, right, result) => Rem(get(left), get(right), result)
      case JumpIf(Eq(left, right), ifTrue, ifFalse) => JumpIf(Eq(get(left), get(right)), ifTrue, ifFalse)
      case JumpIf(Ne(left, right), ifTrue, ifFalse) => JumpIf(Ne(get(left), get(right)), ifTrue, ifFalse)
      case JumpIf(Le(left, right), ifTrue, ifFalse) => JumpIf(Le(get(left), get(right)), ifTrue, ifFalse)
      case JumpIf(Ge(left, right), ifTrue, ifFalse) => JumpIf(Ge(get(left), get(right)), ifTrue, ifFalse)
      case JumpIf(Lt(left, right), ifTrue, ifFalse) => JumpIf(Lt(get(left), get(right)), ifTrue, ifFalse)
      case JumpIf(Gt(left, right), ifTrue, ifFalse) => JumpIf(Gt(get(left), get(right)), ifTrue, ifFalse)
      case Put(source) => Put(get(source))
    }
    propagate
  }

  private def constFold(instruction: Instruction, constMap: mutable.Map[ExtOperand, BigInt]): Instruction = {
    def sub(a: BigInt, b: BigInt): BigInt = if (a < b) 0 else a - b
    def div(a: BigInt, b: BigInt): BigInt = if (b == 0) 0 else a / b
    def mod(a: BigInt, b: BigInt): BigInt = if (b == 0) 0 else a % b
    instruction match {
      case Add(Constant(left), Constant(right), result) => constMap(result) = left + right; Move(Constant(left + right), result)
      case Sub(Constant(left), Constant(right), result) => constMap(result) = sub(left, right); Move(Constant(sub(left, right)), result)
      case Mul(Constant(left), Constant(right), result) => constMap(result) = left * right; Move(Constant(left * right), result)
      case Div(Constant(left), Constant(right), result) => constMap(result) = div(left, right); Move(Constant(div(left, right)), result)
      case Rem(Constant(left), Constant(right), result) => constMap(result) = mod(left, right); Move(Constant(mod(left, right)), result)

      case Add(left, Constant(zero), result) if zero == 0 => Move(left, result)
      case Add(Constant(zero), right, result) if zero == 0 => Move(right, result)
      case Sub(left, Constant(zero), result) if zero == 0 => Move(left, result)
      case Sub(Constant(zero), _, result) if zero == 0 => constMap(result) = 0; Move(Constant(0), result)
      case Mul(_, Constant(zero), result) if zero == 0 => constMap(result) = 0; Move(Constant(0), result)
      case Mul(Constant(zero), _, result) if zero == 0 => constMap(result) = 0; Move(Constant(0), result)
      case Mul(left, Constant(one), result) if one == 1 => Move(left, result)
      case Mul(Constant(one), right, result) if one == 1 => Move(right, result)
      case Div(_, Constant(zero), result) if zero == 0 => constMap(result) = 0; Move(Constant(0), result)
      case Div(Constant(zero), _, result) if zero == 0 => constMap(result) = 0; Move(Constant(0), result)
      case Div(left, Constant(one), result) if one == 1 => Move(left, result)
      case Div(Constant(one), right, result) if one == 1 => Move(right, result)
      case Rem(_, Constant(zero), result) if zero == 0 => constMap(result) = 0; Move(Constant(0), result)
      case Rem(Constant(zero), _, result) if zero == 0 => constMap(result) = 0; Move(Constant(0), result)
      case Rem(left, Constant(one), result) if one == 1 => Move(left, result)

      case JumpIf(Eq(Constant(left), Constant(right)), ifTrue, _) if left == right => Jump(ifTrue)
      case JumpIf(Eq(Constant(left), Constant(right)), _, ifFalse) if left != right => Jump(ifFalse)
      case JumpIf(Ne(Constant(left), Constant(right)), ifTrue, _) if left != right => Jump(ifTrue)
      case JumpIf(Ne(Constant(left), Constant(right)), _, ifFalse) if left == right => Jump(ifFalse)
      case JumpIf(Le(Constant(left), Constant(right)), ifTrue, _) if left <= right => Jump(ifTrue)
      case JumpIf(Le(Constant(left), Constant(right)), _, ifFalse) if left > right => Jump(ifFalse)
      case JumpIf(Ge(Constant(left), Constant(right)), ifTrue, _) if left >= right => Jump(ifTrue)
      case JumpIf(Ge(Constant(left), Constant(right)), _, ifFalse) if left < right => Jump(ifFalse)
      case JumpIf(Lt(Constant(left), Constant(right)), ifTrue, _) if left < right => Jump(ifTrue)
      case JumpIf(Lt(Constant(left), Constant(right)), _, ifFalse) if left >= right => Jump(ifFalse)
      case JumpIf(Gt(Constant(left), Constant(right)), ifTrue, _) if left >= right => Jump(ifTrue)
      case JumpIf(Gt(Constant(left), Constant(right)), _, ifFalse) if left < right => Jump(ifFalse)
      case oth => oth
    }
  }

  private implicit def op2ext(operand: Operand): ExtOperand = operand match {
    case Constant(value) => ExtConstant(value)
    case Name(entry) => ExtName(entry)
    case Temp(id) => ExtTemp(id)
  }

  private sealed trait ExtOperand

  private case class ExtName(entry: SymbolTable.Entry) extends ExtOperand

  private case class ExtConstant(value: BigInt) extends ExtOperand

  private case class ExtTemp(id: Int) extends ExtOperand

  private case class ExtArrayRef(base: Operand, offset: BigInt) extends ExtOperand

}
