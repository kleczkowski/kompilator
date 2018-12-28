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

package com.github.repaj.kompilator.analysis

import com.github.repaj.kompilator.codegen.DescriptorEntry
import com.github.repaj.kompilator.ir._

import scala.collection.mutable

/**
  * Allows to obtain next-use information.
  *
  * @author Konrad Kleczkowski
  */
object NextUseInfoAnalysis {
  /**
    * Runs next-use algorithm to obtain information
    * which variables are dead or lives in basic block
    * with information where is the next usage of defined variable.
    *
    * @param block the basic block
    * @return the next-use information for given basic block
    */
  def apply(block: BasicBlock): Instruction => Map[DescriptorEntry, LiveStatus] = {
    val builder = new mutable.HashMap[Instruction, Map[DescriptorEntry, LiveStatus]]
    val state = new mutable.HashMap[Option[DescriptorEntry], LiveStatus]

    val allVariables = block.list.flatMap(_.operands).flatMap(_.toDescriptor).toSet

    // We assume that all available variables lives at the end of block.
    for (variable <- allVariables) {
      state += Some(variable) -> Live(block.list.last)
    }

    for (inst <- block.list.reverse) {
      inst match {
        case Get(destination) =>
          state += (destination.toDescriptor -> Dead)
        case Put(source) =>
          state += (source.toDescriptor -> Live(inst))
        case Move(source, destination) =>
          state += (destination.toDescriptor -> Dead)
          state += (source.toDescriptor -> Live(inst))
        case IndexedLoad(_, _, destination) =>
          state += (destination.toDescriptor -> Dead)
        case IndexedStore(source, _, _) =>
          state += (source.toDescriptor -> Live(inst))
        case Add(left, right, result) =>
          state += (result.toDescriptor -> Dead)
          state += (left.toDescriptor -> Live(inst))
          state += (right.toDescriptor -> Live(inst))
        case Sub(left, right, result) =>
          state += (result.toDescriptor -> Dead)
          state += (left.toDescriptor -> Live(inst))
          state += (right.toDescriptor -> Live(inst))
        case Mul(left, right, result) =>
          state += (result.toDescriptor -> Dead)
          state += (left.toDescriptor -> Live(inst))
          state += (right.toDescriptor -> Live(inst))
        case Div(left, right, result) =>
          state += (result.toDescriptor -> Dead)
          state += (left.toDescriptor -> Live(inst))
          state += (right.toDescriptor -> Live(inst))
        case Rem(left, right, result) =>
          state += (result.toDescriptor -> Dead)
          state += (left.toDescriptor -> Live(inst))
          state += (right.toDescriptor -> Live(inst))
        case Jump(_) => // do nothing :)
        case JumpIf(condition, _, _) =>
          state += (condition.left.toDescriptor -> Live(inst))
          state += (condition.right.toDescriptor -> Live(inst))
        case Halt => // do nothing :)
      }
      state -= None // remove none key because is useless
      builder += (inst -> state.map(p => (p._1.get, p._2)).toMap)
    }

    builder.toMap
  }

  sealed abstract class LiveStatus

  case object Dead extends LiveStatus

  case class Live(nextUse: Instruction) extends LiveStatus

}
