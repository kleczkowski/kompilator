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

package com.github.repaj.kompilator.codegen.analysis

import com.github.repaj.kompilator.ir._

import scala.collection.mutable

object Predecessors {
  def apply(blocks: BasicBlock*): Map[BasicBlock, Set[BasicBlock]] = {
    val predecessors = new mutable.HashMap[BasicBlock, mutable.Set[BasicBlock]]
      with mutable.MultiMap[BasicBlock, BasicBlock]
    for (block <- blocks) {
      block.list.last match {
        case instruction: BranchInstruction => instruction match {
          case Jump(b) =>
            predecessors.addBinding(b, block)
          case JumpIf(_, ifTrue, ifFalse) =>
            predecessors.addBinding(ifTrue, block)
            predecessors.addBinding(ifFalse, block)
          case _ => ()
        }
        case _ => throw new IllegalStateException
      }
    }
    predecessors.map(p => (p._1, p._2.toSet)).toMap
  }
}
