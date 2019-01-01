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

object LivenessAnalysis {
  def apply(blocks: BasicBlock*): Map[BasicBlock, DataFlowAnalysisResult[Operand]] = {
    val in = new mutable.HashMap[BasicBlock, mutable.Set[Operand]]
    val out = new mutable.HashMap[BasicBlock, mutable.Set[Operand]]
    for (block <- blocks) {
      in(block) = mutable.Set.empty[Operand]
      out(block) = mutable.Set.empty[Operand]
    }
    var changed = true
    while (changed) {
      changed = false
      for (block <- blocks) {
        val blockOut = (mutable.Set.empty[Operand] /: block.successors) { (set, bb) => set ++= in(bb) }
        val blockIn = mutable.Set(blockOut.toSeq: _*)
        for (inst <- block.list.reverse) {
          inst.defines.foreach(blockIn -= _)
          inst.uses.foreach(blockIn += _)
        }
        if (blockIn != in(block) || blockOut != out(block)) changed = true
        in(block) = blockIn
        out(block) = blockOut
      }
    }
    blocks.map(b => (b, DataFlowAnalysisResult(in(b).toSet, out(b).toSet))).toMap
  }
}
