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

import com.github.repaj.kompilator.ir.{BasicBlock, Instruction, Operand}

import scala.collection.mutable

object ReachingDefinitions {
  def apply(blocks: BasicBlock*): Map[BasicBlock, DataFlowAnalysisResult[Instruction]] = {
    val in = new mutable.HashMap[BasicBlock, mutable.Set[Instruction]]
    val out = new mutable.HashMap[BasicBlock, mutable.Set[Instruction]]
    val predecessors = Predecessors(blocks: _*)

    for (block <- blocks) {
      in(block) = mutable.Set.empty[Instruction]
      out(block) = mutable.Set.empty[Instruction]
    }

    var changed = true
    while (changed) {
      changed = false
      for (block <- blocks) {
        val blockIn = (mutable.Set.empty[Instruction] /: predecessors.getOrElse(block, mutable.Set.empty)) {
          (set, bb) => set ++= out(bb)
        }
        val blockOut = mutable.Set(blockIn.toSeq: _*)

        def defs(destination: Operand): Set[Instruction] =
          blocks.flatMap(_.list).filter(_.defines.contains(destination)).toSet
        //        val gen = mutable.Set.empty[Instruction]
        //        val kill = mutable.Set.empty[Instruction]
        for (inst <- block.list) {
          inst.defines.foreach({op => blockOut --= defs(op); blockOut += inst})
        }

        //        blockOut --= kill
        //        blockOut ++= gen

        if (blockIn != in(block) || blockOut != out(block)) changed = true
        in(block) = blockIn
        out(block) = blockOut
      }
    }
    blocks.map(b => (b, DataFlowAnalysisResult(in(b).toSet, out(b).toSet))).toMap
  }
}
