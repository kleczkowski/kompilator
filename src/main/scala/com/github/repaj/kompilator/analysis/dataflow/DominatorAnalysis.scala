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

package com.github.repaj.kompilator.analysis.dataflow

import com.github.repaj.kompilator.analysis.util.Predecessors
import com.github.repaj.kompilator.ir.BasicBlock

import scala.collection.mutable

object DominatorAnalysis {
  def apply(blocks: BasicBlock*): Map[BasicBlock, Set[BasicBlock]] = {
    val predecessors = Predecessors(blocks: _*)

    val dom = new mutable.HashMap[BasicBlock, mutable.Set[BasicBlock]]
      with mutable.MultiMap[BasicBlock, BasicBlock]

    var changed = true
    dom.addBinding(blocks.head, blocks.head)
    for (block <- blocks.tail) {
      dom(block) = mutable.Set(block)
    }
    while (changed) {
      changed = false
      for (block <- blocks.tail) {
        val newDom = (mutable.Set(blocks: _*) /: predecessors(block)) { (set, bb) => set intersect dom(bb) }
        newDom += block
        if (newDom != dom(block)) changed = true
        dom(block) = newDom
      }
    }
    dom.map(p => (p._1, p._2.toSet)).toMap
  }
}
