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

import com.github.repaj.kompilator.codegen.analysis.{LivenessAnalysis, ReachingDefinitions}
import com.github.repaj.kompilator.ir._

import scala.collection.mutable

object RemoveDeadCode {
  def apply(blocks: BasicBlock*): Unit = {
    val reachingDef = ReachingDefinitions(blocks: _*)
    val liveness = LivenessAnalysis(blocks: _*)
    for (block <- blocks) {
      val nextUseInfo = InBlockLivenessAnalysis(block, liveness(block).out)
      val reachingOut = reachingDef(block).out
      val newList = mutable.Buffer.empty[Instruction]
      for (inst <- block.list) {
        inst match {
          case i@(_: Put | _: IndexedStore | _: Jump | _: JumpIf | Halt) => newList += i
          case oth@Move(_: Constant, dst) if !(nextUseInfo(oth) contains dst) => ()
          case oth => newList += oth
        }
      }
      block.list.clear()
      block.list ++= newList
    }
  }
}
