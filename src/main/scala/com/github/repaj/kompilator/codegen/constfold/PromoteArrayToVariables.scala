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

import com.github.repaj.kompilator.SymbolTable.VariableEntry
import com.github.repaj.kompilator.ir._

import scala.collection.mutable

object PromoteArrayToVariables {
  def apply(blocks: BasicBlock*): Unit = {
    val arrayReads = for {
      block <- blocks
      l@IndexedLoad(_, _, _) <- block.list
    } yield l
    val arrayWrites = for {
      block <- blocks
      s@IndexedStore(_, _, _) <- block.list
    } yield s
    val allBases = arrayReads.map(_.base).toSet union arrayWrites.map(_.base).toSet
    def canBePromoted(base: Operand): Boolean = {
      (arrayReads.isEmpty && arrayWrites.filter(p => p.base == base).forall(_.offset.isInstanceOf[Constant])) ||
      (arrayReads.filter(p => p.base == base).forall(_.offset.isInstanceOf[Constant]) &&
        arrayWrites.filter(p => p.base == base).forall(_.offset.isInstanceOf[Constant]))
    }
    val promotable = for {
      base <- allBases if canBePromoted(base)
    } yield base

    def convert: PartialFunction[Instruction, Instruction] = {
      case IndexedLoad(base: Name, Constant(offset), destination) if promotable contains base =>
        Move(Name(VariableEntry("unvec_" + base.entry.name + "_" + offset,
          null,
          initialized = true)), destination)
      case IndexedStore(source, base: Name, Constant(offset)) if promotable contains base =>
        Move(source, Name(VariableEntry("unvec_" + base.entry.name + "_" + offset,
          null,
          initialized = true)))
    }

    for (block <- blocks) {
      val newList = (mutable.Buffer.empty[Instruction] /: block.list) { (buffer, inst) =>
        buffer += convert.applyOrElse(inst, identity[Instruction])
      }
      block.list.clear()
      block.list ++= newList
    }
  }
}
