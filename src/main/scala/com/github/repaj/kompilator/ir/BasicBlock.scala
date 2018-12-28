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

package com.github.repaj.kompilator.ir

import scala.collection.mutable
import scala.util.hashing.MurmurHash3

/**
  * An implementation of basic block.
  *
  * A basic block is a sequence of instructions
  * that has one entry point and one exit point,
  * that is, there is no branch from the middle
  * of basic block.
  *
  * @param name the name of this block
  *             reflected as label name
  * @param list an instruction list that ends
  *             with branching instruction
  * @author Konrad Kleczkowski
  */
case class BasicBlock(name: String, list: mutable.Buffer[Instruction] = mutable.Buffer.empty) {
  /**
    * Returns the string representation of this basic block.
    */
  override final def toString: String =
    (s"$name:\n" /: list) { (str, inst) => str + s"\t$inst\n" }

  /**
    * Appends new instruction to this block and returns this block.
    */
  def append(instruction: Instruction): this.type = {
    list += instruction
    this
  }
}