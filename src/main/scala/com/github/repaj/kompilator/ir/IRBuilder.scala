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

/**
  * Allows to create intermediate code in convenient way.
  *
  * This builder manages inserting instructions by
  * selecting the ''active block'' during the IR code creation.
  *
  * Last instruction that is inserted in the active block
  * should be a terminator, that is, should be a
  * control flow instruction such as conditional and
  * unconditional jumps or halting instruction.
  *
  * Note that this builder does not check
  * if generated code is valid. Validation should be
  * preformed by parser during the semantic analysis.
  *
  * @author Konrad Kleczkowski
  */
class IRBuilder {

  /**
    * Appends new instruction to the end of active basic block.
    *
    * @param instruction the instruction to insert
    */
  def +=(instruction: Instruction): Unit = {
    activeBlock.list += instruction
  }

  /**
    * Sets the active block.
    *
    * @param block the block to be set as active
    */
  def setActiveBlock(block: BasicBlock): Unit = {
    activeBlock = block
  }

  /**
    * Returns the active block.
    */
  def getActiveBlock: BasicBlock = activeBlock

  /**
    * Creates new basic block with given name (as label).
    *
    * @param name the name of basic block label
    * @return new basic block that is empty
    */
  def newBlock(name: String): BasicBlock = {
    val count = basicBlockNameMap.getOrElse(name, 0)
    val block = BasicBlock(name + count)
    basicBlockNameMap(name) = count + 1
    blockSet += block
    block
  }

  /**
    * Creates new temporary variable.
    */
  def newTemp: Temp = {
    val temp = Temp(currentId)
    currentId += 1
    temp
  }

  /**
    * Returns all blocks. First block is entry block.
    */
  def getBlocks: mutable.Buffer[BasicBlock] = blockSet

  private var activeBlock: BasicBlock = _
  private val blockSet: mutable.Buffer[BasicBlock] = mutable.Buffer.empty
  private var currentId: Int = 0
  private var basicBlockNameMap: mutable.Map[String, Int] = mutable.Map.empty
}
