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

import com.github.repaj.kompilator.SymbolTable
import com.github.repaj.kompilator.SymbolTable.VariableEntry

/**
  * Base class for operands of intermediate instructions.
  *
  * @author Konrad Kleczkowski
  */
sealed abstract class Operand {
  /**
    * Returns the string representation of this operand.
    */
  override final def toString: String = this match {
    case Constant(value) => value.toString()
    case Name(entry) => entry.name
    case Temp(id) => s"_t$id"
  }
}

/**
  * A constant value.
  */
case class Constant(value: BigInt) extends Operand

/**
  * A variable.
  *
  * This operand is reflected in the symbol table.
  *
  * @param entry the symbol table entry
  */
case class Name(entry: SymbolTable.Entry) extends Operand

/**
  * A temporary variable.
  *
  * @param id the unique integer
  *           that describes this temporary
  */
case class Temp(id: Int) extends Operand