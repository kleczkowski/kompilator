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

package com.github.repaj.kompilator.vm

import scala.collection.mutable

/**
  * Base class for low-level VM instructions.
  *
  * @author Konrad Kleczkowski
  */
sealed abstract class AsmInstruction {
  /**
    * Returns the string representation of VM instruction
    * that is understandable for VM.
    *
    * @param labelTable the label table for resolving logical label names
    *                   to the raw addresses, mainly maintained by [[AsmBuilder]]
    * @return rendered instruction as VM instruction string
    */
  final def render(labelTable: Map[Int, mutable.Buffer[String]]): String = this match {
    case AsmGet(dst) => s"GET $dst"
    case AsmPut(src) => s"PUT $src"
    case AsmLoad(dst) => s"LOAD $dst"
    case AsmStore(src) => s"STORE $src"
    case AsmCopy(dst, src) => s"COPY $dst $src"
    case AsmAdd(dst, src) => s"ADD $dst $src"
    case AsmSub(dst, src) => s"SUB $dst $src"
    case AsmHalf(dst) => s"HALF $dst"
    case AsmInc(dst) => s"INC $dst"
    case AsmDec(dst) => s"DEC $dst"
    case AsmJump(label) => s"JUMP ${labelTable.filter(_._2 contains label).keys.head}"
    case AsmJzero(cmp, label) => s"JZERO $cmp ${labelTable.filter(_._2 contains label).keys.head}"
    case AsmJodd(cmp, label) => s"JODD $cmp ${labelTable.filter(_._2 contains label).keys.head}"
    case AsmHalt => "HALT"
  }
}

/**
  * `GET` instruction.
  */
case class AsmGet(dst: Register) extends AsmInstruction

/**
  * `PUT` instruction.
  */
case class AsmPut(src: Register) extends AsmInstruction

/**
  * `LOAD` instruction.
  */
case class AsmLoad(dst: Register) extends AsmInstruction

/**
  * `STORE` instruction.
  */
case class AsmStore(src: Register) extends AsmInstruction

/**
  * `COPY` instruction.
  */
case class AsmCopy(dst: Register, src: Register) extends AsmInstruction

/**
  * `ADD` instruction.
  */
case class AsmAdd(dst: Register, src: Register) extends AsmInstruction

/**
  * `SUB` instruction.
  */
case class AsmSub(dst: Register, src: Register) extends AsmInstruction

/**
  * `HALF` instruction.
  */
case class AsmHalf(dst: Register) extends AsmInstruction

/**
  * `INC` instruction.
  */
case class AsmInc(dst: Register) extends AsmInstruction

/**
  * `DEC` instruction.
  */
case class AsmDec(dst: Register) extends AsmInstruction

/**
  * `JUMP` instruction.
  */
case class AsmJump(label: String) extends AsmInstruction

/**
  * `JZERO` instruction.
  */
case class AsmJzero(cmp: Register, label: String) extends AsmInstruction

/**
  * `JODD` instruction.
  */
case class AsmJodd(cmp: Register, label: String) extends AsmInstruction

/**
  * `HALT` instruction.
  */
case object AsmHalt extends AsmInstruction