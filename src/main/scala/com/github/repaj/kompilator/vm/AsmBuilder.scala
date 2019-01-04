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

import java.io.PrintWriter

import com.github.repaj.kompilator.Main

import scala.collection.mutable

/**
  * Builds low-level VM code and allows
  * to render it to the output stream.
  *
  * @author Konrad Kleczkowski
  */
class AsmBuilder(list: mutable.Buffer[AsmInstruction] = mutable.Buffer.empty,
                 labelTable: mutable.Buffer[(String, Int)] = mutable.Buffer.empty,
                 commentTable: mutable.Map[Int, String] = mutable.Map.empty) {
  /**
    * Puts the logical label after the last inserted instruction.
    *
    * @param name the name of label
    */
  def label(name: String): Unit = labelTable += (name -> list.size)

  /**
    * Puts comment at the end.
    */
  def comment(text: String): Unit = commentTable(list.size) = commentTable.getOrElse(list.size, "") + text + " *** "

  /**
    * Inserts the instruction
    * at the end of instruction list.
    *
    * @param instruction the instruction to insert
    */
  def +=(instruction: AsmInstruction): Unit = {
    list += instruction
    if (Main.debug) println("\t\t" + instruction)
  }

  /**
    * Render the built code into
    * the character output stream.
    *
    * @param out the output stream
    */
  def render(out: PrintWriter): Unit = {
    val invLabelTable = labelTable.map(_.swap).groupBy(_._1).map({ case (k, v) => (k, v.map(_._2)) })
    for (i <- list.indices) {
      if (Main.debug && (invLabelTable contains i)) out.print(s"# ${invLabelTable(i).mkString(", ")}:\n")
      list(i) match {
//        case AsmJump(label) if labelTable.exists(p => p._1 == label && p._2 == i + 1) => ()
//        case AsmJzero(_, label) if labelTable.exists(p => p._1 == label && p._2 == i + 1) => ()
//        case AsmJodd(_, label) if labelTable.exists(p => p._1 == label && p._2 == i + 1) => ()
        case oth => out.print(oth.render(invLabelTable) + "\n")
      }
      if (Main.debug && (commentTable contains i)) out.print(s"   # ${commentTable(i)}\n")
    }
  }
}
