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

package com.github.repaj.kompilator.codegen

import com.github.repaj.kompilator.ir.Operand
import com.github.repaj.kompilator.vm._

import scala.collection.mutable

/**
  * An implementation of common operations in language.
  *
  * @author Konrad Kleczkowski
  */
private[codegen] trait Macros extends MemoryManager {
  /**
    * Returns the builder as an output.
    */
  protected def builder: AsmBuilder

  /**
    * Emits `GET` and yields to the register.
    *
    * @return the result register
    */
  protected final def get(): Register = {
    val reg = allocReg()
    builder += AsmGet(reg)
    reg
  }

  /**
    * Emits `PUT`.
    */
  protected final def put(source: Operand): Unit = {
    builder += AsmPut(load(source))
  }

  /**
    * Emits addition of `left` and `right` that yields to the register.
    *
    * @param left  the left operand
    * @param right the right operand
    * @return the result register
    */
  protected final def add(left: Operand, right: Operand): Register = {
//    val begin = getLabel("macro.add.begin")
    val reg = allocReg()
//    builder.label(begin)
    builder += AsmCopy(reg, load(left))
    builder += AsmAdd(reg, load(right))
    reg
  }

  /**
    * Emits addition of `left` and `right` that yields to the register.
    *
    * @param left  the left operand
    * @param right the right operand
    * @return the result register
    */
  protected final def sub(left: Operand, right: Operand): Register = {
//    val begin = getLabel("macro.sub.begin")
    val reg = allocReg()
//    builder.label(begin)
    builder += AsmCopy(reg, load(left))
    builder += AsmSub(reg, load(right))
    reg
  }

  /**
    * Emits multiplication of `left` and `right` that yields to the register.
    *
    * @param left  the left operand
    * @param right the right operand
    * @return the result register
    */
  protected final def longMul(left: Operand, right: Operand): Register = {
    val a = allocReg()
    val b = allocReg()
    val result = allocReg()

    val begin = getLabel("macro.mul.begin")
    val loop = getLabel("macro.mul.loop")
    val rest = getLabel("macro.mul.rest")
    val odd = getLabel("macro.mul.odd")
    val end = getLabel("macro.mul.end")

    builder.label(begin)
    builder += AsmCopy(a, load(left))
    builder += AsmCopy(b, load(right))
    builder += AsmSub(result, result)
    builder.label(loop)
    builder += AsmJzero(b, end)
    builder += AsmJodd(b, odd)
    builder.label(rest)
    builder += AsmAdd(a, a)
    builder += AsmHalf(b)
    builder += AsmJump(loop)
    builder.label(odd)
    builder += AsmAdd(result, a)
    builder += AsmJump(rest)
    builder.label(end)
    result
  }

  /**
    * Emits division of `left` and `right` that yields quotient to the register.
    *
    * @param left  the left operand
    * @param right the right operand
    * @return the result register
    */
  protected final def longDiv(left: Operand, right: Operand): Register = longDiv(left, right, isRem = false)

  /**
    * Emits division of `left` and `right` that yields reminder to the register.
    *
    * @param left  the left operand
    * @param right the right operand
    * @return the result register
    */
  protected final def longRem(left: Operand, right: Operand): Register = longDiv(left, right, isRem = true)

  private def longDiv(leftOp: Operand, rightOp: Operand, isRem: Boolean): Register = {
    val left = load(leftOp)
    val right = load(rightOp)

    val dividend = allocReg()
    val divisor = allocReg()
    val quotient = allocReg()
    val k = allocReg()
    val cmp = allocReg()

    val begin = getLabel("macro.div.begin")
    val kIncLoop = getLabel("macro.div.kincloop")
    val kDecLoop = getLabel("macro.div.kdecloop")
    val end = getLabel("macro.div.end")
    val zeroEnd = getLabel("macro.div.zeroend")

    builder.label(begin)
    builder += AsmSub(quotient, quotient)
    builder += AsmJzero(right, zeroEnd)
    builder += AsmCopy(divisor, right)
    builder += AsmCopy(dividend, left)
    builder += AsmSub(k, k)
    builder.label(kIncLoop)
    builder += AsmCopy(cmp, dividend)
    builder += AsmInc(cmp)
    builder += AsmSub(cmp, divisor)
    builder += AsmJzero(cmp, kDecLoop)
    builder += AsmAdd(divisor, divisor)
    builder += AsmInc(k)
    builder += AsmJump(kIncLoop)
    builder.label(kDecLoop)
    builder += AsmJzero(k, end)
    builder += AsmDec(k)
    builder += AsmHalf(divisor)
    builder += AsmAdd(quotient, quotient)
    builder += AsmCopy(cmp, dividend)
    builder += AsmInc(cmp)
    builder += AsmSub(cmp, divisor)
    builder += AsmJzero(cmp, kDecLoop)
    builder += AsmSub(dividend, divisor)
    builder += AsmInc(quotient)
    builder += AsmJump(kDecLoop)
    builder.label(zeroEnd)
    builder += AsmSub(dividend, dividend)
    builder.label(end)
    if (isRem) dividend else quotient
  }

  /**
    * Emits jump for `<=`.
    *
    * @param leftOp  the left operand
    * @param rightOp the right operand
    * @param label   the label name
    */
  protected final def jumpLe(leftOp: Operand, rightOp: Operand, label: String): Unit = {
//    val begin = getLabel("macro.jge.begin")
    val left = load(leftOp)
    val right = load(rightOp)
    val cmp = allocReg()
//    builder.label(begin)
    builder += AsmCopy(cmp, left)
    builder += AsmSub(cmp, right)
    builder.comment(s"-> $label")
    builder += AsmJzero(cmp, label)
  }

  /**
    * Emits jump for `>=`.
    *
    * @param leftOp  the left operand
    * @param rightOp the right operand
    * @param label   the label name
    */
  protected final def jumpGe(leftOp: Operand, rightOp: Operand, label: String): Unit = {
//    val begin = getLabel("macro.jge.begin")
    val left = load(leftOp)
    val right = load(rightOp)
    val cmp = allocReg()
//    builder.label(begin)
    builder += AsmCopy(cmp, right)
    builder += AsmSub(cmp, left)
    builder.comment(s"-> $label")
    builder += AsmJzero(cmp, label)
  }

  /**
    * Emits jump for `>`.
    *
    * @param leftOp  the left operand
    * @param rightOp the right operand
    * @param label   the label name
    */
  protected final def jumpGt(leftOp: Operand, rightOp: Operand, label: String): Unit = {
//    val begin = getLabel("macro.jge.begin")
    val left = load(leftOp)
    val right = load(rightOp)
    val cmp = allocReg()
//    builder.label(begin)
    builder += AsmCopy(cmp, right)
    builder += AsmInc(cmp)
    builder += AsmSub(cmp, left)
    builder.comment(s"-> $label")
    builder += AsmJzero(cmp, label)
  }

  /**
    * Emits jump for `<`.
    *
    * @param leftOp  the left operand
    * @param rightOp the right operand
    * @param label   the label name
    */
  protected final def jumpLt(leftOp: Operand, rightOp: Operand, label: String): Unit = {
//    val begin = getLabel("macro.jge.begin")
    val left = load(leftOp)
    val right = load(rightOp)
    val cmp = allocReg()
//    builder.label(begin)
    builder += AsmCopy(cmp, left)
    builder += AsmInc(cmp)
    builder += AsmSub(cmp, right)
    builder.comment(s"-> $label")
    builder += AsmJzero(cmp, label)
  }

  /**
    * Emits jump for `!=`.
    *
    * @param leftOp  the left operand
    * @param rightOp the right operand
    * @param label   the label name
    */
  protected final def jumpNe(leftOp: Operand, rightOp: Operand, label: String): Unit = {
//    val begin = getLabel("macro.jge.begin")
    val left = load(leftOp)
    val right = load(rightOp)
    val cmp = allocReg()
//    builder.label(begin)
    builder += AsmCopy(cmp, right)
    builder += AsmInc(cmp)
    builder += AsmSub(cmp, left)
    builder += AsmJzero(cmp, label)
    builder += AsmCopy(cmp, left)
    builder += AsmInc(cmp)
    builder += AsmSub(cmp, right)
    builder.comment(s"-> $label")
    builder += AsmJzero(cmp, label)
  }

  private def getLabel(prefix: String): String = {
    val count = labelTable.getOrElse(prefix, 0)
    labelTable(prefix) = labelTable.getOrElse(prefix, 0) + 1
    s"$prefix$count"
  }

  private val labelTable = new mutable.HashMap[String, Int]
}
