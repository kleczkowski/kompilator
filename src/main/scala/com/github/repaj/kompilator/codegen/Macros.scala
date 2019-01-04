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

import com.github.repaj.kompilator.Main
import com.github.repaj.kompilator.SymbolTable.ArrayEntry
import com.github.repaj.kompilator.codegen2.RegisterAllocator
import com.github.repaj.kompilator.ir.{Constant, Name, Operand}
import com.github.repaj.kompilator.vm._

import scala.collection.mutable

/**
  * An implementation of translation macros.
  *
  * @author Konrad Kleczkowski
  */
trait Macros extends AsmOutput with RegisterAllocator {
  /**
    * Emits an instruction that reads integer from standard input.
    *
    * @return the result register
    */
  protected final def get(): Register = {
    val reg = select()
    builder += AsmGet(reg)
    reg
  }

  /**
    * Emits an instruction that causes
    * printing operand to the standard output.
    *
    * @param source an operand to show
    */
  protected final def put(source: Operand): Unit = {
    builder += AsmPut(load(source))
  }

  /**
    * Emits a copying instruction of an operand.
    *
    * @param source an operand to copy
    * @return a register with the copy of the operand
    */
  protected final def copy(source: Operand): Register = {
    val loadedReg = load(source)
    val copyReg = select()
    builder += AsmCopy(copyReg, loadedReg)
    copyReg
  }


  /**
    * Emits an increment operation on an operand.
    *
    * @param operand an operand to increment
    */
  protected final def incDestructive(operand: Operand): Register = {
    val operandReg = load(operand)
    builder += AsmInc(operandReg)
    operandReg
  }

  /**
    * Emits an increment operation on an operand.
    *
    * @param operand an operand to increment
    */
  protected final def decDestructive(operand: Operand): Register = {
    val operandReg = load(operand)
    builder += AsmDec(operandReg)
    operandReg
  }

  /**
    * Emits an increment operation on an operand.
    *
    * @param operand an operand to increment
    */
  protected final def inc(operand: Operand): Register = {
    val resultReg = select()
    val operandReg = load(operand)
    builder += AsmCopy(resultReg, operandReg)
    builder += AsmInc(resultReg)
    resultReg
  }

  /**
    * Emits an increment operation on an operand.
    *
    * @param operand an operand to increment
    */
  protected final def dec(operand: Operand): Register = {
    val resultReg = select()
    val operandReg = load(operand)
    builder += AsmCopy(resultReg, operandReg)
    builder += AsmDec(resultReg)
    resultReg
  }

  /**
    * Emits an instructiont that adds right operand to left operand
    * with destructing the previous instance of left operand.
    *
    * @param left  the left operand
    * @param right the right operand
    * @return the result register as register of left operand
    */
  protected final def addDestructive(left: Operand, right: Operand): Register = {
    val leftReg = load(left)
    val rightReg = load(right)
    builder += AsmAdd(leftReg, rightReg)
    leftReg
  }

  /**
    * Emits an instructiont that adds right operand to left operand
    * with destructing the previous instance of left operand.
    *
    * @param left  the left operand
    * @param right the right operand
    * @return the result register as register of left operand
    */
  protected final def subDestructive(left: Operand, right: Operand): Register = {
    val leftReg = load(left)
    val rightReg = load(right)
    builder += AsmSub(leftReg, rightReg)
    leftReg
  }

  /**
    * Emits a simple sequence for JZERO jump as a machine idiom.
    *
    * @param operand the operand to test
    * @param label   the label as destiantion of jump.
    */
  protected final def jzero(operand: Operand, label: String): Unit = {
    val operandReg = load(operand)
    builder += AsmJzero(operandReg, label)
  }

  /**
    * Emits an instruction sequence that computes the remainder
    * of division by two on an operand.
    *
    * @param operand the operand
    * @return a register with remainder of two
    */
  protected final def rem2(operand: Operand): Register = {
    val operandReg = load(operand)
    val resultReg = select()

    val isOdd = getLabel("macro.rem2.isOdd")
    val end = getLabel("macro.rem2.end")
    builder += AsmSub(resultReg, resultReg)
    builder += AsmJodd(operandReg, isOdd)
    builder += AsmJump(end)
    builder.label(isOdd)
    builder += AsmInc(resultReg)
    builder.label(end)

    resultReg
  }

  /**
    * Emits an instruction sequence that twices the operand.
    *
    * @param operand the operand
    * @return the register with twiced operand
    */
  protected final def twiceDestructive(operand: Operand): Register = {
    val operandReg = load(operand)
    builder += AsmAdd(operandReg, operandReg)
    operandReg
  }

  /**
    * Emits an instruction sequence that halfs the operand.
    *
    * @param operand the operand
    * @return the register with twiced operand
    */
  protected final def halfDestructive (operand: Operand): Register = {
    val operandReg = load(operand)
    builder += AsmHalf(operandReg)
    operandReg
  }

  /**
    * Emits an instruction that adds
    * two operands and stores to a register
    *
    * @param left  the left operand
    * @param right the right operand
    * @return the result register
    */
  protected final def add(left: Operand, right: Operand): Register = {
    val leftReg = load(left)
    val rightReg = load(right)
    val resultReg = select()
    builder += AsmCopy(resultReg, leftReg)
    builder += AsmAdd(resultReg, rightReg)
    resultReg
  }

  /**
    * Emits an instruction that subtracts
    * two operands and stores to a register.
    *
    * @param left  the left operand
    * @param right the right operand
    * @return the result register
    */
  protected final def sub(left: Operand, right: Operand): Register = {
    val leftReg = load(left)
    val rightReg = load(right)
    val resultReg = select()
    builder += AsmCopy(resultReg, leftReg)
    builder += AsmSub(resultReg, rightReg)
    resultReg
  }

  /**
    * Emits jump for `<=`.
    *
    * @param leftOp  the left operand
    * @param rightOp the right operand
    * @param label   the label name
    */
  protected final def jumpLe(leftOp: Operand, rightOp: Operand, label: String): Unit = {
    val left = load(leftOp)
    val right = load(rightOp)
    val cmp = select()
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
    val left = load(leftOp)
    val right = load(rightOp)
    val cmp = select()
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
    val left = load(leftOp)
    val right = load(rightOp)
    val cmp = select()
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
    val left = load(leftOp)
    val right = load(rightOp)
    val cmp = select()
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
    val left = load(leftOp)
    val right = load(rightOp)
    val cmp = select()
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

  /**
    * Emits multiplication of `left` and `right` that yields to the register.
    *
    * @param left  the left operand
    * @param right the right operand
    * @return the result register
    */
  protected final def longMul(left: Operand, right: Operand): Register = {
    val a = select()
    val b = select()
    val result = select()

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
  protected final def longDiv(left: Operand, right: Operand): Register = longDiv0(left, right, isRem = false)

  /**
    * Emits division of `left` and `right` that yields reminder to the register.
    *
    * @param left  the left operand
    * @param right the right operand
    * @return the result register
    */
  protected final def longRem(left: Operand, right: Operand): Register = longDiv0(left, right, isRem = true)

  /**
    * Emits generic long binary division algorithm.
    *
    * @param leftOp  the left operand
    * @param rightOp the right operand
    * @param isRem   `true` if reminder should be returned
    * @return a register with quotient or remainder, according to the `isRem`
    */
  private def longDiv0(leftOp: Operand, rightOp: Operand, isRem: Boolean): Register = {
    val left = load(leftOp)
    val right = load(rightOp)

    val dividend = select()
    val divisor = select()
    val quotient = select()
    val k = select()
    val cmp = select()

    val begin = getLabel("macro.div.begin")
    val kIncLoop = getLabel("macro.div.kIncLoop")
    val kDecLoop = getLabel("macro.div.kDecLoop")
    val end = getLabel("macro.div.end")
    val zeroEnd = getLabel("macro.div.zeroEnd")

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
    * Emits a constant to the specified register.
    *
    * @param register the target register
    * @param value    the value to be emitted to the given register
    */
  protected final def emitConstant(register: Register, value: BigInt): Unit = {
    if (Main.debug) println(s"\t\t# emiting constant $value")
    def ones(bigInt: BigInt): Int = (0 until bigInt.bitLength).count(bigInt.testBit)

    builder += AsmSub(register, register)
    if (value <= 5 * value.bitLength + ones(value)) {
      ((1: BigInt) to value).foreach { _ =>
        builder += AsmInc(register)
      }
    } else {
      for (bit <- value.toString(2)) {
        builder += AsmAdd(register, register)
        if (bit == '1') builder += AsmInc(register)
      }
    }
  }

  private def getLabel(prefix: String): String = {
    val count = labelTable.getOrElse(prefix, 0)
    labelTable(prefix) = labelTable.getOrElse(prefix, 0) + 1
    s"$prefix$count"
  }

  private val labelTable = new mutable.HashMap[String, Int]
}
