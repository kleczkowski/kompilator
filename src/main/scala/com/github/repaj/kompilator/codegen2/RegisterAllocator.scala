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

package com.github.repaj.kompilator.codegen2

import com.github.repaj.kompilator.Main
import com.github.repaj.kompilator.SymbolTable.ArrayEntry
import com.github.repaj.kompilator.codegen.AsmOutput
import com.github.repaj.kompilator.ir.{Constant, Name, Operand}
import com.github.repaj.kompilator.vm._

import scala.collection.mutable

trait RegisterAllocator extends AsmOutput {
  protected def emitConstant(register: Register, value: BigInt): Unit

  def seize(register: Register, operand: Operand): Unit = {
    // Remove the destination register from all locations.
    // Assign the register only to the operand.
    if (Main.debug) println(s"\t\t# [seize] locations before: $locationMap")
    for (v <- locationMap.keys) locationMap(v) -= InRegister(register)
    locationMap(operand) = mutable.Set(InRegister(register))
    selected += register
    if (Main.debug) println(s"\t\t# [seize] locations after: $locationMap")
  }

  def select(): Register = {
    def selectFree: Option[Register] = {
      val available = (Set(Register.values: _*) - Register.A) diff selected
      val occupied = for {
        (_, locations) <- locationMap.toSet
        InRegister(register) <- locations
      } yield register
      val free = available diff occupied
      free.lastOption.foreach(register => if (Main.debug) println(s"\t\t# [selectFree] selected free $register"))
      free.lastOption
    }

    def spill: Register = {
      val candidates = for {
        (variable, locations) <- locationMap
        InRegister(register) <- locations if !selected(register)
      } yield (variable, register)

      assert(candidates.size == candidates.values.toSet.size, "candidates map is bijective")
      val (variable, register) = candidates.minBy(p => getAddress(p._1))
      if (Main.debug) println(s"\t\t# [spill] spill $register seized by $variable")
      store(variable, register)
      register
    }

    val candidate = selectFree.getOrElse(spill)
    selected += candidate
    candidate
  }

  def clearSelection(): Unit = selected.clear()

  def load(operand: Operand): Register = {
    def loadVariable(operand: Operand): Register = {
      val registerLocation = for {
        (`operand`, locations) <- locationMap
        ir@InRegister(_) <- locations
      } yield ir
      val memoryLocation = for {
        (`operand`, locations) <- locationMap
        im@InMemory(_) <- locations
      } yield im
      assert(registerLocation.size == 1
        || memoryLocation.size == 1
        || (registerLocation.isEmpty && memoryLocation.isEmpty), "memory state is well-formed")
      val preference = registerLocation.headOption.orElse(memoryLocation.headOption)
      preference match {
        case Some(InRegister(register)) =>
          if (Main.debug) println(s"\t\t# [load] variable $operand is already in $register")
          selected += register
          register
        case Some(InMemory(address)) => load(operand, address)
        case None => throw new IllegalStateException(s"variable $operand is uninitialized")
      }
    }

    def loadConstant(value: BigInt): Register = {
      val reg = select()
      emitConstant(reg, value)
      if (Main.debug) println(s"\t\t# [loadConstant] locations before: $locationMap")
      for (v <- locationMap.keys) locationMap(v) -= InRegister(reg)
      if (Main.debug) println(s"\t\t# [loadConstant] locations after: $locationMap")
      selected += reg
      reg
    }

    operand match {
      case Constant(value) => loadConstant(value)
      case v => loadVariable(v)
    }
  }

  /**
    * Loads a value from an array.
    *
    * @param base   a base address of an array
    * @param offset an offset
    * @return a register with loaded value
    */
  def load(base: Operand, offset: Operand): Register = {
    val resultReg = select()
    lea(base, offset)
    builder += AsmLoad(resultReg)
    selected += resultReg
    resultReg
  }

  /**
    * Stores a value to an array.
    *
    * @param base   a base address of an array
    * @param offset an offset
    */
  def store(base: Operand, offset: Operand, value: Operand): Unit = {
    val valueReg = load(value)
    lea(base, offset)
    builder += AsmStore(valueReg)
  }

  def saveVariables(): Unit = {
    if (Main.debug) println(s"\t\t# [saveVariables] locations before: $locationMap")
    val candidates = for {
      (variable, locations) <- locationMap if locations.size == 1 if variable.isInstanceOf[Name]
      InRegister(register) <- locations
    } yield (variable, register)
    candidates.foreach(p => store(p._1, p._2))
    if (Main.debug) println(s"\t\t# [saveVariables] locations after: $locationMap")
  }

  def resetRegistersState(): Unit = {
    if (Main.debug) println(s"\t\t# [resetRegistersState] locations before: $locationMap")
    for (reg <- Register.values; v <- locationMap.keys) locationMap(v) -= InRegister(reg)
    if (Main.debug) println(s"\t\t# [resetRegistersState] locations after: $locationMap")
  }

  private def load(operand: Operand, address: BigInt): Register = {
    val reg = select()
    if (Main.debug) println(s"\t\t# [load] loading variable $operand to selected $reg")
    emitConstant(Register.A, address)
    builder += AsmLoad(reg)
    if (Main.debug) println(s"\t\t# [load] locations before: $locationMap")
    for (v <- locationMap.keys) locationMap(v) -= InRegister(reg)
    locationMap.addBinding(operand, InRegister(reg))
    if (Main.debug) println(s"\t\t# [load] locations after: $locationMap")
    reg
  }

  private def store(operand: Operand, register: Register): Unit = {
    val address = getAddress(operand)
    if (Main.debug) println(s"\t\t# [store] storing $register to address $address seized by $operand")
    emitConstant(Register.A, address)
    builder += AsmStore(register)
    if (Main.debug) println(s"\t\t# [store] locations before: $locationMap")
    locationMap(operand) = mutable.Set(InMemory(address))
    if (Main.debug) println(s"\t\t# [store] locations after: $locationMap")
  }

  /**
    * Computes the effective address for given operands to register A
    *
    * @param base   the base address
    * @param offset the offset
    */
  private def lea(base: Operand, offset: Operand): Unit = {
    val Name(entry: ArrayEntry) = base
    val address = getAddress(base)

    if (Main.debug) println(s"\t\t# [lea] computing lea $base[$offset]")
    val offsetReg = load(offset)
    val relativeBase = address - entry.startIndex
    val relativeBaseReg = load(Constant(relativeBase.abs))
    builder += AsmCopy(Register.A, offsetReg)
    if (relativeBase < 0) {
      builder += AsmSub(Register.A, relativeBaseReg)
    } else if (relativeBase > 0) {
      builder += AsmAdd(Register.A, relativeBaseReg)
    }
  }

  private def getAddress(operand: Operand): BigInt = {
    def allocate: BigInt = {
      val oldOffset = addressOffset
      if (Main.debug) println(s"\t\t# [getAddress] allocating memory for $operand; new address is $oldOffset")
      operand match {
        case Name(entry: ArrayEntry) => addressOffset += entry.size
        case _ => addressOffset += 1
      }
      oldOffset
    }

    addressTable.getOrElseUpdate(operand, allocate)
  }

  private val addressTable = new mutable.HashMap[Operand, BigInt]

  private var addressOffset = 0: BigInt

  private sealed trait Location

  private case class InMemory(address: BigInt) extends Location

  private case class InRegister(register: Register) extends Location

  private val locationMap = new mutable.HashMap[Operand, mutable.Set[Location]] with mutable.MultiMap[Operand, Location]

  private val selected = mutable.Set.empty[Register]
}
