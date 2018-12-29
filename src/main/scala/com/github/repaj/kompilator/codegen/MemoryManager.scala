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

import com.github.repaj.kompilator.StdOut
import com.github.repaj.kompilator.SymbolTable.{ArrayEntry, VariableEntry}
import com.github.repaj.kompilator.analysis.NextUseInfoAnalysis
import com.github.repaj.kompilator.analysis.NextUseInfoAnalysis.{Dead, LiveStatus}
import com.github.repaj.kompilator.analysis.dataflow.DataFlowAnalysisResult
import com.github.repaj.kompilator.ir._
import com.github.repaj.kompilator.vm._

import scala.collection.mutable

/**
  * Trait that facilities memory managing during
  * the code generation.
  *
  * Each variable and temporary are connected with available
  * locations of given variable or temporary. Always
  * the most preferable location is chosen during the
  * translation. These descriptors are called
  * ''location descriptors''.
  *
  * Memory manager is inspired by ''a simple code generator'',
  * that can be found in section 8.6 of Dragonbook by
  * A. Aho, M. Lam, R. Sethi, J. Ullman.
  *
  * @author Konrad Kleczkowski
  */
private[codegen] trait MemoryManager {
  /**
    * Returns the builder as an output.
    */
  protected def builder: AsmBuilder


  /**
    * Stores the source operand to the array at given index.
    *
    * @param source the source operand
    * @param entry  the array described by this entry
    * @param idx    the index operand
    */
  protected final def arrayStore(source: Operand, entry: ArrayEntry, idx: Operand): Unit = {
    lea(entry, idx)
    builder += AsmStore(load(source))
  }

  /**
    * Loads the array at given index to the register.
    *
    * @param entry the array described by this entry
    * @param idx   the index operand
    * @return a register with loaded value
    */
  protected final def arrayLoad(entry: ArrayEntry, idx: Operand): Register = {
    val reg = allocReg()
    lea(entry, idx)
    builder += AsmLoad(reg)
    for (entry <- locationDesc.keys) locationDesc(entry) -= RegisterLocation(reg)
    reg
  }

  private def lea(entry: ArrayEntry, idx: Operand): Unit = {
    val absBase = getAddress(entry)
    val relBase = absBase - entry.startIndex
    val idxReg = load(idx)
    builder += AsmCopy(Register.A, idxReg)
    if (relBase != 0) {
      val relBaseReg = loadConst(relBase.abs)
      builder += (if (relBase > 0) AsmAdd(Register.A, relBaseReg) else AsmSub(Register.A, relBaseReg))
    }
  }

  /**
    * Changes the ownership of the register
    * assigning it exclusively to the variable
    * described by entry.
    *
    * @param source the register
    * @param entry  the variable described by this entry
    */
  protected final def assign(source: Register, entry: DescriptorEntry): Unit = {
    for (entry <- locationDesc.keys) locationDesc(entry) -= RegisterLocation(source)
    locationDesc(entry) = mutable.Set(RegisterLocation(source))
  }

  /**
    * Adds the register ownership to
    * variable described by entry.
    *
    * @param source the source register
    * @param entry  the variable described by this entry
    */
  protected final def copy(source: Register, entry: DescriptorEntry): Unit = {
    locationDesc.getOrElseUpdate(entry, mutable.Set.empty) += RegisterLocation(source)
  }

  /**
    * Load an operand and obtain register
    * with operand value.
    *
    * @param operand the operand to load
    * @return a register with the loaded operand
    */
  protected final def load(operand: Operand): Register = operand match {
    case Constant(value) => loadConst(value)
    case Name(entry: VariableEntry) => loadVar(DescVar(entry))
    case Temp(id) => loadVar(DescTemp(id))
    case oth => throw new IllegalArgumentException(s"operand: $oth")
  }

  private def loadConst(value: BigInt): Register = {
    val reg = allocReg()
    const(reg, value)
    for (entry <- locationDesc.keys) locationDesc(entry) -= RegisterLocation(reg)
    reg
  }

  private def loadVar(entry: DescriptorEntry): Register = {
    val preferableLocation = locationDesc(entry)
      .find(_.isInstanceOf[RegisterLocation])
      .orElse(locationDesc(entry).find(_.isInstanceOf[MemoryLocation]))

    preferableLocation match {
      case Some(RegisterLocation(register)) => register
      case Some(MemoryLocation(address)) => loadFromMem(address, entry)
      case None => entry match {
        case DescVar(ve) =>
          StdOut.error(ve.location, "variable is not initialized but used")
          loadFromMem(getAddress(entry), entry)
        case _: DescTemp => sys.error("temporary variable is not initialized")
      }
    }
  }

  /**
    * Selects a register that could be modified
    * after selection.
    *
    * If there is no free register, algorithm
    * forces to save register to the memory, so
    * called spilling.
    *
    * Selection of sequence of register is obtained by
    * subsequent calls of this method. Each selected
    * register is suppressed for re-selecting it.
    * To undo this, call [[freeRegs]].
    *
    * @return a selected register
    * @throws UnsupportedOperationException if all registers are selected
    *                                       and it is not able to select new one
    */
  protected final def allocReg(): Register = {
    val allRegisters = Set(Register.values: _*) - Register.A
    val available = allRegisters -- selected

    // Try to select free register.
    def selectFree: Option[Register] =
      available.find(r => !locationDesc.values.exists(_ contains RegisterLocation(r)))

    // Try to select dead variable at given point.
    def selectDead: Option[Register] =
      available.find(r => locationDesc.exists(p => (p._2 contains RegisterLocation(r))
        && nextUseInfo(p._1) == Dead))

    // Spills register to the memory.
    def spill: Register = {
      def penalty(register: Register): Int = locationDesc.values.count(_ == Set(RegisterLocation(register)))

      val victim = available.minBy(penalty)

      // Spill all variables associated to this register only.
      val variablesToSpill = locationDesc
        .filter(_._2 == Set(RegisterLocation(victim)))
        .keys
      for (variable <- variablesToSpill) storeToMem(victim, getAddress(variable), variable)

      victim
    }

    // Run algorithm.
    val reg = selectFree.orElse(selectDead).getOrElse(spill)
    selected += reg
    reg
  }

  protected def updateNextUseInfo(info: Map[DescriptorEntry, LiveStatus]): Unit = {
    nextUseInfo = info
  }

  private var nextUseInfo: Map[DescriptorEntry, LiveStatus] = _

  private def storeToMem(value: Register, address: BigInt, entry: DescriptorEntry): Unit = {
    const(Register.A, address)
    builder += AsmStore(value)
    locationDesc.addBinding(entry, MemoryLocation(address))
  }

  private def loadFromMem(address: BigInt, entry: DescriptorEntry): Register = {
    val reg = allocReg()
    const(Register.A, address)
    builder += AsmLoad(reg)
    for (entry <- locationDesc.keys) locationDesc(entry) -= RegisterLocation(reg)
    locationDesc.addBinding(entry, RegisterLocation(reg))
    reg
  }

  /**
    * Clears selection that were given by subsequent
    * calls of [[allocReg]].
    *
    * The most preferable moment to free registers
    * is after instruction emitting.
    */
  protected final def freeRegs(): Unit = selected.clear()

  /**
    * Spills all variables, that exists only in register, to memory.
    */
  protected final def spillAll(): Unit = {
    val allRegisters = Set(Register.values: _*) - Register.A
    for (reg <- allRegisters) {
      val variablesToSpill = locationDesc
        .filter(_._2 == Set(RegisterLocation(reg)))
        .keys
        .filter(live.out contains _)
        .filter(cannotStayInReg)
        .filter(_.isInstanceOf[DescVar])
      for (variable <- variablesToSpill) storeToMem(reg, getAddress(variable), variable)
    }
  }

  /**
    * Removes register locations from descriptors.
    */
  protected final def clearRegistersState(): Unit = {
    val allRegisters = Set(Register.values: _*) - Register.A
    for (reg <- allRegisters) {
      for (entry <- locationDesc.keys if cannotStayInReg(entry)) locationDesc(entry) -= RegisterLocation(reg)
    }
  }

  private def cannotStayInReg(variable: DescriptorEntry): Boolean = {
    currentBlock.successors.exists(b => !(dom(b) contains currentBlock))
  }

  private def allocMem(size: BigInt): BigInt = {
    val address = addressOffset
    addressOffset += size
    address
  }

  protected var currentBlock: BasicBlock = _
  protected var dom: Map[BasicBlock, Set[BasicBlock]] = _
  protected var live: DataFlowAnalysisResult[DescriptorEntry] = _


  private def getAddress(entry: DescriptorEntry): BigInt =
    addressTable.getOrElseUpdate(entry, allocMem(1))

  private def getAddress(entry: ArrayEntry): BigInt =
    arrayTable.getOrElseUpdate(entry, allocMem(entry.size))

  private def const(destination: Register, value: BigInt): Unit = {
    builder += AsmSub(destination, destination)
    def countBits(value: BigInt): Int = value.toString(2).count(_ == '1')
    val cost = 5 * value.bitLength + countBits(value)
    if (value > cost) {
      for (bit <- value.toString(2)) {
        builder += AsmAdd(destination, destination)
        if (bit == '1') builder += AsmInc(destination)
      }
    } else {
      for (_ <- BigInt(1) to value) {
        builder += AsmInc(destination)
      }
    }
  }

  private val addressTable = new mutable.HashMap[DescriptorEntry, BigInt]

  private val arrayTable = new mutable.HashMap[ArrayEntry, BigInt]

  private var addressOffset: BigInt = 0

  private val locationDesc =
    new mutable.HashMap[DescriptorEntry, mutable.Set[EntryLocation]]
      with mutable.MultiMap[DescriptorEntry, EntryLocation]

  private val selected = new mutable.HashSet[Register]

}
