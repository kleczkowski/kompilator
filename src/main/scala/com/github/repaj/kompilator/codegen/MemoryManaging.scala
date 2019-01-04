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
import com.github.repaj.kompilator.SymbolTable.ArrayEntry
import com.github.repaj.kompilator.codegen.analysis.DataFlowAnalysisResult
import com.github.repaj.kompilator.ir.{BasicBlock, Constant, Name, Operand}
import com.github.repaj.kompilator.vm.{AsmLoad, AsmStore, Register}

import scala.collection.mutable

/**
  * An implementation of a simple register allocator.
  *
  * Manages the locations of variables
  * and temporaries in memory and registers.
  *
  * @author Konrad Kleczkowski
  * @deprecated Use [[com.github.repaj.kompilator.codegen2.RegisterAllocator]]
  */
trait MemoryManaging extends AsmOutput {
  /**
    * Returns the liveness analysis for a block.
    */
  def liveness(bb: BasicBlock): DataFlowAnalysisResult[Operand]

  /**
    * Returns the dominators of `bb`.
    *
    * @param bb a basic block
    * @return a dominator set of the `bb`
    */
  def dom(bb: BasicBlock): Set[BasicBlock]

  /**
    * Returns the current block being emitted.
    */
  def currentBlock: BasicBlock

  /**
    * Loads an operand so that checks if
    * this operand is already loaded
    * or if needs loading from memory.
    *
    * @param operand an operand to be loaded
    * @return a register that contains value described by the operand
    */
  final def load(operand: Operand): Register = {
    /**
      * Loads constant to register.
      */
    def loadConstant(value: BigInt): Register = {
      val register = select()
      emitConstant(register, value)
      orphan(register)
      register
    }

    /**
      * Lazily loads variable to register.
      */
    def loadVariable(variable: Operand): Register = {
      // Prefer register over memory.
      val preference = descriptors(variable).find(_.isInstanceOf[RegisterLocation])
        .orElse(descriptors(variable).find(_.isInstanceOf[MemoryLocation]))

      preference match {
        case Some(RegisterLocation(register)) => register
        case Some(MemoryLocation(address)) => loadFromMemory(variable, address)
        case None =>
          variable match {
            case n: Name => StdOut.error(n.entry.location, s"$variable could be not initialized")
            case _ => StdOut.error(null, s"$variable could be not initialized")
          }
          val register = select()
          seize(register, variable)
          register
      }
    }

    operand match {
      case Constant(value) => loadConstant(value)
      case oth => loadVariable(oth)
    }
  }

  /**
    * Loads variable directly from memory.
    *
    * @param variable a variable to load
    * @param address  an address of memory to be loaded
    * @return register with loaded variable
    */
  final def loadFromMemory(variable: Operand, address: BigInt): Register = {
    builder.comment(s"need to load $variable from $address")
    val register = select()
    emitConstant(Register.A, address)
    builder += AsmLoad(register)
    seize(register, variable)
    register
  }


  /**
    * Updates the ownership of a register so that a variable is
    * an exclusive owner of this register.
    *
    * Removes any occurrence of the `register` in descriptors
    * and assigns the location of the `variable` only to the `register`.
    *
    * @param register the register
    * @param variable the variable that will be owner of provided register
    * @throws IllegalArgumentException if `variable` is instance of [[Constant]]
    */
  final def seize(register: Register, variable: Operand): Unit = {
    orphan(register)
    descriptors(variable) = mutable.Set(RegisterLocation(register))
  }

  /**
    * Updates the ownership of a register so that a variable shares
    * the ownership of this register.
    *
    * Clears descriptors for the `variable` and sets
    * it to `register`. Any other variable are untouched as they were.
    *
    * @param register the register
    * @param variable the variable that will share the ownership
    *                 with other variables.
    * @throws IllegalArgumentException if `variable` is instance of [[Constant]]
    */
  final def share(register: Register, variable: Operand): Unit = {
    descriptors.addBinding(variable, RegisterLocation(register))
  }

  /**
    * Selects a register that can be used further.
    *
    * First, checks if there is a free register
    * according to the memory descriptors and returns it.
    *
    * Otherwise dumps one variable from register
    * to memory, picking the cheapest one.
    * This process is called ''spilling''.
    *
    * To free selected registers obtained
    * by subsequent calls, call [[clearSelection]].
    * Freeing selection will cause that
    * registers can be selected further again.
    *
    * @return a selected register
    * @throws UnsupportedOperationException if there is too many registers to select
    */
  final def select(): Register = {
    val available = (Set(Register.values: _*) - Register.A) diff selectedSet

    def selectFree: Option[Register] = {
      val free = for {
        register <- available if descriptors
          .values
          .forall(locations => !(locations contains RegisterLocation(register)))
      } yield register
      free.lastOption
    }

    def spill: Register = {
      /**
        * Computes the penalty for register to spill.
        *
        * @param register the register
        * @return penalty for register
        */
      def penalty(register: Register): BigInt = {
        def ones(bigInt: BigInt): Int = (0 until bigInt.bitLength).count(bigInt.testBit)

        val penalties = for {
          (variable, locSet) <- descriptors if locSet == Set(RegisterLocation(register))
          address = getAddress(variable)
        } yield (address min (5 * address.bitLength + ones(address))) + 50

        penalties.reduceOption(_ + _).getOrElse(0)
      }

      // Get register with the smallest penalty.
      val victim = available.minBy(penalty)

      // Get variables needed to spill.
      val variablesToSpill = for {
        (variable, locSet) <- descriptors if locSet == Set(RegisterLocation(victim))
      } yield variable

      // Store variables to memory.
      variablesToSpill.foreach { variable =>
        val address = getAddress(variable)
        builder.comment(s"spilling $variable that seized $victim to $address")
        emitConstant(Register.A, address)
        builder += AsmStore(victim)
        descriptors(variable) = mutable.Set(MemoryLocation(address))
      }
      orphan(victim)
      victim
    }

    val selected = selectFree.getOrElse(spill)
    selectedSet += selected
    selected
  }

  /**
    * Clears previous selection of registers,
    * so that it makes being able to select
    * them again.
    */
  final def clearSelection(): Unit = selectedSet.clear()

  /**
    * Stores all variables to the memory.
    */
  final def saveVariables(): Unit = {
    val available = Set(Register.values: _*) - Register.A
    for (r <- available) {
      val variablesToSpill = for {
        (variable, locSet) <- descriptors if (locSet == Set(RegisterLocation(r))) /*&& (liveness(currentBlock).out contains variable)*/
      } yield variable
      variablesToSpill.foreach { variable =>
        val address = getAddress(variable)
        builder.comment(s"spilling $variable that seized $r to $address")
        emitConstant(Register.A, address)
        builder += AsmStore(r)
        descriptors(variable) = mutable.Set(MemoryLocation(address))
      }
    }
  }

  /**
    * Resets all register causes that
    * no variable exists in registers.
    */
  final def resetRegistersState(): Unit = {
    val available = Set(Register.values: _*) - Register.A
    for (r <- available) orphan(r)
  }

  /**
    * Emits a constant to the specified register.
    *
    * @param register the target register
    * @param value    the value to be emitted to the given register
    */
  protected def emitConstant(register: Register, value: BigInt): Unit

  /**
    * Updates the ownership of a register
    * so that no variable owns this register.
    *
    * Clears descriptors that
    * points to this register.
    *
    * @param register the register
    */
  final def orphan(register: Register): Unit = {
    for (variable <- descriptors.keys)
      descriptors(variable) -= RegisterLocation(register)
  }

  /**
    * Allocates memory for given operand.
    *
    * @param variable the variable to allocate in memory
    * @return the address of allocated operand
    */
  final def getAddress(variable: Operand): BigInt = {
    def allocate: BigInt = {
      val oldOffset = addressOffset
      variable match {
        case Name(entry: ArrayEntry) => addressOffset += entry.size
        case _ => addressOffset += 1
      }
      oldOffset
    }

    addressTable.getOrElseUpdate(variable, allocate)
  }

  /**
    * Memory descriptors table.
    *
    * Contains information where operands are available.
    */
  private val descriptors = new mutable.HashMap[Operand, mutable.Set[DescriptorEntry]]
    with mutable.MultiMap[Operand, DescriptorEntry]

  /**
    * Address table.
    *
    * Keeps addresses of operands that were allocated.
    */
  private val addressTable = new mutable.HashMap[Operand, BigInt]

  /**
    * Keeps the offset of address.
    */
  private var addressOffset = 0: BigInt

  /**
    * Describes the current location of variable
    * in register or memory.
    */
  private sealed trait DescriptorEntry

  /**
    * Indicates that given variable is in the register.
    *
    * @param register a register where variable takes place
    */
  private case class RegisterLocation(register: Register) extends DescriptorEntry

  /**
    * Indicates a variable is available at given address in memory.
    *
    * @param address the address in memory where the variable is available
    */
  private case class MemoryLocation(address: BigInt) extends DescriptorEntry

  /**
    * Keeps selected register
    * that should not be selected again.
    */
  private val selectedSet = new mutable.HashSet[Register]

  private val endSpilled = new mutable.HashSet[Register]
}
