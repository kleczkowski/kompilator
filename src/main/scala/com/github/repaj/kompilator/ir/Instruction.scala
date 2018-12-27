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


/**
  * Base class of IR instructions.
  *
  * Instruction is a quadruple that realizes
  * high-level operations such as constant loading,
  * branches or arithmetic operations.
  *
  * @author Konrad Kleczkowski
  */
sealed abstract class Instruction extends Product {
  /**
    * Returns the string representation of this quadruple.
    */
  override final def toString: String = this match {
    case Move(source, destination) => s"$destination := $source"
    case Get(destination) => s"get $destination"
    case Put(source) => s"put $source"
    case IndexedLoad(base, offset, destination) => s"$destination := ${base.name}[$offset]"
    case IndexedStore(source, base, offset) => s"${base.name}[$offset] := $source"
    case Add(left, right, result) => s"$result := $left + $right"
    case Sub(left, right, result) => s"$result := $left - $right"
    case Mul(left, right, result) => s"$result := $left * $right"
    case Div(left, right, result) => s"$result := $left div $right"
    case Rem(left, right, result) => s"$result := $left mod $right"
    case Jump(block) => s"jump ${block.name}"
    case JumpIf(condition, ifTrue, ifFalse) => s"jump ${ifTrue.name} if $condition otherwise ${ifFalse.name}"
    case Halt => "halt"
  }

  /**
    * Tests if this instruction is binary instruction.
    */
  final def isBinary: Boolean = this.isInstanceOf[BinaryInstruction]

  /**
    * Test if this instruction is branch instruction.
    */
  final def isBranch: Boolean = this.isInstanceOf[BranchInstruction]

  /**
    * Test if this instruction is load/store instruction.
    */
  final def isLoadStore: Boolean = this.isInstanceOf[LoadStoreInstruction]

//  final def operands: Seq[Operand] = this
//    .productIterator
//    .filter(_.isInstanceOf[Operand])
//    .map(_.asInstanceOf[Operand])
//    .toSeq
//
//  final def defines: Option[Operand] = this match {
//    case Move(_, destination) => Some(destination)
//    case Get(destination) => Some(destination)
//    case Put(_) => None
//    case IndexedLoad(_, _, destination) => Some(destination)
//    case IndexedStore(_, _, _) => None
//    case Add(_, _, result) => Some(result)
//    case Sub(_, _, result) => Some(result)
//    case Mul(_, _, result) => Some(result)
//    case Div(_, _, result) => Some(result)
//    case Rem(_, _, result) => Some(result)
//    case Jump(_) => None
//    case JumpIf(_, _, _) => None
//    case Halt => None
//  }
//
//  final def uses: Seq[Operand] = operands diff defines.toSeq
}

/**
  * Base class for instructions
  * that issue memory movement
  * in the program.
  *
  * @author Konrad Kleczkowski
  */
sealed abstract class LoadStoreInstruction extends Instruction

/**
  * Base class for instructions
  * that preform some binary operations
  * such as arithmetic operations.
  *
  * @author Konrad Kleczkowski
  */
sealed abstract class BinaryInstruction extends Instruction {
  /**
    * Returns the left operand.
    */
  def left: Operand

  /**
    * Returns the right operand.
    */
  def right: Operand

  /**
    * Returns the destination of result.
    */
  def result: Operand
}

/**
  * Base class for instructions
  * that issue a control flow.
  *
  * @author Konrad Kleczkowski
  */
sealed abstract class BranchInstruction extends Instruction

/**
  * Reads an integer from standard input and places it in `destination`.
  */
case class Get(destination: Operand) extends LoadStoreInstruction

/**
  * Writes an integer to standard output from `source`.
  */
case class Put(source: Operand) extends LoadStoreInstruction

/**
  * Assigns `source` to `destination`.
  */
case class Move(source: Operand, destination: Operand) extends LoadStoreInstruction

/**
  * Assigns `base[offset]` to `destination`.
  */
case class IndexedLoad(base: SymbolTable.Entry, offset: Operand, destination: Operand) extends LoadStoreInstruction

/**
  * Assigns `source` to `base[offset]`.
  */
case class IndexedStore(source: Operand, base: SymbolTable.Entry, offset: Operand) extends LoadStoreInstruction

/**
  * Adds `left` to `right` and stores it in `result`.
  */
case class Add(left: Operand, right: Operand, result: Operand) extends BinaryInstruction

/**
  * Subtracts `right` from `left` and stores it in `result`.
  */
case class Sub(left: Operand, right: Operand, result: Operand) extends BinaryInstruction

/**
  * Multiplies `left` by `right` and stores it in `result`.
  */
case class Mul(left: Operand, right: Operand, result: Operand) extends BinaryInstruction

/**
  * Divides `left` by `right` and stores the quotient in `result`.
  */
case class Div(left: Operand, right: Operand, result: Operand) extends BinaryInstruction

/**
  * Divides `left` by `right` and stores the reminder in `result`.
  */
case class Rem(left: Operand, right: Operand, result: Operand) extends BinaryInstruction

/**
  * Jumps to the `block` immediately.
  */
case class Jump(block: BasicBlock) extends BranchInstruction

/**
  * Jumps to the `ifTrue` is `condition` is met, otherwise to `ifFalse`.
  */
case class JumpIf(condition: RelationOperator, ifTrue: BasicBlock, ifFalse: BasicBlock) extends BranchInstruction

/**
  * Stops the execution of code.
  */
case object Halt extends BranchInstruction

/**
  * Base class for jump conditions.
  *
  * @see [[JumpIf]]
  * @author Konrad Kleczkowski
  */
sealed abstract class RelationOperator {
  /**
    * Returns the LHS of condition.
    */
  def left: Operand

  /**
    * Returns the RHS of condition.
    */
  def right: Operand

  /**
    * Returns the string representation of this relational operator.
    */
  override final def toString: String = this match {
    case Eq(left, right) => s"$left = $right"
    case Ne(left, right) => s"$left <> $right"
    case Lt(left, right) => s"$left < $right"
    case Gt(left, right) => s"$left > $right"
    case Le(left, right) => s"$left <= $right"
    case Ge(left, right) => s"$left >= $right"
  }
}

/**
  * `left` is equal to `right`.
  */
case class Eq(left: Operand, right: Operand) extends RelationOperator

/**
  * `left` is different than `right`.
  */
case class Ne(left: Operand, right: Operand) extends RelationOperator

/**
  * `left` is less than `right`.
  */
case class Lt(left: Operand, right: Operand) extends RelationOperator

/**
  * `left` is greater than `right`.
  */
case class Gt(left: Operand, right: Operand) extends RelationOperator

/**
  * `left` is less or equal to `right`.
  */
case class Le(left: Operand, right: Operand) extends RelationOperator

/**
  * `left` is greater or equal to `right`.
  */
case class Ge(left: Operand, right: Operand) extends RelationOperator