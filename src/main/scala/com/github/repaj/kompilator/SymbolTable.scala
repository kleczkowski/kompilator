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

package com.github.repaj.kompilator

import scala.collection.mutable

/**
  * An implementation of a symbol table.
  *
  * A symbol table allows to retrieve information about
  * the symbol during compilation phases.
  *
  * @author Konrad Kleczkowski
  */
final class SymbolTable private(upperContext: Option[SymbolTable],
                                map: mutable.Map[String, SymbolTable.Entry]) {
  /**
    * Creates new symbol table.
    */
  def this() = this(None, mutable.Map.empty)

  /**
    * Creates new symbol table for inner context.
    */
  def enterContext: SymbolTable = new SymbolTable(Some(this), mutable.Map.empty)

  /**
    * Returns the outer scope of this table if available.
    *
    * @throws IllegalStateException if this table has no outer scope
    */
  def exitContext: SymbolTable = upperContext.getOrElse(throw new IllegalStateException)

  /**
    * Lookups this symbol table for information
    * if available.
    *
    * If symbol is not available in the table and
    * upper contexts, then [[SymbolTable.UndefinedEntry]]
    * is returned as null object.
    *
    * @param name the name of symbol
    * @return an information about symbol with given name
    */
  def lookup(name: String): SymbolTable.Entry =
    if (map contains name) map(name) else upperContext.map(_.lookup(name)).getOrElse(SymbolTable.UndefinedEntry)

  /**
    * Updates this table with new symbol entry.
    *
    * @param name  the name of symbol
    * @param entry the entry as updated one
    */
  def update(name: String, entry: SymbolTable.Entry): Unit = map += (name -> entry)

  /**
    * Removes the symbol from this table.
    *
    * @param name the name of symbol
    */
  def remove(name: String): Unit = map -= name

  /**
    * Returns the immutable copy of current entry map.
    *
    * This map reflects all symbols available in current context.
    */
  def toMap: Map[String, SymbolTable.Entry] =
    upperContext.map(_.toMap).getOrElse(Map.empty) ++ map

  /**
    * Inserts new array to the table, checking the semantics.
    *
    * @param location   the location of declaration
    * @param name       the name of an array
    * @param startIndex the start index of new array
    * @param stopIndex  the stop index of new array
    */
  def newArray(location: Location,
               name: String,
               startIndex: BigInt, stopIndex: BigInt): Unit = {
    val entry = lookup(name)
    if (entry.isDefined)
      StdOut.error(location, s"symbol $name is already defined as $entry")
    else if (stopIndex < startIndex)
      StdOut.error(location, s"array $name has invalid bounds")
    update(name, SymbolTable.ArrayEntry(name, location, startIndex, stopIndex))
  }

  /**
    * Inserts new variable to the table,
    * checking the semantics.
    *
    * @param location the location of declaration
    * @param name     the name of an array
    */
  def newVariable(location: Location, name: String, iterator: Boolean): Unit = {
    val entry = lookup(name)
    if (entry.isDefined)
      StdOut.error(location, s"symbol $name is already defined as $entry")
    update(name, SymbolTable.VariableEntry(name, location, iterator))
  }

  def getVariable(location: Location, name: String): SymbolTable.Entry = {
    val raw = lookup(name)
    if (!raw.isDefined) {
      StdOut.error(location, s"variable $name is not defined")
    } else if (!raw.isVariable) {
      StdOut.error(location, s"$raw is not a variable")
    }
    raw
  }

  def getArray(location: Location, name: String): SymbolTable.Entry = {
    val raw = lookup(name)
    if (!raw.isDefined) {
      StdOut.error(location, s"array $name is not defined")
    } else if (!raw.isArray) {
      StdOut.error(location, s"$raw is not an array")
    }
    raw
  }
}

object SymbolTable {

  /**
    * Base class for symbol table entries.
    *
    * Each entry gives an information about a symbol.
    * These information is used among compilation phases,
    * especially during the semantic check.
    *
    * @author Konrad Kleczkowski
    */
  sealed abstract class Entry {
    /**
      * Returns the string representation of this entry.
      */
    override final def toString: String = this match {
      case VariableEntry(name, _, false, _) => s"variable $name"
      case VariableEntry(name, _, true, _) => s"variable (a.k.a. for-loop iterator) $name"
      case ArrayEntry(name, _, startIndex, stopIndex) => s"array $name($startIndex:$stopIndex)"
      case UndefinedEntry => "<undefined>"
    }

    /**
      * Returns the unique identifier of this symbol.
      */
    def name: String

    /**
      * Returns the location of symbol declaration.
      */
    def location: Location

    /**
      * Checks whether this entry is defined.
      */
    final def isDefined: Boolean = this != UndefinedEntry

    /**
      * Checks whether this entry is about an array.
      */
    final def isArray: Boolean = this.isInstanceOf[ArrayEntry]

    /**
      * Checks whether this entry is about a variable.
      */
    final def isVariable: Boolean = this.isInstanceOf[VariableEntry]

    /**
      * Returns this entry as an array entry.
      *
      * @throws IllegalStateException if this entry is not an array entry
      */
    final def asArray: ArrayEntry = this match {
      case ae: ArrayEntry => ae
      case _ => throw new IllegalStateException
    }

    /**
      * Returns this entry as an array entry.
      *
      * @throws IllegalStateException if this entry is not a variable entry
      */
    final def asVariable: VariableEntry = this match {
      case ve: VariableEntry => ve
      case _ => throw new IllegalStateException
    }
  }

  /**
    * Describes a variable.
    *
    * @param name        the name of variable
    * @param location    the location of variable declaration
    * @param temp        `true` if variable is temporary variable
    * @param iterator    `true` if this variable is `for`-loop iterator
    * @param initialized `true` if variable is initialized (default `false`)
    **/
  case class VariableEntry(name: String,
                           location: Location,
                           var iterator: Boolean = false,
                           var initialized: Boolean = false) extends Entry


  /**
    * Describes an array.
    *
    * @param name       the name of array
    * @param location   the location of array declaration
    * @param startIndex the start index
    * @param stopIndex  the stop index
    */
  case class ArrayEntry(name: String,
                        location: Location,
                        startIndex: BigInt,
                        stopIndex: BigInt) extends Entry {
    /**
      * Returns the size of this array.
      */
    def size: BigInt = stopIndex - startIndex + 1
  }

  /**
    * Null object of [[Entry]]. Returned when a symbol table does not
    * provide information about some symbol.
    */
  case object UndefinedEntry extends Entry {
    /**
      * Returns the unique identifier of this symbol.
      */
    def name: String = "???"

    /**
      * Returns the location of symbol declaration.
      */
    def location: Location = new Location("???", -1, -1)
  }

}