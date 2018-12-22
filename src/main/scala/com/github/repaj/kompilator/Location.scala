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

import org.antlr.v4.runtime.ParserRuleContext

/**
  * Describes a location in source code file.
  *
  * @param sourceFile the source code file name
  * @param line       the line number
  * @param column     the column number
  * @author Konrad Kleczkowski
  */
class Location(val sourceFile: String, val line: Int, val column: Int) {
  /**
    * Creates new location basing on current rule context.
    *
    * @param ctx the parser rule context
    * @return new location that is described
    *         by the rule context
    */
  def this(ctx: ParserRuleContext) = this(
    ctx.getStart.getTokenSource.getSourceName,
    ctx.getStart.getLine, ctx.getStart.getCharPositionInLine + 1)

  /**
    * Returns the string representation of this location.
    */
  override def toString: String = s"$sourceFile:$line:$column"
}
