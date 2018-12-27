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

import scala.io.AnsiColor

/**
  * Singleton object that allows to report errors
  * during the compilation.
  *
  * To validate semantic check, run [[StdOut.validate]] method.
  *
  * @author Konrad Kleczkowski
  */
object StdOut {
  private var failed = false

  /**
    * Prints a fatal error to the standard output
    * and exits the program.
    *
    * @param message the message text
    */
  def fatal(message: String): Unit = {
    println(s"${AnsiColor.RED}fatal error: ${AnsiColor.WHITE}$message")
    sys.exit(1)
  }

  /**
    * Prints an error to the standard output with given location.
    *
    * If this method is called, then [[validate]] will cause
    * program exit.
    *
    * @param location the location in the source code
    * @param message  the message text
    */
  def error(location: Location, message: String): Unit = {
    println(s"${AnsiColor.RESET}$location: ${AnsiColor.RED}error: ${AnsiColor.WHITE}$message")
    failed = true
  }

  /**
    * Prints a warning to the standard output with given location.
    *
    * @param location the location in the source code
    * @param message  the message text
    */
  def warning(location: Location, message: String): Unit = {
    println(s"${AnsiColor.RESET}$location: ${AnsiColor.YELLOW}warning: ${AnsiColor.WHITE}$message")
  }

  /**
    * Validates the semantic check.
    *
    * If any error occurs, this method exits the program.
    */
  def validate(): Unit = {
    if (failed) sys.exit(1)
  }
}
