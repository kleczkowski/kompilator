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

import enumeratum._

/**
  * Base class for VM registers.
  *
  * @author Konrad Kleczkowski
  */
sealed abstract class Register extends EnumEntry {
  /**
    * Returns the letter of register.
    */
  final override def toString: String = this match {
    case Register.A => "A"
    case Register.B => "B"
    case Register.C => "C"
    case Register.D => "D"
    case Register.E => "E"
    case Register.F => "F"
    case Register.G => "G"
    case Register.H => "H"
  }
}

object Register extends Enum[Register] {
  val values = findValues

  case object A extends Register

  case object B extends Register

  case object C extends Register

  case object D extends Register

  case object E extends Register

  case object F extends Register

  case object G extends Register

  case object H extends Register

}
