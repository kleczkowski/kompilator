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

import java.io.{File, PrintWriter}

import com.github.repaj.kompilator.codegen.CodeGenerator
import com.github.repaj.kompilator.ir.IRBuilder
import com.github.repaj.kompilator.parser.ErrorListenerImpl
import com.github.repaj.kompilator.parser.antlr4.{ImperatorLexer, ImperatorParser}
import com.github.repaj.kompilator.parser.ast.{AstToTacVisitor, Block}
import com.github.repaj.kompilator.vm.AsmBuilder
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}

/**
  * Main class of compiler.
  *
  * @author Konrad Kleczkowski
  */
object Main extends App {
  val sourceFile = args(0)
  val outputFile = args(1)
  val debug = args.length > 2 && args(2) == "--debug"

  val errorListener = new ErrorListenerImpl(sourceFile)
  val lexer = new ImperatorLexer(CharStreams.fromFileName(sourceFile))
  lexer.removeErrorListeners()
  lexer.addErrorListener(errorListener)
  val tokenStream = new CommonTokenStream(lexer)
  val parser = new ImperatorParser(tokenStream)
  parser.removeErrorListeners()
  parser.addErrorListener(errorListener)
  val block = parser.compilationUnit().node
  StdOut.validate()
  val (_, blocks) = new AstToTacVisitor(new IRBuilder, parser.getSymbolTable).generate(block)
  if (debug) blocks.foreach(print(_))
  val builder = new AsmBuilder
  val generator = new CodeGenerator(builder)
  generator.emit(blocks: _*)
  StdOut.validate()
  val writer = new PrintWriter(new File(outputFile))
  builder.render(writer)
  writer.flush()
  writer.close()
}