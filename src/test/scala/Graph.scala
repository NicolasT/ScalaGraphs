/*
 *   ScalaGraphs, a functional graph library for Scala
 *
 *   Based on the work by Martin Erwig, see
 *       http://web.engr.oregonstate.edu/~erwig/fgl/
 *
 *   Copyright (C) 2009 Nicolas Trangez  <eikke eikke com>
 *
 *   This library is free software; you can redistribute it and/or
 *   modify it under the terms of the GNU Lesser General Public
 *   License as published by the Free Software Foundation, version 2.1
 *   of the License.
 *
 *   This library is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *   Lesser General Public License for more details.
 *
 *   You should have received a copy of the GNU Lesser General Public
 *   License along with this library; if not, write to the Free Software
 *   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 *   MA  02110-1301  USA
 */

package com.eikke.scalagraphs.test

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import com.eikke.scalagraphs.Funs.{isEmpty,gmap,grev,nodes}
import com.eikke.scalagraphs.Types.Context

class EmptyGraphSpec extends Spec with ShouldMatchers {
    describe("The Empty graph") {
        it("should be empty") {
            assert(isEmpty(Empty))
        }

        it("should have no context") {
            intercept[NoSuchFieldException] {
                Empty.context
                () // Need this for ScalaTest
            }
        }

        it("should have no graph") {
            intercept[NoSuchFieldException] {
                Empty.graph
                ()
            }
        }

        it("should return itself on gmap") {
            val new_ = gmap((ctx: Context[Any, Any]) => ctx)(Empty)
            new_ should be theSameInstanceAs (Empty)
        }

        it("should return itself on grev") {
            grev(Empty) should be theSameInstanceAs (Empty)
        }

        it("should have no nodes") {
            nodes(Empty) should be ('empty)
        }
    }
}


// vim: set ts=4 sw=4 et:
