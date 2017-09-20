package xyz.hyperreal.horspool

import scala.collection.immutable.TreeSet
import scala.collection.mutable.ArrayBuffer
import util.Random._


object Main extends App {
//	for (_ <- 1 to 10) {
//		val p = rnd( 4, 10 )
//		val s = rnd( 1000000, 5000000 )
//		val alg = new Pattern( p ).search( s )
//		val lib = s.indexOf( p )
//
//		println( p.length, s.length, alg, lib, if (alg != lib) "failed" else "ok" )
//	}

	val p = new Pattern( rnd(10, 50) )
	val s = rnd( 5000000, 10000000 ) + p.s

	println( s"pattern is ${p.s.length} in length" )
	println( s"subject is ${s.length} in length" )

	time( "boyer-moore-horspool" ) {
		println( p.search(s) )
	}

	time( "naive" ) {
		println( p.naive(s) )
	}

	def time( title: String )( thunk: => Unit ) {
		System.gc
		System.gc
		System.gc

		var tstart: Long = System.currentTimeMillis

		thunk

		val time = "%.3f" format ((System.currentTimeMillis - tstart)*.001)

		println( s"time for '$title' is $time seconds" )
	}

	def rnd( min: Int, max: Int ) = (0 until (nextInt(max - min + 1) + min)) map (_ => nextPrintableChar) mkString
}

class Pattern( val s: String ) {
	private val (min, max, bca) = {
		val p = s.init
		val r = p.reverse
		val cs = TreeSet( p: _* )
		val min = cs.min
		val max = cs.max
		val a = new ArrayBuffer[Int]

		for (c <- min to max)
			if (cs(c))
				a += r.indexOf( c ) + 1
			else
				a += s.length

		(min, max, a.toArray)
	}
	private val r = s.reverse

	def naive( subject: String ): Int = {
		var pos = 0

		while (pos <= subject.length - s.length) {
			def matches: Boolean = {
				for (i <- 0 until s.length)
					if (subject(pos + i) != s(i))
						return false

				true
			}

			if (matches)
				return pos
			else
				pos += 1
		}

		-1
	}

	def search( subject: String ): Int = {
		var pos = s.length - 1

		while (pos < subject.length) {
			def matches: Boolean = {
				for (i <- 0 until s.length)
					if (subject(pos - i) != r(i)) {
						val c = subject(pos)

						pos += (if (c < min || c > max) s.length else bca(c - min))
						return false
					}

				true
			}

			if (matches)
				return pos - s.length + 1
		}

		-1
	}
}