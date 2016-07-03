package autowire

import utest._
import utest.ExecutionContext.RunNow
import utest.PlatformShims._

object TypeParameterTests extends TestSuite {
  import upickle._

  trait InnerApi[A] {
    def show(x : A) : String
    def iff(c: Boolean, t : A, f : A) : A
  }

  class InnerApiImpl[A](str : String) extends InnerApi[A] {
    def show(x : A) = x.toString + str
    def iff(c : Boolean, t : A, f : A) = if(c) t else f
  }

  trait OuterApi {
    val string : InnerApi[String]
    val int : InnerApi[Int]
  }

  class OuterApiImpl extends OuterApi {
    val string = new InnerApiImpl[String]("")
    val int = new InnerApiImpl[Int]("")
  }


  trait Outer2Api[R,S] {
    val r : InnerApi[R]
    val s : InnerApi[S]
  }

  class Outer2ApiImpl[P,Q] extends Outer2Api[P,Q] {
    val r = new InnerApiImpl[P]("")
    val s = new InnerApiImpl[Q]("")
  }

  trait Outer3Api {
    type R = String
    type S = Int
    val r : InnerApi[R]
    val s : InnerApi[S]
  }
  class Outer3ApiImpl extends Outer3Api {
    val r = new InnerApiImpl[String]("")
    val s = new InnerApiImpl[Int]("")
  }


  trait UPickleSerializers extends Serializers[String, upickle.Reader, upickle.Writer] {
    override def write[Result: Writer](r: Result) = upickle.write(r)
    override def read[Result: Reader](p: String) = upickle.read[Result](p)
  }

  object UPickleServer extends autowire.Server[String, upickle.Reader, upickle.Writer] with UPickleSerializers
  case class MyClient(router : UPickleServer.Router) extends autowire.Client[String, upickle.Reader, upickle.Writer] with UPickleSerializers {
    override def doCall(req: Request) = {
      router(req)
    }
  }
  // client-side implementation, and call-site
  val tests = TestSuite {
    'inner {
      val x = new InnerApiImpl[String]("")
      val router = UPickleServer.route[InnerApi[String]](x)

      val a = await(MyClient(router)[InnerApi[String]].show("horse").call())
      val b = await(MyClient(router)[InnerApi[String]].iff(true, "a", "b").call())
      assert("horse" == a)
      assert("a" == b)
    }

    'outer {
      val x = new OuterApiImpl
      val router = UPickleServer.route[OuterApi](x)

      val a = await(MyClient(router)[OuterApi].string.show("horse").call())
      val b = await(MyClient(router)[OuterApi].int.iff(false, 21, 42).call())
      assert("horse" == a)
      assert(42 == b)
    }


    'outer2 {
      val x = new Outer2ApiImpl[String,Int]
      val router = UPickleServer.route[Outer2Api[String,Int]](x)

      val a = await(MyClient(router)[Outer2Api[String,Int]].r.show("horse").call())
      val b = await(MyClient(router)[Outer2Api[String,Int]].s.iff(false, 21, 42).call())
      assert("horse" == a)
      assert(42 == b)
    }
    'outer2alias {
      type A = String
      type B = Int
      val x = new Outer2ApiImpl[A,B]
      val router = UPickleServer.route[Outer2Api[A,B]](x)

      val a = await(MyClient(router)[Outer2Api[A,B]].r.show("horse").call())
      val b = await(MyClient(router)[Outer2Api[A,B]].s.iff(false, 21, 42).call())
      assert("horse" == a)
      assert(42 == b)
    }
    'outer3 {
      val x = new Outer3ApiImpl
      val router = UPickleServer.route[Outer3Api](x)

      val a = await(MyClient(router)[Outer3Api].r.show("horse").call())
      val b = await(MyClient(router)[Outer3Api].s.iff(false, 21, 42).call())
      assert("horse" == a)
      assert(42 == b)
    }


  }
}
