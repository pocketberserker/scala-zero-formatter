/*
 * Copyright 2015 Heiko Seeberger
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/*
  Change log
    * rename package
    * change test target from Argonaut to ZeroFormatter
 */

package zeroformatter.akka.http

import akka.actor.ActorSystem
import akka.http.scaladsl.marshalling.Marshal
import akka.http.scaladsl.model.ContentTypes.`application/octet-stream`
import akka.http.scaladsl.model.{ HttpEntity, RequestEntity }
import akka.http.scaladsl.unmarshalling.Unmarshaller.UnsupportedContentTypeException
import akka.http.scaladsl.unmarshalling.{ Unmarshal, Unmarshaller }
import akka.stream.ActorMaterializer
import org.scalatest.{ AsyncWordSpec, BeforeAndAfterAll, Matchers }
import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import zeroformatter._
import shapeless.cachedImplicit

object ZeroFormatterSupportSpec {
  @ZeroFormattable
  final case class Foo(@Index(0) bar: String) {
    require(bar == "bar", "bar must be 'bar'!")
  }

  object Foo {
    implicit val zeroFormatter: Formatter[Foo] = cachedImplicit
  }
}

class ZeroFormatterSupportSpec extends AsyncWordSpec with Matchers with BeforeAndAfterAll {
  import ZeroFormatterSupport._
  import ZeroFormatterSupportSpec._

  private implicit val system = ActorSystem()
  private implicit val mat    = ActorMaterializer()

  "ArgonautSupport" should {
    import system.dispatcher

    "enable marshalling and unmarshalling objects for generic derivation" in {

      val foo = Foo("bar")
      Marshal(foo)
        .to[RequestEntity]
        .flatMap(Unmarshal(_).to[Foo])
        .map(_ shouldBe foo)
    }

    "fail with NoContentException when unmarshalling empty entities" in {

      val entity = HttpEntity.empty(`application/octet-stream`)
      Unmarshal(entity)
        .to[Foo]
        .failed
        .map(_ shouldBe Unmarshaller.NoContentException)
    }

    "fail with UnsupportedContentTypeException when Content-Type is not `application/octet-stream`" in {

      val entity = HttpEntity("""{ "bar": "bar" }""")
      Unmarshal(entity)
        .to[Foo]
        .failed
        .map(_ shouldBe UnsupportedContentTypeException(`application/octet-stream`))
    }
  }

  override protected def afterAll() = {
    Await.ready(system.terminate(), 42.seconds)
    super.afterAll()
  }
}