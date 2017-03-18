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
    * change target from Argonaut to ZeroFormatter
 */

package zeroformatter.akka.http

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives
import akka.stream.{ ActorMaterializer, Materializer }
import zeroformatter._
import shapeless.cachedImplicit
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.io.StdIn

object ExampleApp {

  final object Foo {
    implicit val zeroFormatter: Formatter[Foo] = cachedImplicit
  }
  @ZeroFormattable
  final case class Foo(@Index(0) bar: String)

  def main(args: Array[String]): Unit = {
    implicit val system = ActorSystem()
    implicit val mat    = ActorMaterializer()

    Http().bindAndHandle(route, "127.0.0.1", 8000)

    StdIn.readLine("Hit ENTER to exit")
    Await.ready(system.terminate(), Duration.Inf)
  }

  def route(implicit mat: Materializer) = {
    import ZeroFormatterSupport._
    import Directives._

    pathSingleSlash {
      post {
        entity(as[Foo]) { foo =>
          complete {
            foo
          }
        }
      }
    }
  }
}