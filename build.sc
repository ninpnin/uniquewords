import mill._, scalalib._, scalanativelib._
import mill.scalanativelib.api.ReleaseMode

object uniquewords extends ScalaNativeModule {
  def scalaVersion = "2.13.4"
  def scalaNativeVersion = "0.4.0"
  //def logLevel = NativeLogLevel.Info
  def releaseMode = ReleaseMode.ReleaseFull // Debug, ReleaseFull
}
