import mill._, scalalib._, scalanativelib._
import mill.scalanativelib.api.ReleaseMode

object uniquewords extends ScalaNativeModule {
  def scalaVersion = "2.13.12"
  def scalaNativeVersion = "0.4.17"
  //def logLevel = NativeLogLevel.Info
  def releaseMode = ReleaseMode.ReleaseFull // Debug, ReleaseFull
}
