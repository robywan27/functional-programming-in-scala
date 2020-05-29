import chapters.chapter8_property_testing.Prop

trait Prop {
  def check: Boolean
  def &&(p: Prop): Prop =
    new Prop {
      def check = check && p.check
    }
}
