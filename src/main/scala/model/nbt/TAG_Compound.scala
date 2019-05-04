package model.nbt

final case class TAG_Compound(elems: Map[String, TAG]) extends TAG {

  def tagAdded(name: String, value: TAG): TAG_Compound =
    TAG_Compound(elems.updated(name, value))

  def tagRemoved(name: String): TAG_Compound =
    TAG_Compound(elems - name)

  def apply(name: String): TAG = elems(name)

  def get(name: String): Option[TAG] = elems.get(name)
}