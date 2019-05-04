package model.nbt

final case class TAG_List(tagId: Byte, elems: List[TAG]) extends TAG {

  def tagAdded(element: TAG): TAG_List =
    TAG_List(tagId, elems :+ element)

  def apply(index: Int): TAG = elems(index)

}

object TAG_List {

  def apply(tagId: Byte = 0): TAG_List = TAG_List(tagId, List.empty)

}