package net.zelinf.cryptohw
import net.zelinf.cryptohw.math.{Field, GF2Elem}

package object aes {

  private[aes] implicit val fieldIns: Field[GF2Elem] =
    GF2Elem.newGF2ElemFieldInstance(BigInt("100011011", 2))
}
