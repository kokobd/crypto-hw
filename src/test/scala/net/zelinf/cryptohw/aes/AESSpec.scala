package net.zelinf.cryptohw.aes

import java.io.ByteArrayOutputStream

import javax.crypto.spec.{IvParameterSpec, SecretKeySpec}
import javax.crypto.{Cipher, CipherOutputStream}
import net.zelinf.cryptohw.UnitSpec

class AESSpec extends UnitSpec {

  describe("encryptCBC") {
    it("Encrypts as javax.crypto.Cipher does") {
      val keyData: Array[Byte] = padStringWithZeros("sysu", 16)
      val key: Key = Key(keyData)
      val iv: Array[Byte] = Array.fill(16)(0.toByte).updated(0, 123.toByte)
      val plainText =
        "School of data science and computer, Sun Yat-sen University."

      val expected = javaEncryptCBC(iv, keyData, plainText.getBytes)
      val actual = encryptCBC(iv, key, plainText.getBytes)
      assert(actual === expected)
    }

  }

  private def padStringWithZeros(str: String, length: Int): Array[Byte] = {
    str.getBytes ++ Array.fill(length - str.length)(0.toByte)
  }

  private def javaEncryptCBC(iv: Array[Byte],
                             key: Array[Byte],
                             plainText: Array[Byte]): Array[Byte] = {
    val stdCipher = Cipher.getInstance("AES/CBC/PKCS5Padding")
    val aesKey = new SecretKeySpec(key, "AES")
    stdCipher.init(Cipher.ENCRYPT_MODE, aesKey, new IvParameterSpec(iv))

    val outputStream = new ByteArrayOutputStream()
    val cipherOutputStream = new CipherOutputStream(outputStream, stdCipher)
    cipherOutputStream.write(plainText)
    cipherOutputStream.close()

    outputStream.toByteArray
  }
}
