package fbsim.common

import java.security._
import java.security.spec.X509EncodedKeySpec
import java.util.Base64
import javax.crypto.{KeyGenerator, Cipher, SecretKey}
import javax.crypto.spec.{IvParameterSpec, SecretKeySpec}

object SecurityApis {

  def buildAESKey(encodedKey:String):SecretKey= new SecretKeySpec(base64Decode(encodedKey),"AES")
  def buildRSAPublicKey(encodedKey:String):PublicKey = {
    val keySpec = new X509EncodedKeySpec(base64Decode(encodedKey))
    val keyFactory = KeyFactory.getInstance("RSA")
    val pubKey = keyFactory.generatePublic(keySpec)
    pubKey
  }

  def base64Encode(bytes:Array[Byte]) = {
    Base64.getEncoder.encodeToString(bytes)
  }
  def base64Decode(encodedStr: String): Array[Byte] = {
    Base64.getDecoder.decode(encodedStr)
  }

  def encryptSharedKey(secretKey:SecretKey, publicKey: PublicKey):String = {
    val cipher = Cipher.getInstance("RSA/ECB/PKCS1Padding")
    cipher.init(Cipher.ENCRYPT_MODE, publicKey)
    base64Encode(cipher.doFinal(secretKey.getEncoded))
  }

  def decryptSharedKey(encryptedKey:Array[Byte], privateKey:PrivateKey):SecretKey = {
    val cipher = Cipher.getInstance("RSA/ECB/PKCS1Padding")
    cipher.init(Cipher.DECRYPT_MODE, privateKey)
    val decBytes = cipher.doFinal(encryptedKey)
    new SecretKeySpec(decBytes,"AES")
  }

  def generateAESKey():SecretKey ={
    val generator = KeyGenerator.getInstance("AES")
    generator.init(128)
    generator.generateKey
  }

  def generateRSAKeyPair():KeyPair ={
    val generator = KeyPairGenerator.getInstance("RSA")
    generator.initialize(1024)
    generator.genKeyPair()
  }

  def encrypt(data:String, secretKey:SecretKey, initVector:Array[Byte]):String = {
    val cipher = Cipher.getInstance("AES/CBC/PKCS5Padding")
    cipher.init(Cipher.ENCRYPT_MODE, secretKey, new IvParameterSpec(initVector))
    base64Encode(cipher.doFinal(data.getBytes("UTF-8")))
  }
  
  def decrypt(data:String, secretKey:SecretKey, initVector:Array[Byte]):String = {
    val cipher = Cipher.getInstance("AES/CBC/PKCS5Padding")
    cipher.init(Cipher.DECRYPT_MODE, secretKey, new IvParameterSpec(initVector))
    new String(cipher.doFinal(base64Decode(data)))
  }

  def genIV():Array[Byte] ={
    val sRNG = SecureRandom.getInstance("SHA1PRNG")
    sRNG.setSeed(sRNG.generateSeed(32))
    val initV = new Array[Byte](16)
    sRNG.nextBytes(initV)
    initV
  }

  def sign(data:String, privateKey: PrivateKey):String = {
    val signinguser =  Signature.getInstance("SHA256withRSA")
    signinguser.initSign(privateKey)
    signinguser.update(data.getBytes("UTF-8"))
    base64Encode(signinguser.sign())
  }

  def verify(data:String, signedValue:String, pubKey: PublicKey):Boolean = {
    val verifier =  Signature.getInstance("SHA256withRSA")
    verifier.initVerify(pubKey)
    verifier.update(data.getBytes("UTF-8"))
    verifier.verify(base64Decode(signedValue))
  }
}
