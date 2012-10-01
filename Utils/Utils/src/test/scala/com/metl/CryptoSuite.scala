package com.metl

import net.liftweb.util._
import net.liftweb.common._

import org.scalatest.FunSuite
import org.scalatest.mock.MockitoSugar
import org.scalatest.OptionValues._

import com.metl.utils._ 

class CryptoSuite extends FunSuite {

    test("construct rsa normal crypto") { 
        val crypto = new RSANormal
    }

    ignore("construct rsa commutative crypto") { 
        info("unable to construct, 'java.security.NoSuchAlgorithmException: Cannot find any provider supporting RSA/None/NoPadding'")
        val crypto = new RSACommutative
    }

    test("construct rsa commutative 1 crypto") {
        val crypto = new RSACommutative1
    }

    test("construct aesctr crypto") {
        val crypto = new AESCTRCrypto
    }

    test("construct descbc crypto") {
 
        val crypto = new DESCBCCrypto
    }

    ignore("construct desecb crypto") {

        info("unable to construct, 'java.security.InvalidAlgorithmParameterException: ECB mode cannot use IV'")
        val crypto = new DESECBCrypto
    }

    test("construct aescbc crypto") {
        val crypto = new AESCBCCrypto
    }
}
