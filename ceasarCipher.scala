import scala.collection.immutable
import scala.io.StdIn.readLine

object ceasarCipher {

    // 65 to 90 capital alphabet

    // encryption
    def encryption(msg: String, shifts: Int): String = {
        var upText = msg.toUpperCase();
        return upText.map(c => {
            if('A'.toInt to 'Z'.toInt contains c.toInt) {
                (((c.toInt - 65)+shifts) % 26)match {
                    case x if(x<0) => (x+91).toChar 
                    case x if(x>=0) => (x+65).toChar  
                }
            }else c
        })
    }

    // decryption
    def decryption(encryptedMsg: String, shifts: Int): String = {
        var upText = encryptedMsg.toUpperCase();
        return upText.map(c => {
            if('A'.toInt to 'Z'.toInt contains c.toInt) {
                (((c.toInt - 65)-shifts) % 26)match {
                    case x if(x<0) => (x+91).toChar 
                    case x if(x>=0) => (x+65).toChar  
                }
            }else c
        })
    }

    // cipher 
    def cipher(method: Char): Unit = method match {
        case 'e' => {
            print("                          Message : ")
            var message: String = readLine()
            print("                           Shifts : ")
            var shifts: Int = readInt()
            println("                Encrypted message : "+encryption(message, shifts))
        }
        case 'd' => {
            print("                          Message : ")
            var message: String = readLine()
            print("                           Shifts : ")
            var shifts: Int = readInt()
            println("                Decrypted message : "+decryption(message, shifts))
        }
        case _ => {
            println("                            Input : Invalid")
        }
    }
    
    // drive function for the code
    def main(args: Array[String]): Unit = {
        println("                    Ceasar Cipher : Encryptor & Decoder")
        println("             For encryption press : e")
        println("             For decryption press : d")
        print("                            Input : ")
        var method: Char = readChar()
        cipher(method)
    }
}
