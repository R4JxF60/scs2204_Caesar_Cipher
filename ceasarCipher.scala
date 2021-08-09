import scala.collection.immutable
import scala.io.StdIn.readLine

object ceasarCipher {

    // 65 to 90 capital alphabet

    // encription
    def encription(msg: String, shifts: Int): String = {
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

    // decription
    def decription(encriptedMsg: String, shifts: Int): String = {
        var upText = encriptedMsg.toUpperCase();
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
            println("                Encripted message : "+encription(message, shifts))
        }
        case 'd' => {
            print("                          Message : ")
            var message: String = readLine()
            print("                           Shifts : ")
            var shifts: Int = readInt()
            println("                Decripted message : "+decription(message, shifts))
        }
        case _ => {
            println("                            Input : Invalid")
        }
    }
    
    // drive function for the code
    def main(args: Array[String]): Unit = {
        println("                    Ceasar Cipher : Encriptor & Decoder")
        println("             For encription press : e")
        println("             For decription press : d")
        print("                            Input : ")
        var method: Char = readChar()
        cipher(method)
    }
}
