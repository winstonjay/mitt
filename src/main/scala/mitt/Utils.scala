package mitt

import java.security.MessageDigest

object Utils {

    val msg = MessageDigest.getInstance("SHA-1")

    def sha1(str: String) : String = {
        msg.reset()
        val m = msg.digest(str.getBytes("UTF-8"))
        m.map("%02x".format(_)).mkString
    }
}