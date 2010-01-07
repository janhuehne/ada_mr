with Crypto.Types;
with Crypto.Symmetric.Mac.Hmac_SHA256;
with Ada.Strings.Unbounded;

package Crypto_Helper is
  
  package ASU renames Ada.Strings.Unbounded;
  
  function Compute_HMAC(Message : String; Key : String) return String;
  
  function Encrypt(Message : String; Key : String) return String;
  function Decrypt(Message : String; Key : String) return String;
  
  Wrong_HMAC : exception;
  
end Crypto_Helper;