with Logger;

package body Crypto_Helper is
  
  function Compute_HMAC(Message : String; Key : String) return String is
    use Crypto.Types;
    use Crypto.Symmetric.Mac.Hmac_SHA256;
    
    
    function To_W_Block512(Message : Bytes) return W_Block512 is
      Block : W_Block512 := (others => 0);
      
      Message_In_Words : Words := To_Words(Message);
    begin
      for I in Message_In_Words'First .. Message_In_Words'Last loop
        Block(I) := Message_In_Words(I);
      end loop;
        
      return Block;
    end To_W_Block512;
    
    
    Block_Length      : constant Positive := Positive(W_Block512'Length);
    Full_Blocks       : Natural := Message'Length / Block_Length;
    Key_Block         : W_Block512 := To_W_Block512(To_Bytes(Key));
    Message_In_Bytes  : Bytes := To_Bytes(Message);
    Position          : Natural := Message_In_Bytes'First;
    Tag               : W_Block256;
    Return_String     : ASU.Unbounded_String;
  begin
--    Logger.Put_Line("Key:   " & Key, Logger.Info, "Crypto");
--    Logger.Put_Line("Input: " & Message, Logger.Info, "Crypto");
    
    Init(Key_Block);
    
    if Message'Length = Full_Blocks * Block_Length then
      Full_Blocks := Full_Blocks - 1;
    end if;
    
    for I in 1 .. Full_Blocks loop
      Sign(
        To_W_Block512(Message_In_Bytes(Position .. Position + Block_Length - 1))
      );
      
      Position := Position + Block_Length;
    end loop;
    
    Final_Sign(
      To_W_Block512(Message_In_Bytes(Position .. Message_In_Bytes'Last)), 
      8, Tag
    );
    
    for I in Tag'Range loop
      ASU.Append(
        Return_String,
        ASU.To_Unbounded_String(To_Hex(Tag(I)))
      );
    end loop;
    
    Logger.Put_Line(ASU.To_String(Return_String) & " -> " & Message, Logger.Info, "Crypto");
    
    return ASU.To_String(Return_String);
  end Compute_HMAC;
  
  
  function Encrypt(Message : String; Key : String) return String is
  begin
    return Message;
  end Encrypt;
  
  
  function Decrypt(Message : String; Key : String) return String is
  begin
    return Message;
  end Decrypt;
  
end Crypto_Helper;