with Crypto.Hashfunction_SHA1; use  Crypto.Hashfunction_SHA1;
with Ada.Text_IO; use Ada.Text_IO;

package body Crypto.Asymmetric.ECNR is
   use Big.Mod_Utils;
   use Big.Utils;



-------------------------------------------------------------------------------

   -- check if key is initialited
   function Is_Init(Key : ECNR_P_KEY) return Boolean is
   begin
      if Key.n /= Big_Unsigned_Zero then
         return True;
      else return False;
      end if;
   end Is_Init; Pragma Inline (Is_Init);

-------------------------------------------------------------------------------

   -- check if key is initialited
   function Is_Init(Key : ECNR_S_KEY) return Boolean is
   begin
      if Key.d /= Big_Unsigned_Zero then
			return True;
      else return False;
      end if;
   end Is_Init; Pragma Inline (Is_Init);

-------------------------------------------------------------------------------

   procedure Gen_Public_Key(
						Public_Key  : out Public_Key_ECNR;
						length      : in DB.Bit_Length) is
	begin
		Get_Elliptic_Curve(Public_Key.E, Public_Key.P, Public_Key.n, length);
		init(Public_Key.E);
	end Gen_Public_Key;

-------------------------------------------------------------------------------

	procedure Gen_Private_Key(
						Public_Key  : in Public_Key_ECNR;
                  Private_Key : out Private_Key_ECNR) is
	begin
		Private_Key.d := Get_Random(Public_Key.n - Big_Unsigned_Three) + Big_Unsigned_One;
		Private_Key.Q := Private_Key.d * Public_Key.P;
	end Gen_Private_Key;

-------------------------------------------------------------------------------


   procedure Sign(
						Public_Key  	: in out Public_Key_ECNR;
						Private_Key_1 	: in  Private_Key_ECNR;
                  SHA1_Hash   	: in  W_Block160;
                  Signature   	: out Signature_ECNR) is
		tmp_H : constant Big_Unsigned := To_Big_Unsigned(To_Bytes(Sha1_Hash));
		tmp   : Big_Unsigned;
		Private_Key_2 	: Private_Key_ECNR;
	begin
			Public_Key.Q :=  Private_Key_1.Q;
		loop
			Gen_Private_Key(Public_Key, Private_Key_2);
			Signature.C := add(Private_Key_2.Q.X, tmp_H, Public_Key.n);

			if Signature.C /= Big_Unsigned_Zero then
				exit;
			end if;
		end loop;
		Signature.D := sub(Private_Key_2.d, mult(Private_Key_1.d, Signature.C ,Public_Key.n) ,Public_Key.n);

	end Sign;

-------------------------------------------------------------------------------

   function Verify(Public_Key  : Public_Key_ECNR;
                   SHA1_Hash   : W_Block160;
                   Signature   : Signature_ECNR) return Boolean is
		tmp_H : constant Big_Unsigned := To_Big_Unsigned(To_Bytes(Sha1_Hash));
		tmp_EC : EC_Point 		:= (Signature.D * Public_Key.P) + (Signature.C * Public_Key.Q);
		tmp_BU : Big_Unsigned	:= sub(Signature.C, (tmp_EC.x) , Public_Key.n);
   begin
      if tmp_H = tmp_BU then return True;
      else return False;
      end if;
	end Verify;

-------------------------------------------------------------------------------

   procedure Sign_File(
							Filename    	: in String;
                 		Public_Key  	: in out Public_Key_ECNR;
							Private_Key_1 	: in Private_Key_ECNR;
                     Signature   	: out Signature_ECNR) is
   begin
      if Is_Init(ECNR_S_Key(Private_Key_1)) and Is_Init(ECNR_P_Key(Public_Key)) then
         Sign(Public_Key, Private_Key_1, F_Hash(Filename), Signature);
      else
         raise Invalid_Private_Key_Error;
      end if;
   end Sign_File;

-------------------------------------------------------------------------------

   function Verify_File(
								Filename   : String;
                        Public_Key : Public_Key_ECNR;
                        Signature  : Signature_ECNR) return Boolean is
   begin
      if Is_Init(ECNR_P_Key(Public_Key)) then
         return Verify(Public_Key, F_Hash(Filename), Signature);
      else
         raise Invalid_Public_Key_Error;
      end if;
   end Verify_File;


-------------------------------------------------------------------------------

   function Verify_Key_Pair(
				            Public_Key  : Public_Key_ECNR;
								Private_Key : Private_Key_ECNR) return Boolean is
   begin
      if Is_Init(ECNR_P_KEY(Public_Key)) = False or
        Is_Init(ECNR_S_KEY(Private_Key)) = False then
         return False;
      elsif
			--- do some more? ---
        	(Private_Key.d * Public_Key.P = Public_Key.Q) and (Public_Key.Q = Private_Key.Q) then
         return True;
      else return False;
      end if;
   end Verify_Key_Pair;

-------------------------------------------------------------------------------

	function equal_Public_Key_Curve(
 						Public_Key_A  : Public_Key_ECNR;
						Public_Key_B  : Public_Key_ECNR) return Boolean is
	begin
		if (Public_Key_A.E = Public_Key_B.E) and (Public_Key_A.P = Public_Key_B.P) and (Public_Key_A.n = Public_Key_B.n) then
			return true;
		else
			return false;
		end if;
	end equal_Public_Key_Curve;

-------------------------------------------------------------------------------

end Crypto.Asymmetric.ECNR;
