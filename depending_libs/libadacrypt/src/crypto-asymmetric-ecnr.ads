with Crypto.Types; use Crypto.Types;
with Crypto.Types.Big_Numbers;
with Crypto.Types.Elliptic_Curves.Zp;
with Crypto.Types.Elliptic_Curves.Zp.Database;

generic
   Size : Positive;

package Crypto.Asymmetric.ECNR is

   package Big is new Crypto.Types.Big_Numbers(Size);
   use Big;
   package EC  is new Crypto.Types.Elliptic_Curves(Big);
   use EC;
   package Zp is new EC.Zp;
   use Zp;
	package DB is new Zp.Database;
	use DB;


   type Public_Key_ECNR is private;
   type Private_Key_ECNR is private;
   type Signature_ECNR is private;

-------------------------------------------------------------------------------

   procedure Gen_Public_Key(
						Public_Key  	: out Public_Key_ECNR;
						length      	: in DB.Bit_Length);

	procedure Gen_Private_Key(
						Public_Key  	: in Public_Key_ECNR;
                  Private_Key 	: out Private_Key_ECNR);

   procedure Sign(
						Public_Key  	: in out Public_Key_ECNR;
						Private_Key_1 	: in  Private_Key_ECNR;
                  SHA1_Hash   	: in  W_Block160;
                  Signature   	: out Signature_ECNR);

   function Verify(
						Public_Key  	: Public_Key_ECNR;
                  SHA1_Hash   	: W_Block160;
                  Signature  		: Signature_ECNR) return Boolean;

-------------------------------------------------------------------------------

   procedure Sign_File(
						Filename    	: in  String;
                  Public_Key  	: in out Public_Key_ECNR;
						Private_Key_1 	: in  Private_Key_ECNR;
                  Signature   	: out Signature_ECNR);

   function Verify_File(
						Filename   		: String;
                  Public_Key 		: Public_Key_ECNR;
                  Signature  		: Signature_ECNR) return Boolean;

   function Verify_Key_Pair(
                  Public_Key  	: Public_Key_ECNR;
						Private_Key 	: Private_Key_ECNR) return Boolean;

-------------------------------------------------------------------------------

	function equal_Public_Key_Curve(
 						Public_Key_A  : Public_Key_ECNR;
						Public_Key_B  : Public_Key_ECNR) return Boolean;

-------------------------------------------------------------------------------


private

   type ECNR_P_KEY is record
      E : Elliptic_Curve_Zp;
      P : EC_Point; --x,y
      n : Big_Unsigned;
      Q : EC_Point; --x,y
   end record;

   type ECNR_S_KEY is record
      Q : EC_Point; --x,y
      d : Big_Unsigned;
   end record;

   type ECNR_KEY is record
      C : Big_Unsigned;
      D : Big_Unsigned;
   end record;

   type Public_Key_ECNR is new ECNR_P_KEY;
   type Private_Key_ECNR is new ECNR_S_KEY;
   type Signature_ECNR is new ECNR_KEY;

   pragma Optimize (Time);



end Crypto.Asymmetric.ECNR;
