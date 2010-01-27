
with Crypto.Types.Big_Numbers;
with Crypto.Types.Elliptic_Curves.Zp;
with Crypto.Types.Elliptic_Curves.Zp.Database;

use Crypto.Types;

generic
   Size : Positive;


package Crypto.Symmetric.Algorithm.ECMQV is

   package Big is new Crypto.Types.Big_Numbers(Size);
   use Big;
   package EC  is new Crypto.Types.Elliptic_Curves(Big);
   use EC;
   package Zp is new EC.Zp;
   use Zp;
	package DB is new Zp.Database;
	use DB;


   type Public_Key_ECMQV is private;
   type Private_Key_ECMQV is private;

   type ECMQV_KEY is record
		z : Big_Unsigned;
   end record;

   type Shared_Key_ECMQV is new ECMQV_KEY;

-------------------------------------------------------------------------------

   procedure Gen_Single_Public_Key(
						Public_Key_A  	: out Public_Key_ECMQV;
						length        	: in DB.Bit_Length);

	procedure Gen_Public_Keys(
						Public_Key_A_1 : out Public_Key_ECMQV;
						Public_Key_A_2 : out Public_Key_ECMQV;
						length        	: in DB.Bit_Length);

   procedure Gen_Single_Private_Key(
						Public_Key_A  	: in out Public_Key_ECMQV;
						Private_Key_A 	: out  Private_Key_ECMQV);

   procedure Gen_Private_Keys(
						Public_Key_A_1	: in out Public_Key_ECMQV;
						Public_Key_A_2	: in out Public_Key_ECMQV;
						Private_Key_A_1: out  Private_Key_ECMQV;
						Private_Key_A_2: out  Private_Key_ECMQV);

   procedure Gen_Shared_Private_Key(
						Public_Key_B_1	: in Public_Key_ECMQV;
						Public_Key_B_2	: in Public_Key_ECMQV;
						Private_Key_A_1: in Private_Key_ECMQV;
						Private_Key_A_2: in Private_Key_ECMQV;
						Shared_Key_A	: out Shared_Key_ECMQV);

   function Verify(
						Public_Key_A_1 : Public_Key_ECMQV;
						Public_Key_A_2 : Public_Key_ECMQV;
						Public_Key_B_1 : Public_Key_ECMQV;
						Public_Key_B_2 : Public_Key_ECMQV;
						Private_Key_A_1: Private_Key_ECMQV;
						Private_Key_A_2: Private_Key_ECMQV;
						Private_Key_B_1: Private_Key_ECMQV;
						Private_Key_B_2: Private_Key_ECMQV;
						Shared_Key_A	: Shared_Key_ECMQV;
						Shared_Key_B	: Shared_Key_ECMQV) return Boolean;

	function equal_Public_Key_Curve(
 						Public_Key_A  : Public_Key_ECMQV;
						Public_Key_B  : Public_Key_ECMQV) return Boolean;

	function log2(input : Big_Unsigned) return Big_Unsigned;

-------------------------------------------------------------------------------

private

   type ECMQV_P_KEY is record
      E : Elliptic_Curve_Zp;
      P : EC_Point; --x,y
      n : Big_Unsigned;
      Q : EC_Point; --x,y
   end record;

   type ECMQV_S_KEY is record
      Q : EC_Point; --x,y
		d : Big_Unsigned;
   end record;

   type Public_Key_ECMQV is new ECMQV_P_KEY;
   type Private_Key_ECMQV is new ECMQV_S_KEY;

   pragma Optimize (Time);

end Crypto.Symmetric.Algorithm.ECMQV;
