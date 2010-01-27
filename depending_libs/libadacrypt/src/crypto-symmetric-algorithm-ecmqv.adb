with Ada.Integer_Text_IO;

package body Crypto.Symmetric.Algorithm.ECMQV is

   use Big.Mod_Utils;
   use Big.Utils;

-------------------------------------------------------------------------------

   -- check if key is initialited
   function Is_Init(Key : ECMQV_P_KEY) return Boolean is
   begin
      if Key.n /= Big_Unsigned_Zero then
         return True;
      else return False;
      end if;
   end Is_Init; Pragma Inline (Is_Init);

-------------------------------------------------------------------------------

   -- check if key is initialited
   function Is_Init(Key : ECMQV_S_KEY) return Boolean is
   begin
      if Key.d /= Big_Unsigned_Zero then
			return True;
      else return False;
      end if;
   end Is_Init; Pragma Inline (Is_Init);



-------------------------------------------------------------------------------

   procedure Gen_Single_Public_Key(
						Public_Key_A  : out Public_Key_ECMQV;
						length      : in DB.Bit_Length) is
	begin
		Get_Elliptic_Curve(Public_Key_A.E, Public_Key_A.P, Public_Key_A.n, length);
		init(Public_Key_A.E);
	end Gen_Single_Public_Key;

-------------------------------------------------------------------------------

	procedure Gen_Public_Keys(
						Public_Key_A_1 : out Public_Key_ECMQV;
						Public_Key_A_2 : out Public_Key_ECMQV;
						length        	: in DB.Bit_Length) is
	begin
		Gen_Single_Public_Key(Public_Key_A_1, length);
		Gen_Single_Public_Key(Public_Key_A_2, length);
	end Gen_Public_Keys;


-------------------------------------------------------------------------------

   procedure Gen_Single_Private_Key(
						Public_Key_A  : in out Public_Key_ECMQV;
						Private_Key_A : out  Private_Key_ECMQV) is
	begin
		Private_Key_A.d := Get_Random(Public_Key_A.E.P - Big_Unsigned_Three) + Big_Unsigned_One;

		Private_Key_A.Q := Private_Key_A.d * Public_Key_A.P;
		Public_Key_A.Q := Private_Key_A.Q;
	end Gen_Single_Private_Key;

-------------------------------------------------------------------------------

   procedure Gen_Private_Keys(
						Public_Key_A_1	: in out Public_Key_ECMQV;
						Public_Key_A_2	: in out Public_Key_ECMQV;
						Private_Key_A_1: out  Private_Key_ECMQV;
						Private_Key_A_2: out  Private_Key_ECMQV) is
	begin
		Gen_Single_Private_Key(Public_Key_A_1, Private_Key_A_1);
		Gen_Single_Private_Key(Public_Key_A_2, Private_Key_A_2);
	end Gen_Private_Keys;

-------------------------------------------------------------------------------

   procedure Gen_Shared_Private_Key(
						Public_Key_B_1	: in Public_Key_ECMQV;
						Public_Key_B_2	: in Public_Key_ECMQV;
						Private_Key_A_1: in Private_Key_ECMQV;
						Private_Key_A_2: in Private_Key_ECMQV;
						Shared_Key_A	: out Shared_Key_ECMQV) is
		tmp_BU_1, tmp_BU_2, tmp_BU_3 : Big_Unsigned;
		h : Big_Unsigned := log2(Public_Key_B_1.n) / Big_Unsigned_Two;
		SHARED_KEY_EXEPTION : exception;
		tmp_EC : EC_Point;
	begin
		if equal_Public_Key_Curve(Public_Key_B_1, Public_Key_B_2) then
			null;
		else
			raise SHARED_KEY_EXEPTION with "Domain Parameters are not equal";
		end if;

		h := pow(Big_Unsigned_Two, h, Public_Key_B_1.E.P);

		tmp_BU_1 := Private_Key_A_2.Q.X mod h;
		tmp_BU_1 := add(tmp_BU_1, h, Public_Key_B_1.E.P);

		tmp_BU_2 := Public_Key_B_2.Q.X mod h;
		tmp_BU_2 := add(tmp_BU_2, h, Public_Key_B_1.E.P);

		tmp_BU_3 := add(mult(tmp_BU_1, Private_Key_A_1.d, Public_Key_B_1.n), Private_Key_A_2.d, Public_Key_B_1.n);

		tmp_EC  := tmp_BU_3 * (Public_Key_B_2.Q + (tmp_BU_2 *  Public_Key_B_1.Q));

		if tmp_EC = EC_Point_Infinity then
			raise SHARED_KEY_EXEPTION with "Key is Point in Infinity";
		else
			Shared_Key_A.z := tmp_EC.X;
		end if;

	end Gen_Shared_Private_Key;

-------------------------------------------------------------------------------

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
						Shared_Key_B	: Shared_Key_ECMQV) return Boolean is

		tmp_bool_1, tmp_bool_2 : Boolean := false;
	begin

		if equal_Public_Key_Curve(Public_Key_A_1, Public_Key_A_2) and equal_Public_Key_Curve(Public_Key_A_1, Public_Key_B_1) and equal_Public_Key_Curve(Public_Key_A_1, Public_Key_B_2) then
			tmp_bool_1 := true;
		end if;

		if Shared_Key_A.z = Shared_Key_B.z then
			tmp_bool_2 := true;
		end if;

		if tmp_bool_1 and tmp_bool_2 then
			return true;
		else
			return false;
		end if;
	end Verify;

-------------------------------------------------------------------------------

	function equal_Public_Key_Curve(
 						Public_Key_A  : Public_Key_ECMQV;
						Public_Key_B  : Public_Key_ECMQV) return Boolean is
	begin
		if (Public_Key_A.E = Public_Key_B.E) and (Public_Key_A.P = Public_Key_B.P) and (Public_Key_A.n = Public_Key_B.n) then
			return true;
		else
			return false;
		end if;
	end equal_Public_Key_Curve;

-------------------------------------------------------------------------------

	function log2(input : Big_Unsigned) return Big_Unsigned is
		tmp_BU : Big_Unsigned := Big_Unsigned_One;
		tmp_BU_count : Big_Unsigned := Big_Unsigned_Zero;
		tmp_BU_2 : Big_Unsigned := input;
	begin
		while tmp_BU <= input loop
			tmp_BU := tmp_BU * Big_Unsigned_Two;
			tmp_BU_count := tmp_BU_count + Big_Unsigned_One;

		end loop;
		return tmp_BU_count-Big_Unsigned_One;
	end;

-------------------------------------------------------------------------------


end Crypto.Symmetric.Algorithm.ECMQV;
