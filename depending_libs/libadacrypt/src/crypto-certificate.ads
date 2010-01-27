WITH Ada.Strings.Unbounded; USE Ada.Strings.Unbounded;
--WITH Crypto.Types.Big_Numbers;


PACKAGE Crypto.Certificate IS

--   PACKAGE Big is new Crypto.Types.Big_Numbers(2048);
--   use Big;

   PROCEDURE CertOut;   --print Cert

   PROCEDURE Get_Cert(Filename: String);

   FUNCTION AnalyseSigned RETURN Unbounded_String;



--   PROCEDURE DecryptSignature;

   ---------------------------------------------------------------------------
   -----------------------------Exceptions------------------------------------
   ---------------------------------------------------------------------------

   Cert_Typ_Error       : EXCEPTION;
   Cert_Structure_Error : EXCEPTION;

   ---------------------------------------------------------------------------
   --------------------------------PRIVATE------------------------------------
   ---------------------------------------------------------------------------

   PRIVATE
   PROCEDURE Init;

   FUNCTION Trust RETURN Boolean;

   PROCEDURE Validate;

   FUNCTION ValidateStructure(Count : Integer) RETURN Boolean;

   FUNCTION ExecuteValue RETURN Boolean;

   FUNCTION DecryptSignature Return Unbounded_String;


   TYPE ValueType IS   --value type of the certification
      RECORD
         Attribute: Unbounded_String;
         Value: Unbounded_String;
      END RECORD;

   RootCertificate : Boolean;

   ----- Values of Imported Certificate
   CertString : Unbounded_String;
   Cert: ARRAY(1..11) OF ValueType;
   SignAlgorithm :  ValueType;

   ---------------------------------------------------------------------------
   --------------------------------ASN 1------------------------------------
   ---------------------------------------------------------------------------

   PACKAGE ASN1 IS


      BLOCK_0_TAG          : CONSTANT String := "A0";
      BLOCK_1_TAG          : CONSTANT String := "A1";
      BLOCK_2_TAG          : CONSTANT String := "A2";
      BLOCK_3_TAG          : CONSTANT String := "A3";
      BOOLEAN_TAG          : CONSTANT String := "01";
      INTEGER_TAG          : CONSTANT String := "02";
      BITSTRING_TAG        : CONSTANT String := "03";   --?
      NULL_TAG             : CONSTANT String := "05";   --Length=0  == 0500h
      OBJECTIDENTIFIER_TAG : CONSTANT String := "06";
      PPRINTABLESTRING_TAG : CONSTANT String := "13";
      TIME_TAG             : CONSTANT String := "17";   --GENERALIZEDTIME Length=15;   UTCTIME Length=13;
      SEQUENCE_TAG         : CONSTANT String := "30";
      SET_TAG              : CONSTANT String := "31";
      IA5STRING_TAG        : CONSTANT String := "16";
	
      -- SignatureAlgorithm ID
      RSAEnrypt            : CONSTANT Unbounded_String := To_Unbounded_String("2A864886F70D010101");   --RSAEncryption
      MD5RSA               : CONSTANT Unbounded_String := To_Unbounded_String("2A864886F70D010104");   --md5WithRSAEncryption
      SHA1RSA              : CONSTANT Unbounded_String := To_Unbounded_String("2A864886F70D010105");   --sha-1WithRSAEncryption

      TYPE ValueType IS   --value type of the asn1
      RECORD
         Attribute: Unbounded_String;
         Value: Unbounded_String;
      END RECORD;

      TBS : ARRAY(1..11) OF ValueType;		-- all Cert values in binary
      ASN_Issuer : Unbounded_String;
      Signature : ValueType;
      Public_Key: ValueType;
      Exponent  : Unbounded_String;
--      FUNCTION Subject(ASN1Data : Unbounded_String) RETURN Unbounded_String;
      FUNCTION Get_TagValue(Data : Unbounded_String; TagLength : Integer) RETURN Unbounded_String;

      PROCEDURE Parse(ASN1Data : Unbounded_String);
      PROCEDURE ASN1_Out;               --print Cert
--      FUNCTION Commit RETURN Boolean;   --Value

      ---------------------------------------------------------------------------
      -----------------------------Exceptions------------------------------------
      ---------------------------------------------------------------------------

      ASN1_Typ_Error       : EXCEPTION;
      ASN1_Structure_Error : EXCEPTION;
      Conversion_Error     : EXCEPTION;

   END ASN1;

END Crypto.Certificate;
