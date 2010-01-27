WITH Crypto.Asymmetric.RSA;

separate(Crypto.Certificate)

   PACKAGE BODY ASN1 IS
	PACKAGE RSA IS NEW Crypto.Asymmetric.RSA(2048);



   FUNCTION ConvertToString (Data : Unbounded_String) RETURN Unbounded_String IS
      StringOut : Unbounded_String := Null_Unbounded_String;
      Tag	: Unbounded_String;
      TmpString : Unbounded_String;
      Tmp 	: Unbounded_String;
      TmpInt   	: Integer;
      Char	: Character;
      Str	: String (1..1) := " ";
   BEGIN
      IF Length(Data) < 2 THEN 
         RETURN StringOut;
      ELSE
-- 	Put(" ConvertToString:");
         
 	TmpString := Data;
	While Length(TmpString) >= 2 Loop
						
		Tmp := To_Unbounded_String("16#") & Delete(TmpString,3,Length(TmpString)) & To_Unbounded_String("#");
		TmpString := Delete(TmpString,1,2);
		
------------------------------------------------------------------------------------------------------------------
 		Tmp := To_Unbounded_String(RSA.Big.Utils.To_String( RSA.Big.Utils.To_Big_Unsigned(To_String(Tmp)), 10));
 		TmpInt := Integer'VALUE(To_String(Tmp));
		If TmpInt > 31 AND TmpInt < 161 Then
			Char := Character'Val(TmpInt);
		Else 
			Char := '#';
		End If;
		Str(1) := Char;
		StringOut := StringOut & To_Unbounded_String(Str);
	End Loop;        
         
--	 Put(To_String(StringOut));
	 return StringOut;
      END IF;
   END ConvertToString;


   FUNCTION Get_TagLength(Data : Unbounded_String) RETURN Integer IS
      Struct : Unbounded_String;
      Tmp    : Unbounded_String;
      LTmp   : Integer;
   BEGIN
      IF Length(Data) < 3 THEN
         RETURN -1;
      ELSE
         Struct := Delete(Data,1,2);
         Tmp := Delete(Struct,3,Length(Struct));
         IF Tmp = "82" THEN
            Struct := Delete(Struct,1,2);
            Tmp := Delete(Struct,5,Length(Struct));
	 Elsif Tmp = "81" THEN
	    Struct := Delete(Struct,1,2);
            Tmp := Delete(Struct,3,Length(Struct));
         END IF;	
	Tmp := To_Unbounded_String("16#") & Tmp & To_Unbounded_String("#");
------------------------------------------------------------------------------------------------------------------
         Tmp := To_Unbounded_String(RSA.Big.Utils.To_String( RSA.Big.Utils.To_Big_Unsigned(To_String(Tmp)), 10));
         LTmp := Integer'VALUE(To_String(Tmp))*2;
         IF LTmp > Length(Data) THEN
            LTmp := Length(Data) - 4;
         END IF;
         return LTmp;
      END IF;
   END Get_TagLength;


   FUNCTION Get_TagValue(Data : Unbounded_String; TagLength : Integer ) RETURN Unbounded_String IS
      Struct   : Unbounded_String;
      TmpStruct: Unbounded_String := Null_Unbounded_String;
      Tag      : Unbounded_String;
      TL82     : Unbounded_String;
      TLength: Integer;
   BEGIN
      IF Length(Data) < 4 THEN
         RETURN To_Unbounded_String("");
      ELSE
	 
--         Put("GetValue "); --Put(To_String(Delete(Data,TagLength+5,Length(Data))));
         Tag := Delete(Data,3,Length(Data));
	 TL82 := Delete(Delete(Data,1,2),3,Length(Data)-2);
	 If To_String(TL82) = "82" Then
		Struct := Delete(Data,1,8);
	 Elsif To_String(TL82) = "81" Then
		Struct := Delete(Data,1,6); 
	 Else
		Struct := Delete(Data,1,4);
	 End If;
	 If (TagLength+1) <= Length(Struct) Then
		Struct := Delete(Struct,TagLength+1,Length(Struct));
	 End If;
         IF Tag = BOOLEAN_TAG OR Tag = INTEGER_TAG OR Tag = BITSTRING_TAG OR Tag = OBJECTIDENTIFIER_TAG OR Tag = PPRINTABLESTRING_TAG OR Tag = TIME_TAG OR Tag = IA5STRING_TAG THEN
            IF (Length(Struct) > TagLength+1) THEN
               Struct := Delete(Struct,TagLength+1,Length(Struct));
            END IF;
         ELSIF Tag = SEQUENCE_TAG OR Tag = SET_TAG OR Tag = BLOCK_0_TAG OR Tag = BLOCK_1_TAG OR Tag = BLOCK_2_TAG OR Tag = BLOCK_3_TAG  Then
	    TmpStruct := Struct;
            TLength := Get_TagLength(Struct);
	    If (TLength+8) < TagLength Then				-- (8 for ObjectTag TagLength and possible NullTag (0500)) 
		TmpStruct := Delete(TmpStruct,1,TLength+4);				
		TmpStruct := To_Unbounded_String(":::") & Get_TagValue(TmpStruct,Get_tagLength(TmpStruct));
		Struct := Get_TagValue(Struct,TLength) & TmpStruct;
	    Else
                Struct := Get_TagValue(Struct,TLength);
	    End If;        
         END IF;
	 --Put_Line(" GetValue=" & To_string(Struct) & "--  ");
         return Struct;
      END IF;
   END Get_TagValue;


   FUNCTION ConvertByteVersion(Data : Unbounded_String) RETURN Unbounded_String IS
      HexTmp	: Unbounded_String;
      HexResult	: Unbounded_String := Null_Unbounded_String;
   BEGIN
	HexTmp := Data;      
	IF Length(Data) > 2 THEN
         HexResult := Delete(HexTmp,3,Length(HexTmp));
	 HexTmp := Delete(HexTmp,1,2);
         WHILE ( Length(HexTmp) >= 2) LOOP
            HexResult := HexResult & To_Unbounded_String(":") & Delete(HexTmp,3,Length(HexTmp));
	    HexTmp := Delete(HexTmp,1,2);
         END LOOP;
      END IF;

      RETURN HexResult;
   END ConvertByteVersion;


   FUNCTION Get_SigAlgoByID(Data : Unbounded_String) RETURN Unbounded_String IS
      Algorithm : Unbounded_String := To_Unbounded_String("Undefined");
   BEGIN
      IF Length(Data) = Length(MD5RSA) THEN
         IF Data = MD5RSA THEN
            Algorithm := To_Unbounded_String("md5WithRSAEncryption");
         ELSIF Data = SHA1RSA THEN
            Algorithm := To_Unbounded_String("sha1WithRSAEncryption");
         END IF;
      END IF;
      RETURN Algorithm;
   END Get_SigAlgoByID;



   FUNCTION Get_Subject(Data : Unbounded_String; A : Integer) RETURN Unbounded_String IS
      	SubjectString : Unbounded_String := Null_Unbounded_String;
	Subject       : Unbounded_String;
	Tmp	      : Unbounded_String;
	TmpID	      : Unbounded_String;
	TmpSubject    : Unbounded_String;
	Tag           : Unbounded_String;
	TL82          : Unbounded_String;
	TagLength     : Integer;
	IndexN        : Integer;
	FirstTime     : Boolean := true;
	
	----- ObjectIdentifier for Issuer and Subject
	OI_C	: Constant Unbounded_String := To_Unbounded_String("550406");
	OI_CN	: Constant Unbounded_String := To_Unbounded_String("550403");
	OI_SN	: Constant Unbounded_String := To_Unbounded_String("550404");
	OI_O	: Constant Unbounded_String := To_Unbounded_String("55040A");
	OI_PA	: Constant Unbounded_String := To_Unbounded_String("550410");
	OI_N	: Constant Unbounded_String := To_Unbounded_String("55042A");
	OI_DQ	: Constant Unbounded_String := To_Unbounded_String("55042E");
	OI_L	: Constant Unbounded_String := To_Unbounded_String("550407");
	OI_OU	: Constant Unbounded_String := To_Unbounded_String("55040B");
	OI_DC	: Constant Unbounded_String := To_Unbounded_String("0992268993F22C640119");
   BEGIN
--      Put_Line("Beginn Subject " & A'Img);
      TBS(A).Value := Null_Unbounded_String;
      IF Length(Data) > 16 THEN
	Tag := Delete(Data,3,Length(Data));
        IF Tag = SEQUENCE_TAG Then
		TagLength := Get_TagLength(Data);
		TL82 := Delete(Delete(Data,1,2),3,Length(Data)-2);
		If To_String(TL82) = "82" Then
			Subject := Delete(Data,1,8);
		Elsif To_String(TL82) = "81" Then
			Subject := Delete(Data,1,6);
		Else
			Subject := Delete(Data,1,4);
		End If;		
		
		Subject := Delete(Subject,TagLength+1,Length(Subject));
		TagLength := Get_TagLength(Subject);
		--Subject := Delete(Subject,TagLength+1,Length(Subject));
		Tag := Delete(Subject,3,Length(Subject));
		While (Tag = SET_TAG) Loop
			If FirstTime = false Then
				SubjectString := SubjectString & To_Unbounded_String(", ");
			Else
				FirstTime := false;				
			End If;
			TagLength := Get_TagLength(Subject);						
			Tmp := Get_TagValue(Subject,TagLength);
			IndexN := Index(Tmp, ":::");
			If indexN > 0 Then
				TmpID := Delete(Tmp,IndexN, Length(Tmp));
				TBS(A).Value := TBS(A).Value & TmpID;
	
				If    TmpID = OI_C Then 
					TmpID := To_Unbounded_String(" C=");
				Elsif TmpID = OI_CN Then 
					TmpID := To_Unbounded_String(" CN=");
				Elsif TmpID = OI_SN Then 
					TmpID := To_Unbounded_String(" SN=");
				Elsif TmpID = OI_O Then 
					TmpID := To_Unbounded_String(" O=");
				Elsif TmpID = OI_PA Then 
					TmpID := To_Unbounded_String(" PA=");
				Elsif TmpID = OI_N Then 
					TmpID := To_Unbounded_String(" N=");
				Elsif TmpID = OI_DQ Then 
					TmpID := To_Unbounded_String(" DQ=");
				Elsif TmpID = OI_L Then 
					TmpID := To_Unbounded_String(" L=");
				Elsif TmpID = OI_OU Then 
					TmpID := To_Unbounded_String(" OU=");
				Elsif TmpID = OI_DC Then 
					TmpID := To_Unbounded_String(" DC=");
				End If;

				TmpSubject := Delete(Tmp,1,IndexN+2);
				TBS(A).Value := TBS(A).Value & TmpSubject;
				
				TmpSubject := ConvertToString(TmpSubject);					
			
				SubjectString := SubjectString & TmpID & TmpSubject;
			Else
				SubjectString := SubjectString & Tmp;
			End If;					
			Subject := Delete(Subject,1,TagLength+4);
			Tag := Delete(Subject,3,Length(Subject));
			Tmp := Null_Unbounded_String;						
		End Loop;		
	End If;
      END IF;
      RETURN SubjectString;
   END Get_Subject;

	FUNCTION Get_Validity(Data : Unbounded_String) RETURN Unbounded_String IS
		ValidT: Unbounded_String := To_Unbounded_String("Undefined");
		Year  : Integer;
		Month : Integer;
		Day   : Integer;
		Time  : Unbounded_String;

		Month_T: ARRAY (1..12) OF Unbounded_String := (
		    1=>To_Unbounded_String("Jan"),
		    2=>To_Unbounded_String("Feb"),
		    3=>To_Unbounded_String("Mar"),
		    4=>To_Unbounded_String("Apr"),
		    5=>To_Unbounded_String("May"),
		    6=>To_Unbounded_String("Jun"),
		    7=>To_Unbounded_String("Jul"),
		    8=>To_Unbounded_String("Aug"),
		    9=>To_Unbounded_String("Sep"),
		    10=>To_Unbounded_String("Okt"),
		    11=>To_Unbounded_String("Nov"),
		    12=>To_Unbounded_String("Dec") );
	BEGIN
		If Length(Data) = 26 Then		-- UTC Time		
			ValidT := ConvertToString(Data);
			Year := Integer'VALUE(To_String(Delete(ValidT,3,Length(ValidT))));
			ValidT := Delete(ValidT,1,2);
			Month:= Integer'VALUE(To_String(Delete(ValidT,3,Length(ValidT))));
			ValidT := Delete(ValidT,1,2);
			Day  := Integer'VALUE(To_String(Delete(ValidT,3,Length(ValidT))));
			ValidT := Delete(ValidT,1,2);
			Time := Delete(ValidT,7,Length(ValidT));
			ValidT := Delete(ValidT,1,6);
			
			If To_String(ValidT) = "Z" Then
				ValidT := To_Unbounded_String("GMT");
			End If;

			If Year < 50 Then				
				Year := 2000 + Year;
			Else
				Year := 1900 + Year;				
			End If;

			Time := Delete(Time,3,Length(Time)) & To_Unbounded_String(":") & Delete(Delete(Time,1,2),3,Length(Time)-2) & To_Unbounded_String(":") & Delete(Time,1,Length(Time)-2);
			ValidT := Month_T(Month) & To_Unbounded_String(Day'Img) & To_Unbounded_String(" ") & Time & To_Unbounded_String(Year'Img) & To_Unbounded_String(" ") & ValidT;
		End If;					--TODO GENERALIZED TIME		
		
		RETURN ValidT;
   	END Get_Validity;

	FUNCTION Get_PublicKey(Data : Unbounded_String) RETURN Unbounded_String IS
		PData    : Unbounded_String;	
		PublicKey: Unbounded_String;		
		Tag      : Unbounded_String;
		TValue   : Unbounded_String;
		TL82	 : Unbounded_String;
		TLength  : Integer;
	BEGIN
--		Put_Line("Get Public Key"); 
		TLength := Get_TagLength(Data);	
		
		Tag := Delete(Data,3,Length(Data));
		TL82 := Delete(Delete(Data,1,2),3,Length(Data)-2);
		If To_String(TL82) = "82" Then
			PData := Delete(Data,TLength+9,Length(Data));
		Elsif To_String(TL82) = "81" Then
			PData := Delete(Data,TLength+7,Length(Data));
		Else
			PData := Delete(Data,TLength+5,Length(Data));
		End If;

		PublicKey := Get_TagValue(PData,TLength);
		
--		Put_Line("End Public Key");
		RETURN PublicKey;
   	END Get_PublicKey;

	FUNCTION Get_Extensions(Data : Unbounded_String) RETURN Unbounded_String IS
		EData    : Unbounded_String;
		ObjectID : Unbounded_String;	
		Extension: Unbounded_String := Null_Unbounded_String;
		SeqTag   : Unbounded_String;
		Tag      : Unbounded_String;
		TValue   : Unbounded_String;
		TL82	 : Unbounded_String;
		TLength  : Integer;
	BEGIN
--		Put_Line("Get Extensions"); Put_Line(To_String(Data));
		Tag := Delete(Data,3,Length(Data));
		IF Tag = BLOCK_3_TAG THEN
			TLength := Get_TagLength(Data);			
			TL82 := Delete(Delete(Data,1,2),3,Length(Data)-2);
			If To_String(TL82) = "82" Then
				EData := Delete(Data,TLength+9,Length(Data));
				EData := Delete(EData,1,8);
			Elsif To_String(TL82) = "81" Then
				EData := Delete(Data,TLength+7,Length(Data));
				EData := Delete(EData,1,6);
			Else
				EData := Delete(Data,TLength+5,Length(Data));
				EData := Delete(EData,1,4);
			End If;
			
			Tag := Delete(EData,3,Length(EData));
			If Tag = SEQUENCE_TAG Then
				TLength := Get_TagLength(EData);			
				TL82 := Delete(Delete(EData,1,2),3,Length(EData)-2);
				If To_String(TL82) = "82" Then
					EData := Delete(EData,1,8);
				Elsif To_String(TL82) = "81" Then
					EData := Delete(EData,1,6);
				Else
					EData := Delete(EData,1,4);
				End If;				
				
				TLength := Get_TagLength(EData);
				SeqTag := Delete(EData, TLength+5, Length(EData));
				SeqTag := Delete(SeqTag,1,4);				
				While Length(SeqTag) > 0 Loop					
					TLength := Get_TagLength(SeqTag);
					SeqTag := Delete(SeqTag,1,TLength+4);
				End Loop;
				 
			End If;

--			TLength := Get_TagLength(PData);
--			Extension := Get_TagValue(PData,TLength);
			
--			ObjectID := Get_TagValue(PData,TLength);

--			PData := Delete(PData,1,TLength);
--			TLength := Get_TagLength(PData);
--			PublicKey := Get_TagValue(PData,TLength);								
		End If;	
			
--		Put_Line("End Extensions");
--		Put_Line(To_String(Extension));		
		RETURN Extension;
   	END Get_Extensions;

--	FUNCTION Subject(ASN1Data : Unbounded_String) RETURN Unbounded_String is
--	Begin
--	End Subject;


   PROCEDURE Parse(ASN1Data : Unbounded_String) IS
	Data     : Unbounded_String;
	Tag      : Unbounded_String;
	TL82     : Unbounded_String;
	TValue   : Unbounded_String;
	SignData : Unbounded_String;
	PTmp	 : Unbounded_String;
	
	NotBefore: Unbounded_String;
	NotAfter : Unbounded_String;
	TLength  : Integer;
	MaxLength: Integer;
	TbsLength: Integer;
	IndexN   : Integer;
	Attribute: Integer := 1;
	AString  : ARRAY (1..9) OF Unbounded_String := (
	 1=>To_Unbounded_String("Version:"),
	 2=>To_Unbounded_String("Serial Number:"),
	 3=>To_Unbounded_String("Signature Algorithm:"),
	 4=>To_Unbounded_String("Issuer:"),
	 5=>To_Unbounded_String("Validity"),
	 6=>To_Unbounded_String("Subject:"),
	 7=>To_Unbounded_String("Subject Public Key Info:"),
	 --8=>To_Unbounded_String("Issuer Unique ID:") );   -- v2
	 --9=>To_Unbounded_String("Subject Unique ID:") );
	 8=>To_Unbounded_String("X509v3 extensions:") ,    -- v3
	 9=>To_Unbounded_String("----------") );
   BEGIN
--      Put_Line("--Parse ASN1 --");
      IF Length(ASN1Data) = 0 THEN
         RAISE Conversion_Error;
      ELSE
         Data := ASN1Data; 	--
--	 
         Tag := Delete(Data,3,Length(Data));
         IF Tag /= SEQUENCE_Tag THEN
            RAISE ASN1_Structure_Error;
         ELSE
            MaxLength:= Get_TagLength(Data);
            IF MaxLength <= 16 THEN
               RAISE ASN1_Structure_Error;
            ELSE
               Data := Delete(Data,1,8);
         ------------------------------------ TBS Sequence ---------------------------------------
               Tag := Delete(Data,3,Length(Data));
               IF Tag /= SEQUENCE_Tag THEN
                  RAISE ASN1_Structure_Error;
               ELSE
                  TbsLength := Get_TagLength(Data);
                  Data := Delete(Data,1,8); 
		  SignData := Delete(Data,1,TbsLength);
		  Data := Delete(Data,TbsLength+1,Length(Data));		--TBS
                  Cert(Attribute).Value := To_Unbounded_String("01");
		  TBS(Attribute).Value := To_Unbounded_String("00");
                  WHILE Length(Data) > 0 LOOP
	             TLength := Get_TagLength(Data);
			

		     TL82 := Delete(Delete(Data,1,2),3,Length(Data)-2);
		     Tag := Delete(Data,3,Length(Data));
		     
		     If Attribute = 1 Then
			If To_String(Tag) = "A0" Then 
				TValue := Get_TagValue( Data, TLength);
				If To_String(TValue) = "00" Then
					TBS(Attribute).Value := TValue;
					TValue := To_Unbounded_String("01");					
				Elsif To_String(TValue) = "01" Then
					TBS(Attribute).Value := TValue;
					TValue := To_Unbounded_String("02");
				Elsif To_String(TValue) = "02" Then
					TBS(Attribute).Value := TValue;
					TValue := To_Unbounded_String("03");
				Else							-- Default
					TBS(Attribute).Value := To_Unbounded_String("00");
					TValue := To_Unbounded_String("01");
				End If;
			Else
				TBS(Attribute).Attribute := AString(Attribute);
				Cert(Attribute).Attribute := TBS(Attribute).Attribute;			
				Attribute := Attribute + 1;
				TValue := Get_TagValue( Data, TLength);
			End If;			
		     Elsif Attribute = 2 Then 				-- Version
			TValue := Get_TagValue( Data, TLength);
		     Elsif Attribute = 3 THEN				-- SigAlgoId		
			TValue := Get_TagValue( Data, TLength);		
		     Elsif Attribute = 4 OR Attribute = 6  THEN		-- Issuer and Subject
			TValue := Get_Subject(Data,Attribute);
		     Elsif Attribute = 5 Then				-- Validity
			TValue := Get_TagValue( Data, TLength );
			IndexN := Index(TValue, ":::");
			If (IndexN > 0) Then
				NotBefore := Delete(TValue,IndexN, Length(TValue));
				TBS(Attribute).Value := NotBefore;
				NotBefore := Get_Validity(NotBefore);
				NotAfter := Delete(TValue,1,IndexN+2);
				TBS(Attribute).Value := TBS(Attribute).Value & NotAfter;
				NotAfter := Get_Validity(NotAfter);
				TValue := To_Unbounded_String(" Not Before: ") & NotBefore & To_Unbounded_String("  ") & To_Unbounded_String("Not After : ") & NotAfter;
			End If;
		     Elsif Attribute = 7 Then				-- PublicKey			
			TValue := Get_PublicKey( Data );
			IndexN := Index(TValue, ":::");
			If indexN > 0 Then
				PTmp := Delete(TValue,IndexN, Length(TValue));				
				TBS(Attribute).Value := PTmp;
				Public_Key.Attribute := PTmp;
				If To_String(PTmp) = RSAEnrypt Then
					Cert(Attribute).Value := To_Unbounded_String("Public Key Algorithm: rsaEncryption  ");
				Else
					Cert(Attribute).Value := PTmp;
				End If;
				PTmp := Delete(TValue,1,IndexN+4);				
				TBS(Attribute).Value := TBS(Attribute).Value & PTmp;
				--TLength := Get_TagLength(PTmp);
				Public_Key.Value := Get_TagValue(PTmp,TLength);
--				Put_Line("PublicKey:" & To_String(PTmp));
--				Put_Line("PublicKey:" & To_String(Public_Key.Value));
				Cert(Attribute).Value:= Cert(Attribute).Value & PTmp;
				IndexN := Index(Public_Key.Value, ":::");
				If IndexN > 0 Then
					PTmp := Delete(Public_Key.Value,1,IndexN+4);
					Exponent := Get_TagValue(PTmp,Get_TagLength(PTmp));
					Put_Line("Exponent:" & To_String(Exponent));
					Public_Key.Value := Delete(Public_Key.Value,IndexN, Length(Public_Key.Value));					
				End If;				
			End If;	
		     Elsif Attribute = 8 Then
			TValue := Get_Extensions( Data );
			If TValue = Null_Unbounded_String Then
				Attribute := Attribute + 1;
			End If;		
--		     Else 
--			TValue := Get_TagValue( Data, TLength);
		     End If;

	             
	             If Attribute = 2 OR Attribute = 3  THEN 			
			TBS(Attribute).Attribute := AString(Attribute);
			TBS(Attribute).Value := TValue;
		     Else 
			TBS(Attribute).Attribute := AString(Attribute);
--			TBS(Attribute).Value := TValue;
		     End If;

	 ----------------- Convert for Cert in textform  ---------------------
	             IF Attribute = 2 THEN
	                Cert(Attribute).Value := ConvertByteVersion(TValue);
	             ELSIF Attribute = 3 THEN
	                Cert(Attribute).Value := Get_SigAlgoByID(TValue);
		     ELSIF Attribute = 7 THEN
	                Cert(Attribute).Attribute := TBS(Attribute).Attribute;
	             ELSE
	                Cert(Attribute).Value := TValue;
	             END IF;
		     Cert(Attribute).Attribute := TBS(Attribute).Attribute;
	----------------------------------------------------------------------
     
			If To_String(TL82) = "82" Then
				Data := Delete(Data,1,TLength+8);
			Elsif To_String(TL82) = "81" Then
				Data := Delete(Data,1,TLength+6);
			Else
				Data := Delete(Data,1,TLength+4);
			End If;
	   

		     IF (Length(Data) > 0) And Attribute > 8 THEN
			New_Line;Put_Line("To Long TBS Length in ASN1");
		     END IF;
		     If (Attribute < 9) Then
			Attribute := Attribute + 1;
		     End If;

                  END LOOP;
       ---------------------------------- SignaturAlgorithm ------------------------------------
		  TLength := Get_TagLength(SignData); 
		  SignAlgorithm.Attribute :=  Get_SigAlgoByID(Get_TagValue(SignData,TLength));		 	  
		  SignData := Delete(SignData,1,TLength+4);
		  TLength := Get_TagLength(SignData);
		  SignAlgorithm.Value     :=  Get_TagValue(SignData,TLength);
               END IF;

            END IF;

         END IF;

      END IF;

	IF TBS(4).Value = TBS(6).Value THEN		--Issuer and  Subject same?
	    RootCertificate := True;
	ELSE
	    RootCertificate := False;
	END IF;
--	Put_Line("SignData:");
--	Put_Line(To_String(SignAlgorithm.Attribute));
--	Put_Line(To_String(SignAlgorithm.Value));
--      ASN1_Out;

   EXCEPTION
         WHEN Conversion_Error =>
            Put_Line(File => Standard_Error, Item => "Conversion failed!");
         WHEN ASN1_Structure_Error =>
            Put_Line(File => Standard_Error, Item => "Invalid ASN1 Structure!");

   END Parse;


   PROCEDURE ASN1_Out IS
      I : Integer := 1;

   BEGIN
      Put_Line("--ASN1 OUT--");
      WHILE (Length(Cert(I).Attribute)) > 0 LOOP
         New_Line;
         Put(To_String(Cert(I).Attribute));
         Put(To_String(Cert(I).Value));
         New_Line;
         I := I + 1;
      END LOOP;
	I:= 1;
	Put_Line("--ASN1 TBS OUT--");
      WHILE (Length(TBS(I).Attribute)) > 0 LOOP
         New_Line;
         Put(To_String(TBS(I).Attribute));
         Put(To_String(TBS(I).Value));
         New_Line;
         I := I + 1;
      END LOOP;
--        CertOut;
   END ASN1_Out;

END ASN1;
