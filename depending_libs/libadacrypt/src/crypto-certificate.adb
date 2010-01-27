--WITH Ada.Strings.Unbounded; USE Ada.Strings.Unbounded;
WITH Ada.Text_IO, Ada.Integer_Text_IO; USE Ada.Text_IO, Ada.Integer_Text_IO;
WITH Ada.Strings.Fixed, Ada.Strings.Maps;
WITH Ada.Calendar;
WITH Ada.Directories;
WITH Crypto.Asymmetric.RSA;
WITH Crypto.Hashfunction_SHA1;
WITH Crypto.Types.Big_Numbers;

pragma Elaborate_All (Crypto.Asymmetric.RSA);
pragma Elaborate_All (Crypto.Types.Big_Numbers);

PACKAGE BODY Crypto.Certificate IS
   PACKAGE BODY ASN1 IS SEPARATE;

   PACKAGE RSA IS NEW Crypto.Asymmetric.RSA(2048);
   
   use Crypto.Hashfunction_SHA1;


   Seperators : CONSTANT Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set(" .,;?!" & ASCII.HT & ASCII.LF);

   FileExist: Boolean := True;

   CertTyp  : String := "   ";
   Attribute: ARRAY (1..9) OF Unbounded_String := (
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

   AttributeLength : CONSTANT Integer := 11;


   PROCEDURE Init IS
   BEGIN
      CertString  := Null_Unbounded_String;
      RootCertificate := False;
   END Init;


   PROCEDURE CertOut IS
      I: Integer := 1;
   BEGIN
      WHILE (Length(Cert(I).Attribute)) > 0 LOOP
         New_Line;
         Put(To_String(Cert(I).Attribute));
         Put(To_String(Cert(I).Value));
         New_Line;
         I := I + 1;
      END LOOP;

      New_Line;New_Line;
      Put("Signature Algorithm: ");
      Put(To_String(SignAlgorithm.Attribute));
      New_Line;
      Put(To_String(SignAlgorithm.Value));
   

   END CertOut;


	Function To_Hex_Token(TokenS: Unbounded_String) Return Unbounded_String IS

		T      : RSA.Big.Big_Unsigned;
		Token  : Unbounded_String;
		IndexN : Integer;	
	BEGIN
		Token := TokenS;
--		Put(To_String(Token));Put(" ");
				
		T := RSA.Big.Utils.To_Big_Unsigned(To_String(Token));
		Token := To_Unbounded_String(RSA.Big.Utils.To_String(T,16));
		IndexN := Index(Token,"16#");
		If IndexN > 0 Then
			Token := Delete(Token,IndexN,IndexN+2);
		end if;
		IndexN := Index(Token,"#");
		If IndexN > 0 Then
			Token := Delete(Token,IndexN,Length(Token));
		end if;
		IF Length(Token) < 2 THEN
	       	   	Token:= To_Unbounded_String("0") & Token;
	       	END IF;
		return Token;
	END To_Hex_Token;



      FUNCTION ValidateStructure(Count : Integer) RETURN Boolean IS
         Version : Integer;
         MustHave: CONSTANT Integer := 7;
         Max     : Integer;
         First   : Integer;
         Last    : Integer;
         Tmp     : Unbounded_String;
         Result : Boolean := True;
      BEGIN
         --New_Line;Put("--Validate Structure--");
         IF Count < MustHave THEN
            Result := False;
            New_Line;Put("Less Than Attributes!");
         END IF;
         Tmp := Cert(1).Value;
         Ada.Strings.Fixed.Find_Token(Source => Ada.Strings.Unbounded.To_String(Tmp), Set => Seperators, Test => Ada.Strings.Inside, First => First, Last => Last);
         IF First = 1 THEN
            Tmp:= Delete(Tmp,First,Last);   --by find token inside
         END IF;
         Tmp := Delete(Tmp,2,Length(Tmp));   --Version Number execute; (first Position of the Value)

         Version := Integer'VALUE(To_String(Tmp));	--StrToInt(Tmp);

         CASE Version IS
            WHEN 1   =>
               Max := MustHave;
               IF Count > Max THEN
                  Result := False;
                  New_Line;Put("To Much Attributes!");
               END IF;
            WHEN 2   =>
               Max := MustHave+2;
               IF Count > Max THEN
                  Result := False;
                  New_Line;Put("To Much Attributes!");
               END IF;
            WHEN 3   =>
               Max := MustHave+3;
               IF Count > Max THEN
                  Result := False;
                  New_Line;Put("To Much Attributes!");
               END IF;
            WHEN OTHERS =>
               Result := False;
               New_Line;Put("Invalid Version!");
         END CASE;

         return Result;
      END ValidateStructure;


      FUNCTION ExecuteValue RETURN Boolean IS   --if return false then escape validation
	 IndexN: Integer;
         First : Integer;
         Last  : Integer;
         ANr   : Integer := 1;       --Attribute Number of Certificate
         AText : Unbounded_String;   --AttributeText
         CText : Unbounded_String;   --CertificateText

         Result : Boolean := False;
	 I	: Integer := 1;

      BEGIN
--	 If Length(CertString ) > 0 then
	If CertTyp = "txt" then
		 SignAlgorithm.Attribute := To_Unbounded_String("");

		 Ada.Strings.Fixed.Find_Token(Source => Ada.Strings.Unbounded.To_String(CertString), Set => Seperators, Test => Ada.Strings.Inside, First => First, Last => Last);
		 IF First = 1 THEN
		    CertString := Delete(CertString,First,Last);   --by find token inside
		 END IF;

		 IndexN := Index(CertString,"<newLine>");   --search for "<newLine>"
		 WHILE IndexN > 0 LOOP
		    CText:= Delete(CertString,IndexN,Length(CertString));
		    CertString := Delete(CertString,1,IndexN+8);
		    AText := Delete(CText ,Length(Attribute(ANr))+1,Length(CText));
		    IF AText = Attribute(ANr) THEN
		       Cert(ANr).Value := Delete(CText,1,Length(Attribute(ANr)));
		       IF Length(Cert(ANr).Value) > 0 THEN
		          Ada.Strings.Fixed.Find_Token(Source => Ada.Strings.Unbounded.To_String(Cert(ANr).Value), Set => Seperators, Test => Ada.Strings.Inside, First => First, Last => Last);
		          IF First = 1 THEN
		             Cert(ANr).Value := Delete(Cert(ANr).Value,First,Last);
		          END IF;
		          Cert(ANr).Attribute:=Attribute(ANr);
		       ELSE
		          Cert(ANr).Value := To_Unbounded_String("");
		          Cert(ANr).Attribute:=Attribute(ANr);
		       END IF;

		       IF ANr < AttributeLength THEN
		          ANr := ANr + 1;
		       END IF;

		    ELSE
		       IF ANr > 1 THEN
		          IF ANr > 7 THEN
		             IndexN := Index(CText,"Signature Algorithm:");
		             IF IndexN > 0 THEN
		                SignAlgorithm.Attribute := Delete(CText,IndexN,IndexN+20);
		             ELSIF SignAlgorithm.Attribute /= "" THEN
		                SignAlgorithm.Value := SignAlgorithm.Value & "" & CText;
		             ELSE
		                Cert(ANr-1).Value := Cert(ANr-1).Value & " " & CText;--Put(Anr,3);
		             END IF;
		          ELSE
		             Cert(ANr-1).Value := Cert(ANr-1).Value & " " & CText;--Put(Anr,3);
		          END IF;
		       END IF;
		       --Put(Anr,3);
		    END IF;

		    Ada.Strings.Fixed.Find_Token(Source => Ada.Strings.Unbounded.To_String(CertString), Set => Seperators, Test => Ada.Strings.Inside, First => First, Last => Last);
		    IF First = 1 THEN
		       CertString := Delete(CertString,First,Last);   --by find token inside
		    END IF;

		    IndexN := Index(CertString,"<newLine>");   --search for "<newLine>"
		    --Count := Count + 1;
		 END LOOP;

		 IF Cert(4).Value = Cert(6).Value THEN
		    RootCertificate := True;
		 ELSE
		    RootCertificate := False;
		 END IF;


		 --validate cert structure
		 Result := ValidateStructure(ANr-1);
	
	 Elsif (CertTyp /= "cer") Then
		Put_Line("No Certificate imported!");		
--		raise  Constraint_Error;
	 Else 		
		WHILE (Length(ASN1.TBS(I).Attribute)) > 0 LOOP			
			I := I + 1;
		END LOOP;
		If I > 6 Then
			Result := True;
		End If;
	 End If;
         Return Result;
      END ExecuteValue;




      FUNCTION Trust RETURN Boolean IS
         Directory: String  := Ada.Directories.Current_Directory;
	   MyDirectory: Unbounded_String := To_Unbounded_String("RootZertifikate/");
         File       : Unbounded_String;
         Subject    : Unbounded_String;   --Subject value of saved confidential certifications
         Issuer     : Unbounded_String;   --Issuer  value of saved confidential certifications
         CertIssuer : Unbounded_String;   --Issuer of certification

         IndexN     : Integer;
         First      : Integer;
         Last       : Integer;
         N : Integer := 1;

         Tree       : Unbounded_String := " -- " & Cert(6).Value;    --value 6 = Subject of certification

         Result: Boolean := False;

      BEGIN
         --New_Line;Put("--Trust--");Put(Directory);New_Line;
	

         IF RootCertificate = False AND Length(Cert(4).Attribute) > 0 THEN      --Attribute 4 is Issuer
            CertIssuer := Cert(4).Value;

            File := Directory & "/" & MyDirectory & To_Unbounded_String(Integer'Image(N)) & ".txt";

            Ada.Strings.Fixed.Find_Token(Source => Ada.Strings.Unbounded.To_String(File), Set => Seperators, Test => Ada.Strings.Inside, First => First, Last => Last);
            File := Delete(File,First,Last);

            WHILE (Ada.Directories.Exists(To_String(File))) AND Result=False LOOP
               --New_Line;Put(To_String(File));
               Get_Cert(To_String(File));
               IndexN := Index(CertString,"Subject: ");
               IF IndexN > 0 THEN
                  Subject:= Delete(CertString,1,IndexN+8);
                  IndexN := Index(Subject,"<newLine>");
                  Subject:= Delete(Subject,IndexN,Length(Subject));
               END IF;
               
               IF Length(Subject) = Length(CertIssuer) THEN

                  IndexN := Index(CertString,"Issuer: ");

                  IF IndexN > 0 THEN
                     Issuer:= Delete(CertString,1,IndexN+7);
                     IndexN := Index(Issuer,"<newLine>");
                     Issuer:= Delete(Issuer,IndexN,Length(Issuer));
                  END IF;

                  IF Subject = CertIssuer THEN
                     --Put("TRUST ME");
                     IF Subject = Issuer THEN
                        --Tree := Subject & Tree;
                        Tree := File & Tree;
                        Result := True;
                     ELSE
                        --Tree := "--" & Subject & Tree;
                        Tree := " -- " & File & Tree;
                        N := 1;
                        CertIssuer := Issuer;
                     END IF;

                  END IF;
               END IF;

               N := N + 1;
               File := MyDirectory & To_Unbounded_String(Integer'Image(N)) & ".txt";
               Ada.Strings.Fixed.Find_Token(Source => Ada.Strings.Unbounded.To_String(File), Set => Seperators, Test => Ada.Strings.Inside, First => First, Last => Last);
               File := Delete(File,First,Last);

            END LOOP;
            New_Line;Put(To_String(Tree));New_Line;
         ELSIF RootCertificate THEN
            New_Line;Put("This is an ROOT CERTIFICATE!");
            Result := false;
         END IF;

         RETURN Result;
      END Trust;


      FUNCTION AnalyseSigned RETURN Unbounded_String Is
	DecryptedHash : Unbounded_String;
	Begin
		DecryptedHash := DecryptSignature;
		Return DecryptedHash;		
	End AnalyseSigned;


      PROCEDURE Validate IS

         Now   : Ada.Calendar.Time := Ada.Calendar.Clock;
         Year  : Ada.Calendar.Year_Number;
         Month : Ada.Calendar.Month_Number;
         Day   : Ada.Calendar.Day_Number;
         Seconds: Ada.Calendar.Day_Duration;

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

         First: Integer;
         Last : Integer;

         TYPE Date IS
         RECORD
            Day  : Integer;
            Month: Integer;
            Year : Integer;
            Time : Unbounded_String;
         END RECORD;

         IndexN : Integer := 0;
         Before : Unbounded_String;
         After  : Unbounded_String;
         Tmp    : Unbounded_String;
         DBefore: Date;
         DAfter : Date;

         Df : Float;
         Mf : Float;
         Yf : Float;

         Today    : Float;
         TmpAfter : Float;
         TmpBefore: Float;
         N : Integer;

         Result : Boolean := true;   --false if illigal date
	 

      BEGIN
         Ada.Calendar.Split(Now,Year,Month,Day,Seconds);
         --Put(Day,2);Put("/");
         --Put(Month,2);Put("/");
         --Put(Year,4);Put("  ");

         IF Cert(5).Attribute = "Validity" THEN   --certificate 5 = Validity
            IndexN := Index(cert(5).Value,"Not Before: ");
            IF IndexN > 0 THEN
               Before := Delete(Cert(5).Value,IndexN,IndexN+11);
               IndexN := 0;
            END IF;
            IndexN := Index(Before,"Not After : ");
            IF IndexN > 0 THEN
               After := Delete(Before,1,IndexN+11);
               Before:= Delete(Before,IndexN,Length(Before));
               IndexN := 0;

               --execute Date from before
               
               Ada.Strings.Fixed.Find_Token(Source => Ada.Strings.Unbounded.To_String(Before), Set => Seperators, Test => Ada.Strings.Inside, First => First, Last => Last);
               IF First = 1 THEN
                  Before := Delete(Before,First,Last);   --by find token inside
               END IF;

               N := 0;

               FOR I IN 1.. 12 LOOP
                  N := I;
                  Tmp := Delete(Before,4,Length(Before));
                  EXIT WHEN Tmp = Month_T(I);
               END LOOP;

               DBefore.Month := N; --Put(N,2);
               Before := Delete(Before,1,4);
               Ada.Strings.Fixed.Find_Token(Source => Ada.Strings.Unbounded.To_String(Before), Set => Seperators, Test => Ada.Strings.Inside, First => First, Last => Last);
               IF First = 1 THEN
                  Before := Delete(Before,First,Last);   --by find token inside
               END IF;

               Ada.Strings.Fixed.Find_Token(Source => Ada.Strings.Unbounded.To_String(Before), Set => Seperators, Test => Ada.Strings.Inside, First => First, Last => Last);
               DBefore.Day := Integer'VALUE(To_String(Delete(Before,First,Length(Before))));	--StrToInt(Delete(Before,First,Length(Before)));   --by find token inside

               Before := Delete(Before,1,First);

               Ada.Strings.Fixed.Find_Token(Source => Ada.Strings.Unbounded.To_String(Before), Set => Seperators, Test => Ada.Strings.Inside, First => First, Last => Last);

               DBefore.Time := Delete(Before,First,Length(Before));   --by find token inside


               Before := Delete(Before,1,First);

               Ada.Strings.Fixed.Find_Token(Source => Ada.Strings.Unbounded.To_String(Before), Set => Seperators, Test => Ada.Strings.Inside, First => First, Last => Last);
               DBefore.Year := Integer'VALUE(To_String(Delete(Before,First,Length(Before))));		--StrToInt(Delete(Before,First,Length(Before)));   --by find token inside
               Before := Delete(Before,1,First);

               --execute Date from after
               --New_Line;Put(Ada.Strings.Unbounded.To_String(After));

               Ada.Strings.Fixed.Find_Token(Source => Ada.Strings.Unbounded.To_String(After), Set => Seperators, Test => Ada.Strings.Inside, First => First, Last => Last);
               IF First = 1 THEN
                  After:= Delete(After,First,Last);   --by find token inside
               END IF;

               N := 0;

               FOR I IN 1.. 12 LOOP
                  N := I;
                  Tmp := Delete(After,4,Length(After));
                  EXIT WHEN Tmp = Month_T(I);
               END LOOP;

               DAfter.Month := N;
               After := Delete(After,1,4);
               Ada.Strings.Fixed.Find_Token(Source => Ada.Strings.Unbounded.To_String(After), Set => Seperators, Test => Ada.Strings.Inside, First => First, Last => Last);
               IF First = 1 THEN
                  After := Delete(After,First,Last);   --by find token inside
               END IF;

               Ada.Strings.Fixed.Find_Token(Source => Ada.Strings.Unbounded.To_String(After), Set => Seperators, Test => Ada.Strings.Inside, First => First, Last => Last);
               DAfter.Day := Integer'VALUE(To_String(Delete(After,First,Length(After))));		--StrToInt(Delete(After,First,Length(After)));   --by find token inside
               After := Delete(After,1,First);

               Ada.Strings.Fixed.Find_Token(Source => Ada.Strings.Unbounded.To_String(After), Set => Seperators, Test => Ada.Strings.Inside, First => First, Last => Last);

               DAfter.Time := Delete(After,First,Length(After));   --by find token inside


               After := Delete(After,1,First);

               Ada.Strings.Fixed.Find_Token(Source => Ada.Strings.Unbounded.To_String(After), Set => Seperators, Test => Ada.Strings.Inside, First => First, Last => Last);
               DAfter.Year := Integer'VALUE(To_String(Delete(After,First,Length(After))));		--StrToInt(Delete(After,First,Length(After)));   --by find token inside
               After := Delete(After,1,First);

               Df := Float(Day);
               Mf := Float(Month);
               Yf := Float(Year);
               Today     := Df/10000.0 + Mf/100.0 + Yf;

               Df := Float(DAfter.Day);
               Mf := Float(DAfter.Month);
               Yf := Float(DAfter.Year);

               TmpAfter  := Df/10000.0 + Mf/100.0 + Yf;

               Df := Float(DBefore.Day);
               Mf := Float(DBefore.Month);
               Yf := Float(DBefore.Year);

               TmpBefore := Df/10000.0 + Mf/100.0 + Yf;
               IF Length(DBefore.Time) /= 8 AND Length(DAfter.Time) /= 8 THEN
                  New_Line; Put("- invalid time information -");DAfter.Time := To_Unbounded_String("00:00:00");
                  DBefore.Time := To_Unbounded_String("00:00:00");
               END IF;

               New_Line;
               IF TmpAfter < Today THEN
                  Result := False;
                  New_Line; Put("- certificate out of date since ");Put(DAfter.Day,2);Put(".");Put(DAfter.Month,2);Put(".");Put(DAfter.Year,4);Put(" !!! -");New_Line;
               END IF;

               IF TmpAfter = TmpBefore AND Result THEN
                  IF DAfter.Time <= DBefore.Time THEN
                     Result := False;
                  END IF;
               ELSE
                  IF TmpAfter < TmpBefore AND Result THEN
                     Result := False;
                     New_Line; Put("- invalid certificate: Validity illegal -");
                  END IF;
               END IF;

            END IF;
            --New_Line;Put(Ada.Strings.Unbounded.To_String(DBefore.Time));
            --Put(DBefore.Day,2);Put(DBefore.Month,2); Put(DBefore.Year,5); --Put(Ada.Strings.Unbounded.To_String(TimeC));
            --Put(Ada.Strings.Unbounded.To_String(After));
         END IF;

         IF Result THEN
            New_Line; Put("- certificate is up to date -");
            IF Trust THEN
               New_Line; Put("this certifikate can be trust");
            ELSE
               New_Line;Put("This certifikate CAN NOT be trust!");
            END IF;
	    
	    If AnalyseSigned /= Null_Unbounded_String Then
		Put_Line("Decrpyted Well");				--TODO Hash of TBS and Compare Here
	    End If;
         END IF;

      END Validate;


	FUNCTION DecryptSignature Return Unbounded_String IS 
		package WIO is new Ada.Text_IO.Modular_IO(Crypto.Types.Word);

		M     : RSA.Big.Big_Unsigned;
		E     : RSA.Big.Big_Unsigned;
		CT    : RSA.Big.Big_Unsigned;
		PlainT: RSA.Big.Big_Unsigned;
		
		Tmp   : Unbounded_String := Null_Unbounded_String;
		PText : Unbounded_String := Null_Unbounded_String;
--		HText : Unbounded_String := Null_Unbounded_String;
		IndexN: Integer;
--		Result : Crypto.Types.W_Block160;

		Tag_I1: Unbounded_String := To_Unbounded_String("55040756616C69436572742056616C69646174696F6E204E6574776F726B");		
		Tag_I2: Unbounded_String := To_Unbounded_String("55040A56616C69436572742C20496E632E");		
		Tag_I3: Unbounded_String := To_Unbounded_String("55040B56616C694365727420436C617373203220506F6C6963792056616C69646174696F6E20417574686F72697479");		
		Tag_I4: Unbounded_String := To_Unbounded_String("550403687474703A2F2F7777772E76616C69636572742E636F6D2F");		
		Tag_I5: Unbounded_String := To_Unbounded_String("2A864886F70D010901696E666F4076616C69636572742E636F6D");		
		
		Tag_PKID  : Unbounded_String := To_Unbounded_String("2A864886F70D010101");
		Tag_PKey  : Unbounded_String := To_Unbounded_String("0030818902818100CE3A71CAE5ABC8599255D7"
        &"ABD8740EF9EED9F655475965470E0555DCEB98363C5C535DD330CF38ECBD4189ED254209246B0A5EB37CDD522"
        &"D4CE6D4D67D5A59A965D449132D244D1C506FB5C185543BFE71E4D35C42F980E0911A0A5B393667F33F557C1B"
        &"3FB45F647334E3B412BF8764F8DA12FF3727C1B343BBEF7B6E2E69F70203010001");


		Tag_V : Unbounded_String := To_Unbounded_String("01");			--nicht angegeben in 13
		Tag_SN: Unbounded_String := To_Unbounded_String("01");
		Tag_SAlgo: Unbounded_String := To_Unbounded_String("06092A864886F70D0101050500");
		Tag_Issuer: Unbounded_String := To_Unbounded_String("3081BB312430220603550407131B56616C6943"
        &"6572742056616C69646174696F6E204E6574776F726B31173015060355040A130E56616C69436572742C20496"
        &"E632E31353033060355040B132C56616C694365727420436C617373203220506F6C6963792056616C69646174"
        &"696F6E20417574686F726974793121301F06035504031318687474703A2F2F7777772E76616C69636572742E6"
        &"36F6D2F3120301E06092A864886F70D0109011611696E666F4076616C69636572742E636F6D");
		Tag_Vallid: Unbounded_String := To_Unbounded_String("3939303632363030313935345A3139303632363030313935345A");
		Tag_Subj  : Unbounded_String := Tag_Issuer;

		Tag_PK    : Unbounded_String := To_Unbounded_String("300D06092A864886F70D010101050003818D00"
        &"30818902818100CE3A71CAE5ABC8599255D7ABD8740EF9EED9F655475965470E0555DCEB98363C5C535DD330C"
        &"F38ECBD4189ED254209246B0A5EB37CDD522D4CE6D4D67D5A59A965D449132D244D1C506FB5C185543BFE71E4"
        &"D35C42F980E0911A0A5B393667F33F557C1B3FB45F647334E3B412BF8764F8DA12FF3727C1B343BBEF7B6E2E69F70203010001");
		
		
--		RS : String (1..40);
		S13: String := ("30820250020101300D06092A864886F70D01010505003081BB312430220603550407131B56"
        &"616C69436572742056616C69646174696F6E204E6574776F726B31173015060355040A130E56616C694365727"
        &"42C20496E632E31353033060355040B132C56616C694365727420436C617373203220506F6C6963792056616C"
        &"69646174696F6E20417574686F726974793121301F06035504031318687474703A2F2F7777772E76616C69636"
        &"572742E636F6D2F3120301E06092A864886F70D0109011611696E666F4076616C69636572742E636F6D301E17"
        &"0D3939303632363030313935345A170D3139303632363030313935345A3081BB312430220603550407131B566"
        &"16C69436572742056616C69646174696F6E204E6574776F726B31173015060355040A130E56616C6943657274"
        &"2C20496E632E31353033060355040B132C56616C694365727420436C617373203220506F6C6963792056616C6"
        &"9646174696F6E20417574686F726974793121301F06035504031318687474703A2F2F7777772E76616C696365"
        &"72742E636F6D2F3120301E06092A864886F70D0109011611696E666F4076616C69636572742E636F6D30819F3"
        &"00D06092A864886F70D010101050003818D0030818902818100CE3A71CAE5ABC8599255D7ABD8740EF9EED9F6"
        &"55475965470E0555DCEB98363C5C535DD330CF38ECBD4189ED254209246B0A5EB37CDD522D4CE6D4D67D5A59A"
        &"965D449132D244D1C506FB5C185543BFE71E4D35C42F980E0911A0A5B393667F33F557C1B3FB45F647334E3B412BF8764F8DA12FF3727C1B343BBEF7B6E2E69F70203010001");	
		
	BEGIN
		
		Tmp := To_Unbounded_String("16#") & SignAlgorithm.Value & To_Unbounded_String("#");
		CT := RSA.Big.Utils.To_Big_Unsigned(To_String(Tmp));
		Tmp := To_Unbounded_String("16#") & ASN1.Public_Key.Value & To_Unbounded_String("#");
		M := RSA.Big.Utils.To_Big_Unsigned(To_String(Tmp));	
		E := RSA.Big.Utils.To_Big_Unsigned("16#10001#");
		PlainT := RSA.Big.Mod_Utils.Pow(CT,E,M);
		PText := To_Unbounded_String(RSA.Big.Utils.To_String(PlainT,16));		
		IndexN := Index(PText,"3021300906052B0E03021A05000414");	--AlgorithmIdentifier for sha1
		IF IndexN > 0 Then
			PText := Delete(PText,1, IndexN+29);			
			PText := Delete(PText,Length(PText), Length(PText));	--last # delete
			If Length(PText) /= 40 Then
				PText := Null_Unbounded_String;										
			End If;
		End If;
--		Put_Line(To_String(PText));
		Return PText;
	END DecryptSignature;



   PROCEDURE Get_Cert(Filename: String) IS
      file     : File_Type;
      InLine   : String(1..1024); 	--String big enough to read
      LengthL  : Natural;		--Length of String Line
      Token    : Unbounded_String;
--	ASN1 : Array(1..9) Of Unbounded_String := (
--		1=>"A0030201",			--X509 Version Tag
--	      	2=>"301E170D"			--Validity Tag
--      		);

      BEGIN

--	Put_Line(Filename'Last)
        Init;

         IF Filename(Filename'Last-2 .. Filename'Last) = "cer" THEN
            DECLARE
               ASN1Data : Unbounded_String := To_Unbounded_String("");

            BEGIN
               Put_Line("-- " & Filename);
               Open(File => file, Mode => In_File,Name => Filename);
               WHILE NOT End_Of_File(File) LOOP		  
                  Get_Line(File => file, Item => InLine, Last => LengthL);
                  FOR I IN 1..LengthL LOOP
                     Token := To_Unbounded_String(Character'Pos(InLine(I))'Img);
                     Token := To_Hex_Token(Token);
                     ASN1Data := ASN1Data & Token;
                  END LOOP;
		  Token := To_Hex_Token(To_Unbounded_String("10"));
                  ASN1Data := ASN1Data & Token;		 	
          		
	       END LOOP;
	       Close(File);
	       ASN1Data := Delete(ASN1Data, Length(ASN1Data)-1, Length(ASN1Data));
               CertTyp := "cer";
               ASN1.Parse(ASN1Data);
            END;
          ELSIF Filename(Filename'Last-2 .. Filename'Last) = "txt" THEN

		Put_Line("-- " & Filename);
		Open(File => file, Mode => In_File,Name => Filename);
		WHILE NOT End_Of_File(file) LOOP
		    	Get_Line(File => file, Item => InLine, Last => LengthL);
			CertString := CertString & InLine(1..LengthL) & "<newLine>" ;      --extract cert in CertString
		END LOOP;
		CertTyp := "txt";
	Else		
		raise  Cert_Typ_Error;
      END IF;

      IF ExecuteValue THEN Validate;
      ELSE
--            Put("Escape of validation: invalid structure.");
            raise Cert_Structure_Error;
      END IF;


      EXCEPTION
         WHEN Name_Error =>
            Put_Line(File => Standard_Error, Item => "File name not found.");
            FileExist := false;
         WHEN Status_Error =>
            Put_Line(File => Standard_Error, Item => "File already open.");
         WHEN Use_Error =>
            Put_Line(File => Standard_Error, Item => "You do not have permission to open the file.");
         WHEN Cert_Typ_Error =>
            Put_Line(File => Standard_Error, Item => "*." & Filename(Filename'Last-2 .. Filename'Last) & " " & "Filetype not supported!");
         WHEN Cert_Structure_Error =>
            Put_Line(File => Standard_Error, Item => "Escape of validation: invalid structure.");
      END Get_Cert;


	

END Crypto.Certificate;
