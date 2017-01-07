------------------------------------------------------------------------------
--                               N L C O L L
--  N a t u r a l   L a n g u a g e   C o m p o n e n t   C o l l e c t i o n
--
--          Copyright 2009-2011 M. Grella, M. Nicola, D. Christen
--
--  In collaboration with Politecnico di Torino and Università di Torino.
--
--  Supported by
--   (a) Fondazione CRT under a "VivoMeglio" grant (Speak2Home)
--   (b) Region Piedmont under a “Converging Technologies” programme (ATLAS)
--        
--  The project aims at developing open-source linguistic technologies for the
--  Italian language to improve the welfare of people, especially impaired users.
--
--  This is free software; you can redistribute it and/or modify it under
--  terms of the GNU General Public License as published by the Free Software
--  Foundation; either version 2, or (at your option) any later version.
--  This software is distributed in the hope that it will be useful, but WITH
--  OUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
--  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
--  for more details. Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.
--
------------------------------------------------------------------------------

pragma License (Modified_GPL);

with ARColl.Progress;
with Ada.Containers; use Ada.Containers;
with NLColl.Linguistic_Description.Surface; use NLColl.Linguistic_Description.Surface;
with ARColl.Strings;
with ARColl.Strings.Unbounded;
use ARColl.Strings.Unbounded;

separate (NLColl.Corpus.Dictionary_Of_Forms)

package body From_JSON is

    procedure Collect_Morphology
      (Forms_Dictionary  : out Forms_Dictionary_Type;
       Form_List         : in  String_Vectors.Vector;
       Morphology        : in  Morphological_Features_Vectors.Vector) is
        
        use Text_IO;

        Dict_Key         : Unbounded_String;
        Dict_Key_Cursor  : Dict_Maps.Cursor;
        
        Form_IDs         : Form_ID_Array 
          (Index_Type'First .. Index_Type'First + Index_Type(Form_List.Length) - 1)  := (others => -1);

        Analysis_IDs     : Analysis_Array
          (Index_Type'First .. Index_Type'First + Index_Type(Morphology.Length) - 1) := (others => (-1,-1));

    begin

        -- Key: Forms Lowercase and without Accents
        
        for Form of Form_List loop
            if Length (Dict_Key) > 0 then
                Append (Dict_Key, " ");
            end if;
            
            Append (Dict_Key, (String_To_Lower (Remove_Accents (Str => Form, Skip_Apostrophe => True))));
        end loop;

        Dict_Key_Cursor := Forms_Dictionary.Dict.Find (To_String (Dict_Key));
        
        -- Form
        
        declare
            I : Index_Type := Index_Type'First;
        begin
            for Form of Form_List loop
                declare
                    Form_ID : Extended_ID 
                      := Forms_Dictionary.Forms.Find_ID (Form);
                begin
                    if Form_ID = -1 then
                        Forms_Dictionary.Forms.Insert (Form);
                        Form_ID := Forms_Dictionary.Forms.Last_ID;
                    end if;

                    Form_IDs(I) := Form_ID;
                end;
                I := I + 1;
            end loop;
        end;
        
        -- Morphological Analysis
        declare
            I : Index_Type := Index_Type'First;
        begin
            for Morpho_Item of Morphology loop
                declare
                    Morpho_Pattern : constant Morpho_Pattern_Type
                      := (PosTag       => Morpho_Item.PosTag,
                          Mood         => Morpho_Item.Mood,
                          Tense        => Morpho_Item.Tense,
                          Person       => Morpho_Item.Person,
                          Number       => Morpho_Item.Number,
                          Gender       => Morpho_Item.Gender,
                          GCase        => Morpho_Item.GCase,
                          Degree       => Morpho_Item.Degree,
                          Person_Poss  => NONE,
                          Number_Poss  => NONE);

                    Pattern_ID  : Extended_ID := Forms_Dictionary.Morpho_Patterns.Find_ID (Morpho_Pattern);
                
                    Lemma       : constant String := To_String (Morpho_Item.Lemma);
                    Lemma_ID    : Extended_ID     := Forms_Dictionary.Lemma.Find_ID (Lemma);                
                begin
                    if Lemma_ID = -1 then
                        Forms_Dictionary.Lemma.Insert (Lemma);
                        Lemma_ID := Forms_Dictionary.Lemma.Last_ID;
                    end if;
                
                    if Pattern_ID = -1 then
                        Forms_Dictionary.Morpho_Patterns.Insert (Morpho_Pattern);
                        Pattern_ID := Forms_Dictionary.Morpho_Patterns.Last_ID;
                    end if;
                
                    Analysis_IDs (I).Lemma_ID   := Lemma_ID;
                    Analysis_IDs (I).Pattern_ID := Pattern_ID;
                end;
                
                I := I + 1;
            end loop;
        end;
        
        if not Dict_Maps.Has_Element (Dict_Key_Cursor) then
            Forms_Dictionary.Dict.Insert (To_String (Dict_Key), Dictionary_Forms_Vectors.Empty_Vector);
        end if;
        
        declare
            Morpho_Data     : Morpho_Data_Type 
              (Last_Analysis => Analysis_IDs'Last,
               Last_Form     => Form_IDs'Last);
        begin
            Morpho_Data.Analysis := Analysis_IDs;
            Morpho_Data.Form_IDs := Form_IDs;
                
            Forms_Dictionary.Dict.Reference (To_String (Dict_Key)).Append (Morpho_Data);
        end;
        
    end Collect_Morphology;
    
    procedure Load_From_JSONL_Files
      (Forms_Dictionary  : out Forms_Dictionary_Type;
       Containing_Folder : String) is

        JSONL_Files : constant String_Vectors.Vector
          := Get_All_JSONL_Files (Containing_Folder);

    begin

        Text_IO.Put_Line ("-- Reading forms dictionary JSONL files");

        Forms_Dictionary := Empty_Forms_Dictionary;

        for Filename of JSONL_Files loop
            Load_JSONL_File (Forms_Dictionary, Filename);
        end loop;

    end Load_From_JSONL_Files;

    function Get_All_JSONL_Files 
      (Containing_Folder : String) return String_Vectors.Vector is

        use Ada.Directories;

        function Is_Hidden (Directory_Entry : Directory_Entry_Type) return Boolean with Inline is
            Name : constant String := Simple_Name (Directory_Entry);
        begin
            return (Name'Length > 0 and then Name (Name'First) = '.');
        end Is_Hidden;

        Filenames : String_Vectors.Vector;

        procedure Collect (Directory_Entry : Directory_Entry_Type) is
        begin
            if not is_Hidden (Directory_Entry) then
                Filenames.Append (Full_Name (Directory_Entry));
            end if;
        end Collect;

    begin

        Search
          (Directory => Containing_Folder,
           Pattern   => "",
           Filter    => (Directory => False, Ordinary_File => True, Special_File => False),
           Process   => Collect'Access);

        Alphabetical_Sorting.Sort (Filenames);

        return Filenames;

    end Get_All_JSONL_Files;

    procedure Load_JSONL_File
      (Forms_Dictionary : in out Forms_Dictionary_Type;
       Filename         : String) is
        pragma Unreferenced (Forms_Dictionary);

        use Text_IO;
        use Ada.Strings;
        use Ada.Strings.Fixed;

        File        : File_Type;
        Line_Number : Natural := 0;
        Progress    : ARColl.Progress.Progress_Indicator;
        
    begin

        Progress := ARColl.Progress.Create_Indicator 
          (Total => Natural(Ada.Directories.Size(Filename)),
           Mode  => ARColl.Progress.BAR);
        
        Put_Line ("  -- Reding file " & Filename);

        Open (File, In_File, Filename);

        while not End_Of_File (File) loop            
            declare
                Line  : constant String := Get_Line (File);
                TLine : constant String := Trim (Line, Both);
            begin
                Line_Number := Line_Number + 1;
                
                Progress.Tick (Amount => Line'Length + 1);
                
                if TLine'Length > 0 then
                    declare
                        Form_List  : String_Vectors.Vector;
                        Morphology : Morphological_Features_Vectors.Vector;
                    begin
                        Load_JSON
                          (JSON_Text  => TLine,
                           Form_List  => Form_List,
                           Morphology => Morphology);
                        
                        Collect_Morphology
                          (Forms_Dictionary => Forms_Dictionary,
                           Form_List        => Form_List,
                           Morphology       => Morphology);
                    end;
                end if;

            exception
                when Error : Invalid_Or_Malformed_JSON =>
                    Put_Line
                      (Standard_Error,
                       ASCII.LF & "  ERROR: invalid or malformed JSON in file " &
                         Filename & " at line" & Line_Number'Img & ASCII.LF &
                         "    Message => " & Ada.Exceptions.Exception_Message (Error) & ASCII.LF &
                         "    Line => " & TLine);
                    raise;
            end;
        end loop;
        
        Close (File);

    end Load_JSONL_File;

    procedure Load_JSON
      (JSON_Text  : in  String;
       Form_List  : out String_Vectors.Vector;
       Morphology : out Morphological_Features_Vectors.Vector) is

        use ARColl.Readers.JSON;
        Fatal_Error : exception;

        function String_Value_Exists
          (Obj : not null JSON_Element_Class_Access; Key : String) return Boolean is
          (Map (Obj).Contains (Key) and then Element (Obj, Key).all in JSON_String_Type)
            with Inline;

        JSON_Data   : JSON_Element_Class_Access;
        JSON_Form   : JSON_Element_Class_Access;
        JSON_Morpho : JSON_Element_Class_Access;
    begin

        JSON_Data := JSON_Decode (JSON_Text);
        Validation.Validate (JSON_Data);

        JSON_Form := Element (JSON_Data, "form");
        JSON_Morpho := Element (JSON_Data, "morpho");

        -----
        -- Form
        -----
          
        if JSON_Form.all in JSON_String_Type then
            -- handle here the single Form
            
            declare
                Form : constant String := Value (JSON_Form);
            begin
                Form_List.Append (Form);
            end;

        elsif JSON_Form.all in JSON_Array_Type then
            -- handle here the multiple Form_List
            
            for JItem of Vector (JSON_Form) loop
                Form_List.Append (Value (JItem));
            end loop;
            
        else
            raise Fatal_Error with "Unexpected JSON_Form class type.";
        end if;

        -----
        -- Morho
        -----

        for JMorpho_Item of Vector (JSON_Morpho) loop

            declare
                Morpho_Item : Morphological_Features_Type;
            begin
                
                -- Mandatory fields

                Morpho_Item.Lemma   := Value (Element (JMorpho_Item, "lemma"));
                Morpho_Item.PosTag  := Get_POS (Value (Element (JMorpho_Item, "pos")));

                -- Optional fields

                if String_Value_Exists (JMorpho_Item, "mood") then
                    Morpho_Item.Mood := Get_Mood (Value (Element (JMorpho_Item, "mood")));
                end if;
                
                if String_Value_Exists (JMorpho_Item, "tense") then
                    Morpho_Item.Tense := Get_Tense  (Value (Element (JMorpho_Item, "tense")));
                end if;
                
                if String_Value_Exists (JMorpho_Item, "person") then
                    Morpho_Item.Person := Get_Person  (Value (Element (JMorpho_Item, "person")));
                end if;
                
                if String_Value_Exists (JMorpho_Item, "number") then
                    Morpho_Item.Number := Get_Number  (Value (Element (JMorpho_Item, "number")));
                end if;
                
                if String_Value_Exists (JMorpho_Item, "gender") then
                    Morpho_Item.Gender := Get_Gender  (Value (Element (JMorpho_Item, "gender")));
                end if;
                
                if String_Value_Exists (JMorpho_Item, "case") then
                    Morpho_Item.GCase := Get_GCase  (Value (Element (JMorpho_Item, "case")));
                end if;
                
                if String_Value_Exists (JMorpho_Item, "degree") then
                    Morpho_Item.Degree := Get_Degree  (Value (Element (JMorpho_Item, "degree")));
                end if;
        
                -- Add to Morphology
                
                Morphology.Append(Morpho_Item);
            end;
        end loop;

        -----

        Destroy (JSON_Data);

    exception
        when Error : JSON_Error | Invalid_Or_Malformed_JSON | Invalid_Value =>
            Destroy (JSON_Data);
            raise Invalid_Or_Malformed_JSON with Ada.Exceptions.Exception_Message (Error);
        when others =>
            Destroy (JSON_Data);
            raise;
    end Load_JSON;

    package body Validation is

        procedure Validate (JSON_Data : not null JSON_Element_Class_Access) is
        begin

            JSON_Assert
              (JSON_Data.all in JSON_Object_Type, "Main data structure is not an object.");

            JSON_Assert (Map (JSON_Data).Contains ("form"), "Missing ""form"" key.");
            JSON_Assert (Map (JSON_Data).Contains ("morpho"), "Missing ""morpho"" key.");

            for Position in Map (JSON_Data).Iterate loop
                declare
                    use Element_Hashed_Maps;
                    Obj_Key   : constant String := Key (Position);
                    Obj_Value : constant JSON_Element_Class_Access := Element (Position);
                begin
                    if Obj_Key = "form" then
                        Validate_Form (Obj_Value);
                    elsif Obj_Key = "morpho" then
                        Validate_Morpho (Obj_Value);
                    else
                        raise Invalid_Or_Malformed_JSON with "Invalid key """ & Obj_Key & """.";
                    end if;
                end;
            end loop;

        end Validate;

        procedure Validate_Form (JSON_Form : not null JSON_Element_Class_Access) is
        begin

            if JSON_Form.all in JSON_String_Type then
                JSON_Assert (String'(Value (JSON_Form))'Length > 0, "Form string is empty.");

            elsif JSON_Form.all in JSON_Array_Type then
                JSON_Assert (not Is_Empty (JSON_Form), "Form array is empty.");

                for JItem of Vector (JSON_Form) loop
                    JSON_Assert
                      (JItem.all in JSON_String_Type, "Form array item is not string.");

                    JSON_Assert
                      (String'(Value (JItem))'Length > 0, "Form array string item is empty.");
                end loop;

            else
                JSON_Assert (Length (JSON_Form) > 0, "Invalid form data structure type.");
            end if;

        end Validate_Form;

        procedure Validate_Morpho (JSON_Morpho : not null JSON_Element_Class_Access) is

            procedure Assert_Morpho_Key (Obj : not null JSON_Element_Class_Access; Key : String)
              with Inline is
            begin
                JSON_Assert
                  (Map (Obj).Contains (Key), "Missing """ & Key & """ key in morpho object.");
            end Assert_Morpho_Key;

        begin

            JSON_Assert
              (JSON_Morpho.all in JSON_Array_Type, "Morpho data structure is not an array.");

            JSON_Assert (not Is_Empty (JSON_Morpho), "Morpho array is empty.");

            for JMorpho_Item of Vector (JSON_Morpho) loop
                JSON_Assert
                  (JMorpho_Item.all in JSON_Object_Type, "Morpho array item is not an object.");

                Assert_Morpho_Key (JMorpho_Item, "lemma");
                Assert_Morpho_Key (JMorpho_Item, "pos");

                for Position in Map (JMorpho_Item).Iterate loop
                    declare
                        use Element_Hashed_Maps;
                        Obj_Key   : constant String := Key (Position);
                        Obj_Value : constant JSON_Element_Class_Access := Element (Position);
                    begin
                        if Obj_Key in "lemma" | "pos" then
                            JSON_Assert
                              (Obj_Value.all in JSON_String_Type,
                               "Value of """ & Obj_Key & """ is not a string.");

                            JSON_Assert
                              (String'(Value (Obj_Value))'Length > 0,
                               "Value of """ & Obj_Key & """ is an empty string.");

                            if Obj_Key = "pos" then
                                declare
                                    Obj_Value_Str : constant String := String'(Value (Obj_Value));
                                    POS           : POS_Type_Array;
                                    pragma Unreferenced (POS);
                                begin
                                    POS := Get_POS (Obj_Value_Str);
                                exception
                                    when Wrong_POS =>  
                                        raise Invalid_Value with
                                          "Invalid Value """ & Obj_Value_Str & """ for """ & Obj_Key & """.";
                                end;
                            end if;
                            
                        elsif Obj_Key in "mood" | "tense" | "person"
                          | "number" | "gender" | "case" | "degree" then

                            if Obj_Value.all in JSON_String_Type then
                                declare
                                    Obj_Value_Str : constant String := String'(Value (Obj_Value));
                                begin
                                    JSON_Assert
                                      (Obj_Value_Str'Length > 0,
                                    "Value of """ & Obj_Key & """ is an empty string.");

                                    declare
                                        Mood   : Mood_Type;
                                        Tense  : Tense_Type;
                                        Gender : Gender_Type;
                                        Person : Person_Type;
                                        Number : Number_Type;
                                        Degree : Degree_Type;
                                        GCase  : GCase_Type_Array;

                                        pragma Unreferenced 
                                          (Mood, Tense, Gender, 
                                           Person, Number, Degree, GCase);
                                    begin
                                        if Obj_Key = "mood" then
                                            Mood   := Get_Mood   (Obj_Value_Str);
                                        elsif Obj_Key = "tense" then
                                            Tense  := Get_Tense  (Obj_Value_Str);
                                        elsif Obj_Key = "gender" then
                                            Gender := Get_Gender (Obj_Value_Str);
                                        elsif Obj_Key = "person" then
                                            Person := Get_Person (Obj_Value_Str);
                                        elsif Obj_Key = "number" then
                                            Number := Get_Number (Obj_Value_Str);
                                        elsif Obj_Key = "degree" then
                                            Degree := Get_Degree (Obj_Value_Str);
                                        elsif Obj_Key = "case" then
                                            GCase  := Get_GCase (Obj_Value_Str);
                                        end if;                                            
                                    exception
                                        when Wrong_Mood | Wrong_Tense | Wrong_Gender |
                                             Wrong_Person | Wrong_Number | Wrong_Degree |
                                             Wrong_GCase =>
                                                
                                            raise Invalid_Value with
                                              "Invalid Value """ & Obj_Value_Str & """ for """ & Obj_Key & """.";
                                    end;
                                end;
                                
                            elsif Obj_Value.all not in JSON_Null_Type then
                                raise Invalid_Or_Malformed_JSON with
                                  "Invalid data structure type for value of """ & Obj_Key & """.";
                            end if;

                        else

                            raise Invalid_Or_Malformed_JSON with "Invalid key """ & Obj_Key & """.";
                        end if;
                    end;
                end loop;

            end loop;

        end Validate_Morpho;

        procedure JSON_Assert 
          (Check : Boolean; Message : String) is
        begin
            if not Check then
                raise Invalid_Or_Malformed_JSON with Message;
            end if;
        end JSON_Assert;

    end Validation;

end From_JSON;

