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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers;

package body NLColl.Linguistic_Description.Morphology is

    procedure Print
      (Features : in Morphological_Features_Type) is
    begin

        Put_Line ("    Lemma : " & To_String (Features.Lemma));

        Put ("    PosTag : ");
        for Item of Features.PosTag loop
            Put (Item'Img & "-");
        end loop;
        New_Line;

        Put_Line ("    Mood :   " & Features.Mood'Img);
        Put_Line ("    Tense :  " & Features.Tense'Img);
        Put_Line ("    Person : " & Features.Person'Img);
        Put_Line ("    Number : " & Features.Number'Img);
        Put_Line ("    Gender : " & Features.Gender'Img);

        Put ("    GCase : ");
        for Item of Features.GCase loop
            Put (Item'Img & "-");
        end loop;
        New_Line;

        Put_Line ("    Degree :      " & Features.Degree'Img);
        Put_Line ("    Person_Poss : " & Features.Person_Poss'Img);
        Put_Line ("    Number_Poss : " & Features.Number_Poss'Img);
        Put_Line ("    Voice :       " & Features.Voice'Img);

        Put_Line ("    MWE_Range : ");
        Put_Line ("    FIRST :      " & Features.MWE_Range (FIRST)'Img);
        Put_Line ("     LAST :      " & Features.MWE_Range (LAST)'Img);

    end Print;


    ------------------------
    --- Grammatical Features
    ------------------------

    function Get_Tense
      (Tense : String) return Tense_Type is -- TODO: Why don't just do a Tense_Type'Value(Tense)?
    begin
        if    Tense = "BASE"      then return BASE;
        elsif Tense = "PRESENT"   then return PRESENT;
        elsif Tense = "IMPERFECT" then return IMPERFECT;
        elsif Tense = "PAST"      then return PAST;
        elsif Tense = "FUTURE"    then return FUTURE;
        else raise Wrong_Tense with "Tense: " & Tense;
        end if;
    end Get_Tense;

    function Get_Degree
      (Degree : String) return Degree_Type is
        pragma Unreferenced (Degree);
    begin
        return NONE;
    end Get_Degree;

    function Get_Person
      (Person : String) return Person_Type is
    begin
        if    Person in "1" | "FIRST"  then return FIRST;
        elsif Person in "2" | "SECOND" then return SECOND;
        elsif Person in "3" | "THIRD"  then return THIRD;
        elsif Person = "ALLVAL"        then return ALLVAL;
        else raise Wrong_Person with "Person: " & Person;
        end if;
    end Get_Person;

    function Get_Gender
      (Gender : String) return Gender_Type is
    begin
        if    Gender in "M" | "MASC"      then return MASC;
        elsif Gender in "F" | "FEM"       then return FEM;
        elsif Gender in "BOTH" | "ALLVAL" then return BOTH;
        elsif Gender = "NEUT"             then return NEUT;
        elsif Gender = "NONE"             then return NONE;
        else raise Wrong_Gender with "Gender: " & Gender;
        end if;
    end Get_Gender;

    function Get_Number
      (Number : String) return Number_Type is
    begin
        if    Number = "SING"             then return SING;
        elsif Number in "PL" | "PLUR"     then return PLUR;
        elsif Number in "BOTH" | "ALLVAL" then return BOTH;
        elsif Number = "DUAL"             then return DUAL;
        elsif Number = "NONE"             then return NONE;
        else raise Wrong_Number with "Number: " & Number;
        end if;
    end Get_Number;

    function Get_Mood
      (Mood : String) return Mood_Type is -- TODO: if not retro compatibility, why just don't do a Mood_Type'Value (Mood)?
    begin
        if    Mood = "BASE"                     then return BASE;
        elsif Mood in "IND" | "INDICATIVE"      then return INDICATIVE;  -- TODO: back compatibility?
        elsif Mood in "CONDIZ" | "CONDITIONAL"  then return CONDITIONAL; -- TODO: back compatibility?
        elsif Mood in "CONG" | "SUBJUNCTIVE"    then return SUBJUNCTIVE; -- TODO: back compatibility?
        elsif Mood = "PARTICIPLE"               then return PARTICIPLE;
        elsif Mood = "GERUND"                   then return GERUND;
        elsif Mood in "INFINITE" | "INFINITIVE" then return INFINITIVE; -- TODO: back compatibility?
        elsif Mood in "IMPER" | "IMPERATIVE"    then return IMPERATIVE; -- TODO: back compatibility?
        else raise Wrong_Mood with "Mood: " & Mood;
        end if;
    end Get_Mood;

    function Get_GCase
      (GCase : String) return GCase_Type_Array is
    begin
        if    GCase = "NONE"                then return (NONE,  others => NONE);
        elsif GCase = "NEUT"                then return (NEUT,  others => NONE);
        elsif GCase = "SUBJ"                then return (SUBJ,  others => NONE);
        elsif GCase = "OBJ"                 then return (OBJ,   others => NONE);
        elsif GCase = "IOBJ"                then return (IOBJ,  others => NONE);
        elsif GCase = "PART"                then return (PART,  others => NONE);
        elsif GCase = "LOC"                 then return (LOC,   others => NONE);
        elsif GCase = "EMPTY"               then return (EMPTY, others => NONE);
        elsif GCase = "SUBJ+OBJ"            then return (SUBJ, OBJ,   others => NONE);
        elsif GCase = "OBJ+IOBJ"            then return (OBJ, IOBJ,   others => NONE);
        elsif GCase = "PART+LOC"            then return (PART, LOC,   others => NONE);
        elsif GCase = "EMPTY+IOBJ"          then return (EMPTY, IOBJ, others => NONE);
        elsif GCase = "EMPTY+OBJ"           then return (EMPTY, OBJ,  others => NONE);
        elsif GCase = "SUBJ+OBJ+IOBJ"       then return (SUBJ, OBJ, IOBJ,        others => NONE);
        elsif GCase = "EMPTY+SUBJ+OBJ"      then return (EMPTY, SUBJ, OBJ,       others => NONE);
        elsif GCase = "EMPTY+SUBJ+OBJ+IOBJ" then return (EMPTY, SUBJ, OBJ, IOBJ, others => NONE);
        elsif GCase = "IOBJ+PART"           then return (IOBJ, PART, others => NONE);            -- german
        elsif GCase = "SUBJ+OBJ+PART+IOBJ"  then return (SUBJ, OBJ, PART, IOBJ, others => NONE); -- german
        elsif GCase = "SUBJ+OBJ+PART"       then return (SUBJ, OBJ, PART, others => NONE);       -- german
        elsif GCase = "IOBJ+OBJ"            then return (IOBJ, OBJ, others => NONE);             -- german
        elsif GCase = "SUBJ+IOBJ+OBJ"       then return (SUBJ, OBJ, IOBJ, others => NONE);       -- german
        elsif GCase = "PART+IOBJ"           then return (PART, IOBJ, others => NONE);            -- german
        elsif GCase = "OBJ+PART+IOBJ"       then return (OBJ, PART, IOBJ, others => NONE);       -- german
        else raise Wrong_GCase with "GCase = """ & GCase & """";
        end if;
    end Get_GCase;

    function Gen_Match
      (A, B         : Gender_Type;
       Strict_Match : Boolean := True) return Boolean is
    begin
        return
          (Strict_Match and then A = NONE) or else
          (Strict_Match and then B = NONE) or else
          A = BOTH or else
          B = BOTH or else
          A = B;
    end Gen_Match;

    function Num_Match
      (A, B         : Number_Type;
       Strict_Match : Boolean := True) return Boolean is
    begin
        return
          (Strict_Match and then A = NONE) or else
          (Strict_Match and then B = NONE) or else
          A = BOTH or else
          B = BOTH or else
          A = B;
    end Num_Match;

    function Pers_Match
      (A, B         : Person_Type;
       Strict_Match : Boolean := True) return Boolean is
    begin
        return
          (Strict_Match and then A = NONE) or else
          (Strict_Match and then B = NONE) or else
          A = ALLVAL or else
          B = ALLVAL or else
          A = B;
    end Pers_Match;

    function GCase_Match
      (A            : GCase_Type_Array;
       B            : GCase_Type;
       Strict_Match : Boolean := True) return Boolean is
        pragma Unreferenced (Strict_Match);
    begin
        return Ret : Boolean := False do
            for I in A'Range loop
                if A (I) = B then
                    Ret := True;
                    exit;
                end if;
            end loop;
        end return;
    end GCase_Match;

    function Combine_Genders
      (A, B : Gender_Type) return Gender_Type is
    begin
        return Combine_Genders_Array (A, B);
    end Combine_Genders;
    pragma Unreferenced (Combine_Genders);

    function Combine_Person
      (A, B : Person_Type) return Person_Type is
    begin
        return Combine_Person_Array (A, B);
    end Combine_Person;
    pragma Unreferenced (Combine_Person);

    -----------------
    -- Part of Speech
    -----------------

    function Is_Open_Pos
      (POS : POS_Type_Array) return Boolean is
    begin
        return Match_POS (POS, VERB)
          or else Match_POS (POS, NOUN)
          or else Match_POS (POS, ADV)
          or else Match_POS (POS, ADJ);
    end Is_Open_Pos;
    pragma Unreferenced (Is_Open_Pos);

    function Get_POS
      (POS : String) return POS_Type_Array is
    begin
        if String_To_POS_Array.Contains (POS) then
            return String_To_POS_Array.Element (POS);
        else
            raise Wrong_POS with "POS = """ & POS & """";
        end if;
    end Get_POS;

    function Simplify_POS
      (POS : in String) return String is
    begin
        --if POS = "DATE" or else POS = "HOUR" then
        --    return "NOUN";
        --end if;
        return POS;
    end Simplify_POS;
    pragma Unreferenced (Simplify_POS);

    function To_String
      (POS : POS_Type_Array) return String is
    begin
        return POS_Array_To_String.Element (POS);
    exception
        when Constraint_Error =>
            raise Wrong_POS with "POS array not in map: " & POS (1)'Img & " - " & POS (2)'Img & " - " & POS (3)'Img & " - " & POS (4)'Img;
    end To_String;

    function Match_POS
      (Source_PosTag     : POS_Type_Array;
       Target_First_Pos  : Misc_POS_Type;
       Target_Second_Pos : Misc_POS_Type;
       Strict_Match      : Boolean := False) return Boolean is
    begin
        return Match_POS (Source_PosTag, (Target_First_Pos, Target_Second_Pos, others => NONE), Strict_Match);
    end Match_POS;

    function Match_POS
      (Source_PosTag     : POS_Type_Array;
       Target_First_Pos  : Misc_POS_Type;
       Strict_Match      : Boolean := False) return Boolean is
    begin
        return Match_POS (Source_PosTag, (Target_First_Pos, others => NONE), Strict_Match);
    end Match_POS;

    function Match_POS
      (Source_PosTag : POS_Type_Array;
       Target_PosTag : POS_Type_Array;
       Strict_Match  : Boolean := False) return Boolean is
        Ret : Boolean := False;
    begin
        if Source_PosTag (Source_PosTag'First) = Target_PosTag (Target_PosTag'First) then
            Ret := True;
            for I in Source_PosTag'First + 1 .. Source_PosTag'Last loop
                exit when not Strict_Match and then Target_PosTag (I) = NONE;
                if Target_PosTag (I) /= Source_PosTag (I) then
                    Ret := False;
                    exit;
                end if;
            end loop;
        end if;

        return Ret;
    end Match_POS;

    function Match_MWE_POS
      (First_PosTag  : POS_Type_Array;
       Second_PosTag : POS_Type_Array) return Boolean is

        Ret : Boolean := False;
    begin

        if First_PosTag (First_PosTag'First) = Second_PosTag (Second_PosTag'First) then
            Ret := True;
            for I in First_PosTag'First + 1 .. First_PosTag'Last loop
                if First_PosTag (I) /= Second_PosTag (I) then
                    if not ((First_PosTag (I) = NONE and then Second_PosTag (I) = POLI)
                            or else (First_PosTag (I) = POLI and then Second_PosTag (I) = NONE)) then
                        Ret := False;
                        exit;
                    end if;
                end if;
            end loop;
        end if;

        return Ret;

    end Match_MWE_POS;


    function Is_Open_Class
      (POS : in POS_Type_Array) return Boolean is
    begin
        return
          Match_POS (POS, VERB) or else
          Match_POS (POS, NOUN) or else
          Match_POS (POS, ADJ, QUALIF) or else
          Match_POS (POS, ADJ, Strict_Match => True) or else
          Match_POS (POS, ADV, Strict_Match => True);
        --Match_POS(POS, ADV, MANNER) or else
    end Is_Open_Class;

    function Create_String
      (GCases : GCase_Type_Array) return String is

        Total_Length : Natural := 0;
    begin

        if GCases (GCases'First) = NONE then
            return GCase_Type'Image(NONE);
        end if;

        for I in GCases'Range loop
            exit when GCases (I) = NONE;
            Total_Length := Total_Length + GCases (I)'Img'Length + 1;
        end loop;

        declare
            Str      : String (1 .. Total_Length);
            Position : Natural := Str'First;
        begin
            for I in GCases'Range loop
                exit when GCases (I) = NONE;
                declare
                    Cur_GCase : constant String
                      := GCases (I)'Img;
                begin
                    --T_IO.Put_Line("Token => " & I'Img & " - " & Cur_POS);
                    if Position > Str'First then
                        Str (Position) := '+';
                        Position := Position + 1;
                    end if;
                    Str (Position .. Position + Cur_GCase'Length - 1) := Cur_GCase;
                    Position := Position + Cur_GCase'Length;
                end;
            end loop;

            return Str (1 .. Position - 1);
        end;
    end Create_String;

    ----------
    -- Private
    ----------

    function Create_String
      (POS : POS_Type_Array) return String is

        Total_Length : Natural := 0;
    begin

        for I in POS'Range loop
            exit when POS (I) = NONE;
            Total_Length := Total_Length + POS (I)'Img'Length + 1;
        end loop;

        declare
            Str      : String (1 .. Total_Length);
            Position : Natural := Str'First;
        begin
            for I in POS'Range loop
                exit when POS (I) = NONE;
                declare
                    Cur_POS : constant String
                      := POS (I)'Img;
                begin
                    --T_IO.Put_Line("Token => " & I'Img & " - " & Cur_POS);
                    if Position > Str'First then
                        Str (Position) := '-';
                        Position := Position + 1;
                    end if;
                    Str (Position .. Position + Cur_POS'Length - 1) := Cur_POS;
                    Position := Position + Cur_POS'Length;
                end;
            end loop;

            return Str (1 .. Position - 1);
        end;
    end Create_String;

    function Hash (POS : POS_Type_Array) return Ada.Containers.Hash_Type is
        use  Ada.Containers;
        Ret : Hash_Type := 0;
    begin
        for I in POS'Range loop
            Ret := Ret + ( Misc_POS_Type'Pos (POS (I)) * (100 ** (Natural(I) - 1)) );
        end loop;
        return Ret;
    end Hash;

    procedure Initialize_Part_Of_Speech is
        procedure Callback (Cursor : String_To_POS_Array_Maps.Cursor) is
        begin
            POS_Array_To_String.Insert
              (String_To_POS_Array_Maps.Element (Cursor),
               String_To_POS_Array_Maps.Key (Cursor));
            --Put_Line(String_To_POS_Array_Maps.Key(Cursor));
        end Callback;
    begin
        --Put_Line("Initialize_Part_Of_Speech");
        for P of Known_Part_Of_Speech loop
            String_To_POS_Array.Insert (Create_String (P), P);
        end loop;

        String_To_POS_Array_Maps.Iterate (String_To_POS_Array, Callback'Access);
    end;

begin

    if Misc_POS_Type'Pos (Misc_POS_Type'Last) >= 100 then
        raise Constraint_Error with "Hashing Error: Misc_POS_Type'Pos(Misc_POS_Type'Last) >= 100";
    end if;

    Initialize_Part_Of_Speech;

end NLColl.Linguistic_Description.Morphology;
