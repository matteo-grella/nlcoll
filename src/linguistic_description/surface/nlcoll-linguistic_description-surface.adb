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

with Unicode;
with Unicode.CES.Utf8;


package body NLColl.Linguistic_Description.Surface is

    function Uni2Str
      (C : in Unicode.Unicode_Char) return String is
        S : String (1 .. 10);
        I : Natural;
    begin
        I := S'First - 1;
        Unicode.CES.Utf8.Encode (C, S, I);
        return S (1 .. I);
    end Uni2Str;

    function Remove_Accents
      (Str                  : in String;
       Skip_Apostrophe      : in Boolean := False) return String is

        use Unicode;
        use Chars;

        Out_Str              : String (Str'First .. Str'Last + (Str'Length * 4));
        -- *4 because a single utf-8 char could be 1 to 4 byte length

        Last_Index           : Natural := Str'First - 1;
        Cur_Index            : Natural := Str'First;
        Char                 : Unicode.Unicode_Char;
        Is_Last_Character    : Boolean := False;
        Add_Final_Apostrophe : Boolean := False;
    begin

        while Cur_Index <= Str'Last loop
            Unicode.CES.Utf8.Read
              (Str   => Str,
               Index => Cur_Index,
               Char  => Char);

            if not Skip_Apostrophe or else not Chars_Like_Apostrophe (Char) then
                Is_Last_Character := (Cur_Index > Str'Last);

                -- Remove accent
                if Chars_To_Basic_Chars (Char) /= 0 then
                    Char := Chars_To_Basic_Chars (Char);
                    if Is_Last_Character then
                        Add_Final_Apostrophe := True;
                    end if;
                end if;

                -- Normalize apostrophe
                if Chars_Like_Apostrophe (Char) then
                    Char := Unicode.Names.Basic_Latin.Apostrophe;
                end if;

                -- Checking last combi mark
                if Is_Last_Character and then Chars_Combining_Diacritical_Marks (Char) then
                    Add_Final_Apostrophe := True;
                end if;

                -- Skip Combining Diacritical Marks
                if not Chars_Combining_Diacritical_Marks (Char) then
                    -- Insert character
                    declare
                        SC : constant String := Uni2Str (Char);
                    begin
                        Out_Str (Last_Index + 1 .. Last_Index + SC'Length) := SC;
                        Last_Index := Last_Index + SC'Length;
                    end;
                end if;

            end if;
        end loop;

        if not Skip_Apostrophe and then Add_Final_Apostrophe then
            Out_Str (Last_Index + 1) := ''';
            Last_Index := Last_Index + 1;
        end if;

        return Out_Str (Str'First .. Last_Index);

    end Remove_Accents;

    procedure Empty is
    begin
        null;
    end Empty;

end NLColl.Linguistic_Description.Surface;
