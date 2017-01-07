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

with Ada.Exceptions;
with Ada.Wide_Wide_Characters;
with Ada.Text_IO; use Ada.Text_IO;

with ARColl; use ARColl;
with ARColl.Strings; use ARColl.Strings;
with ARColl.Strings.Tokenizer; use ARColl.Strings.Tokenizer;
with ARColl.Strings.Unbounded; use ARColl.Strings.Unbounded;

with NLColl.Linguistic_Description.Morphology; use NLColl.Linguistic_Description.Morphology;

package body NLColl.Corpus.Treebank is

    procedure Split_Deprel
      (Str     : in  String;
       Deprels : out Deprel_Vectors.Vector;
       POSs    : out POS_Vectors.Vector) is
        
        Strs : String_Vectors.Vector;

    begin

        Split (Str, DEPREL_SEPARATOR, Strs);
                            
        for Label of Strs loop
            declare
                Mid    : constant Natural := Find_String (Label, POS_DEPREL_SEPARATOR);
                POS    : String renames Label (Label'First .. Mid - 1);
                Deprel : String renames Label (Mid + 1 .. Label'Last);
            begin
                POSs.Append (To_Unbounded_String(POS));
                Deprels.Append (To_Unbounded_String(Deprel));
            end;
        end loop;

    end Split_Deprel;
    
    procedure Read_CoNLL
      (File_Name           :  in String;
       Sentences           : out Sentence_Vectors.Vector;
       Dependency_Trees    : out Dependency_Tree_Vectors.Vector;
       Number_Of_Sentences : out Natural;
       Number_Of_Tokens    : out Natural;
       Ignore_Deprel       :  in Boolean := False;
       Verbose             :  in Boolean := True) is
        
        File : File_Type;
        
        Line_Count  : Natural := 0;
        Token_Count : Natural := 0;
        
        Sentence        : Sentence_Type;
        Dependency_Tree : Dependency_Tree_Type;
        
    begin
        
        if Verbose then
            Put_Line(Standard_Error, "Reading corpus...");
        end if;
        
        Open(File, In_File, File_Name);

        while not End_Of_File(File) loop
            Line_Count := Line_Count + 1;

            declare
                Buffer : constant String := Get_Line(File);
                
                Strs   : String_Vectors.Vector;
            begin
                
                --Put_Line(Buffer);
                
                if Buffer'Length > 0 then
                    
                    if Buffer (1) = '#' -- Multi-words
                      or else Buffer (1) = '0' -- Semantic Head
                      or else (Buffer'Length > 1 and then Buffer (1 .. 2) = "--") then -- Comments
                        
                        goto Next_Line;
                        
                    end if;
                end if;
                
                -- Split Buffer using Tab separator

                Split(Buffer, ASCII.HT&"", Strs);

                if Buffer'Length > 0 then
                    
                    -- Check if the number of column is valid
                    
                    if Natural (Strs.Length) < Min_Number_Of_ConLL_Column 
                      or else Natural (Strs.Length) > Max_Number_Of_ConLL_Column then
                        
                        raise Corpus_Format_Error
                          with "Invalid Line: " & Line_Count'Img;
                    end if;
                    
                    -- Check if it is a multi-word id (universal-dependencies)
                    if In_String(Strs.Element(Strs.First_Index), "-") then
                        goto Next_Line;
                    end if;
                    
                    declare
                        ID     : constant Token_Index_Type 
                          := Token_Index_Type'Value(Strs.First_Element);
                        
                        Word   : constant String 
                          := Strs.Element(Strs.First_Index + 1);
                        
                        POS    : constant String 
                          := Strs.Element(Strs.First_Index + 3);

                        Head   : constant Extended_Token_Index_Type
                          := Extended_Token_Index_Type'Value
                            (Strs.Element (Strs.First_Index + 6));
                        
                        Deprel : constant String
                          := Strs.Element (Strs.First_Index + 7);
                        
--                            := POS
--                              & POS_DEPREL_SEPARATOR 
--                              & Strs.Element (Strs.First_Index + 7);
--                          
                        POSs     : POS_Vectors.Vector;
                        Deprels  : Deprel_Vectors.Vector;
                    begin
                        
                        if Word'Length = 0 or else POS'Length = 0 then
                            raise Corpus_Format_Error 
                              with "Invalid Word or POS Length at Line: " & Line_Count'Img;
                        end if;
                          
                        if Integer(ID) /= Integer(Sentence.Elements.Length) + 1 then
                            raise Corpus_Format_Error 
                              with "Invalid ID [" & ID'Img & "] at Line: " & Line_Count'Img;
                        end if;

                        if Head < 0 then
                            raise Corpus_Format_Error 
                              with "Invalid Head [" & Head'Img & "] at Line: " & Line_Count'Img;
                        end if;
                        
                        if ARColl.Strings.In_Str(Deprel, POS_DEPREL_SEPARATOR) then
                            Split_Deprel
                              (Str     => Deprel,
                               Deprels => Deprels,
                               POSs    => POSs);
                        else
                            POSs.Append(To_Unbounded_String(POS));
                        end if;
                        
                        declare
                            
                            DPOS   : constant String
                              := To_String (POSs.First_Element);
                            
                            Token  : constant Token_Type
                              := (ID             => ID,
                                  Word           => To_Unbounded_String(Word),
                                  POS            => To_Unbounded_String(DPOS),
                                  Morphology     => Empty_Morphology);
                        begin
                            
                            -- Append Token to the Sentence
                        
                            Sentence.Elements.Append (new Token_Type'(Token));
                        
                        end;
                        
                        -- Set the Dependency Arc
                        
                        Dependency_Tree.Set_Arc
                          (Governor  => Head,
                           Dependent => ID,
                           Deprel    => 
                             (if Ignore_Deprel 
                              then UNKNOWN_DEPREL 
                              else To_Unbounded_String(Deprel)));

                        -- Increment Token counter
                        Token_Count := Token_Count + 1;
                    end;
                end if;

                if (Buffer'Length = 0 or else End_Of_File(File))
                  and then not Sentence.Elements.Is_Empty then

                    if not Dependency_Tree.Is_Tree then
                        raise Is_Not_A_Tree; 
                    end if;

                    -- Append a new sentence
                    
                    Sentences.Append (Sentence);
                    Dependency_Trees.Append(Dependency_Tree);
                    
                    -- Clear temporary structures
                    
                    Sentence.Elements.Clear;
                    Dependency_Tree.Clear;
                    
                    if Verbose then
                        
                        -- Show Progress
                        if Natural(Sentences.Length) mod 1000 = 0 then
                            Put(Standard_Error, "*"); Flush(Standard_Error);
                        elsif Natural(Sentences.Length) mod 100 = 0 then
                            Put(Standard_Error, "."); Flush(Standard_Error);
                        end if;
                        
                    end if;
                end if;
                    
            exception
                when Generic_Error : others =>
                    raise Corpus_Format_Error 
                      with "Error: input string was not valid at line: " 
                      & Line_Count'Img 
                      & " [" & Buffer & "]"
                      & ASCII.LF
                      & Ada.Exceptions.Exception_Information(Generic_Error);                    
            end;
            
            <<Next_Line>>
        end loop;
        
        Close(File);
        
        Number_Of_Tokens    := Token_Count;
        Number_Of_Sentences := Natural(Sentences.Length);
        
        if Verbose then
            
            ----
            -- Print some corpus statistics
            ----
            
            New_Line (Standard_Error);
            Put_Line (Standard_Error, "Total number of senteces .. " & Img (Number_Of_Sentences));
            Put_Line (Standard_Error, "Total number of tokens .... " & Img (Number_Of_Tokens));
            New_Line (Standard_Error);
            
            Print_Dependency_Trees_Stats
              (Dependency_Trees_Display_Name => File_Name,
               Dependency_Trees_Stats        => Get_Dependency_Trees_Stats(Dependency_Trees));
        end if;
        
    end Read_CoNLL;
    
    procedure To_CoNLL
      (Output           : in Ada.Text_IO.File_Type := Standard_Error;
       Sentence         : in Sentence_Type;
       Dependency_Tree  : in Dependency_Tree_Type) is
        
    begin
        
        for J in Sentence.Elements.First_Index .. Sentence.Elements.Last_Index loop
            declare
                Token : constant Token_Type
                  := Sentence.Elements.Element (J).all;
                        
                Line  : constant String
                  := Img(Integer(Token.ID))
                          & ASCII.HT
                          & To_String(Token.Word)
                          & ASCII.HT
                          & "_" 
                          & ASCII.HT
                          & To_String(Token.POS)
                          & ASCII.HT
                          & To_String(Token.POS)
                          & ASCII.HT 
                          & "_" 
                          & ASCII.HT
                          & Img (Integer (Dependency_Tree.Get_Head (Token.ID)))
                          & ASCII.HT 
                          & To_String(Dependency_Tree.Get_Deprel(Token.ID))
                          & ASCII.HT 
                          & "_" 
                          & ASCII.HT 
                          & "_";
            begin
                Put_Line (Output, Line);
            end;
        end loop;
        
    end To_CoNLL;
        
    procedure Write_CoNLL
      (File_Name        : String;
       Sentences        : Sentence_Vectors.Vector;
       Dependency_Trees : Dependency_Tree_Vectors.Vector) is

        File : File_Type;
    begin

        Create (File, Name => File_Name);

        for I in Sentences.First_Index .. Sentences.Last_Index loop
            declare
                Sentence        : constant Sentence_Type
                  := Sentences.Element (I);

                Dependency_Tree : constant Dependency_Tree_Type
                  := Dependency_Trees.Element (I);
            begin

                To_CoNLL
                  (Output          => File,
                   Sentence        => Sentence,
                   Dependency_Tree => Dependency_Tree);

                if I /= Sentences.Last_Index then
                    New_Line (File);
                end if;
            end;
        end loop;

        Close (File);

    end Write_CoNLL;
    
end NLColl.Corpus.Treebank;
