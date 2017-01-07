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

with Ada.Text_IO;
with NLColl.Linguistic_Description.Sentences; use NLColl.Linguistic_Description.Sentences;
with NLColl.Linguistic_Description.Syntax.Dependency_Trees; use NLColl.Linguistic_Description.Syntax.Dependency_Trees;

package NLColl.Corpus.Treebank is

    use NLColl.Linguistic_Description.Sentences.Tokens;
    use NLColl.Linguistic_Description.Syntax.Dependency_Trees.Statistics;
    
    procedure Read_CoNLL
      (File_Name           :  in String;
       Sentences           : out Sentence_Vectors.Vector;
       Dependency_Trees    : out Dependency_Tree_Vectors.Vector;
       Number_Of_Sentences : out Natural;
       Number_Of_Tokens    : out Natural;
       Ignore_Deprel       :  in Boolean := False;
       Verbose             :  in Boolean := True);
    
    procedure To_CoNLL
      (Output          : in Ada.Text_IO.File_Type := Ada.Text_IO.Standard_Error;
       Sentence        : in Sentence_Type;
       Dependency_Tree : in Dependency_Tree_Type);
        
    procedure Write_CoNLL
      (File_Name        : in String;
       Sentences        : in Sentence_Vectors.Vector;
       Dependency_Trees : in Dependency_Tree_Vectors.Vector);
    
    Is_Not_A_Tree : exception;
    
private
    
    Max_Number_Of_ConLL_Column : constant := 10;
    Min_Number_Of_ConLL_Column : constant := 8;
    
    DEPREL_SEPARATOR     : constant String := " ";
    POS_DEPREL_SEPARATOR : constant String := "~";
    
end NLColl.Corpus.Treebank;

