/****************************************************************************
** $HQL_BEGIN_LICENSE$
**
** Copyright (C) 2020 by Luigi Ferraris
**
** This file is part of HQL project
**
** HQL is free software: you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation, either version 3 of the License, or
** (at your option) any later version.
**
** HQL is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with HQL.  If not, see <http://www.gnu.org/licenses/>.
**
** $HQL_END_LICENSE$
****************************************************************************/
#include "hqlinclude.ch"

/*!

 \brief show mainwindow

*/
PROCEDURE UDFfileDialog( oParent )
   LOCAL oWnd, oSize

   WITH OBJECT oWnd := hqlChildWindow(/*name*/, oParent)
      :setWindowTitle( "HQLFILEDIALOG tester" )
      :setWindowIcon( QIcon( ":/hqlres/HQL96" ) )

      WITH OBJECT hqlMenuBar(/*name*/)
         WITH OBJECT :hqlAddMenu(/*name*/)
            :hqlCaption( "&File" ) //==>:setTitle( "&File" )
            WITH OBJECT :hqlAddAction(/*name*/)
               :hqlCaption( "&Quit" )  //==>:setText( "&Quit" )
               :setIcon( QIcon( ":/hqlres/quit" ) )
               :setShortcut( QKeySequence( "Alt+Q" ) )
               :hqlOnTriggered( { || oWnd:hqlRelease() } )
            END WITH
            :addSeparator()
            WITH OBJECT :hqlAddAction(/*name*/)
               :hqlCaption( "&Close all windows" )
               :hqlOnTriggered( { || hqlQapplication:closeAllWindows() } )
               :setIcon( QIcon( ":/hqlres/exit" ) )
            END WITH
         END WITH

         WITH OBJECT :hqlAddMenu(/*name*/)
            :hqlCaption( "&Samples" ) //==>:setTitle( "&File" )
            WITH OBJECT :hqlAddAction(/*name*/)
               :hqlCaption( "get &Directory" )
               :hqlOnTriggered( { || UDFgetDirectory() } )
            END WITH
            WITH OBJECT :hqlAddAction(/*name*/)
               :hqlCaption( "&Choose files" )
               :hqlOnTriggered( { || UDFchooseFiles() } )
            END WITH
            WITH OBJECT :hqlAddAction(/*name*/)
               :hqlCaption( "&Save file" )
               :hqlOnTriggered( { || UDFsaveFile() } )
            END WITH
         END WITH

      END WITH

   END WITH

   // trick to resize window at 90% of desktop
   oSize := HqlQDesktop:availableGeometry():size()
   oSize := QSize( oSize:width()*0.9, oSize:height()*0.9 )
   oWnd:resize( oSize )

   oWnd:hqlActivate()

RETURN

STATIC PROCEDURE UDFgetDirectory()
   LOCAL oWnd, nExit, oSlist

   oWnd := hqlFileDialog( /*name*/ )
   oWnd:setFileMode( QFileDialog_Directory )
   oWnd:setOption( QFileDialog_ShowDirsOnly, .T. )
   oWnd:hqlOnFilesSelected( { |oSList| UDFtraceFiles( "from signal :", oSList ) } )

   nExit := oWnd:hqlActivate()
   IF ( nExit == QDialog_Accepted )
      oSlist := oWnd:selectedFiles()
      UDFtraceFiles( "from var :", oSList )
   ELSE
      hql_Trace( "not selected" )
   ENDIF

RETURN

STATIC PROCEDURE UDFchooseFiles()
   LOCAL oWnd, nExit, oSlist

   oWnd := hqlFileDialog( /*name*/ )
   /* oWnd:setDirectory( const QString &directory ) by default current directory */
   /* oWnd:setFileMode( QFileDialog_ExistingFile )   only one */
   oWnd:setFileMode( QFileDialog_ExistingFiles ) /* more then */
   oWnd:hqlOnFilesSelected( { |oSList| UDFtraceFiles( "from signal :", oSList ) } )
   oWnd:setNameFilter( "All Harbour files (*.prg *.ch *.hbp *.hbc)" )

   nExit := oWnd:hqlActivate()
   IF ( nExit == QDialog_Accepted )
      oSlist := oWnd:selectedFiles()
      UDFtraceFiles( "from var :", oSList )
   ELSE
      hql_Trace( "not selected" )
   ENDIF

RETURN

STATIC PROCEDURE UDFsaveFile()
   LOCAL oWnd, nExit, oSlist

   oWnd := hqlFileDialog( /*name*/ )
   /* oWnd:setDirectory( const QString &directory ) by default current directory */
   oWnd:setAcceptMode( QFileDialog_AcceptSave )
   oWnd:setFileMode( QFileDialog_AnyFile )   /*only one */
   oWnd:hqlOnFilesSelected( { |oSList| UDFtraceFiles( "from signal :", oSList ) } )
   oWnd:setNameFilter( "Clipper source (*.prg)" )
   oWnd:setDefaultSuffix( "prg" )

   nExit := oWnd:hqlActivate()
   IF ( nExit == QDialog_Accepted )
      oSlist := oWnd:selectedFiles()
      UDFtraceFiles( "from var :", oSList )
   ELSE
      hql_Trace( "not selected" )
   ENDIF

RETURN

STATIC PROCEDURE UDFtraceFiles( ctext, oSlist )
   LOCAL nAt
   hql_Trace( ctext )

   FOR nAt := 0 TO (oSlist:size()-1)
      hql_Trace( "... # " + hb_NtoS(nAt) + " "  + oSlist:at(nAt) )
   NEXT

RETURN
