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
#include "fileio.ch"
#include "error.ch"
#include "hbqtgui.ch"

/*!

 \brief change error block handling

*/
PROCEDURE hqlErrorSys()
   ERRORBLOCK( {| oError | hql_DefError( oError ) } )
RETURN

/*!

 \brief error block handler copied/arranged from CLIPPER errsys

*/
STATIC FUNCTION hql_DefError( oError )
   LOCAL cMessage, cDOSError, nChoice

   // By default, division by zero results in zero
   IF ( oError:genCode == EG_ZERODIV .AND. oError:canSubstitute )
      RETURN 0
   ENDIF

   // By default, retry on RDD lock error failure
   IF ( oError:genCode == EG_LOCK .AND. oError:canRetry )
      // oError:tries++
      RETURN .T.
   ENDIF

   // Set NetErr() of there was a database open error
   IF ( oError:genCode == EG_OPEN .AND. oError:osCode == 32 .AND. oError:canDefault )
      NETERR( .T. )
      RETURN .F.
   ENDIF

   // Set NetErr() if there was a lock error on dbAppend()
   IF ( oError:genCode == EG_APPENDLOCK .AND. oError:canDefault )
      NETERR( .T. )
      RETURN .F.
   ENDIF

   cMessage := hql_ErrorMessage( oError )

   IF ( !EMPTY( oError:osCode ) )
      cDOSError := hb_StrFormat( "(DOS Error %1$d)", oError:osCode )
   ENDIF

   // always write a log file
   hql_WriteErrorLog( oError )

   // Show alert box IF gui is running
   IF ( hbqt_IsActiveApplication() )
      nChoice := hql_ShowErrorMessage( cMessage, cDOSError, oError )
      IF ( nChoice == QMessageBox_Abort )
         // this make the difference; break means returns oError object to the recover procedure (CLIPPER)
         // BREAK( oError )
      ELSEIF ( nChoice == QMessageBox_Retry )
         RETURN .T.
      ELSEIF ( nChoice == QMessageBox_Ignore )
         RETURN .F.
      ENDIF
   ENDIF

   IF ( hbqt_IsActiveApplication() )
      QApplication():closeAllWindows()
      QApplication():exit( 1 )
   ENDIF

   ERRORLEVEL( 1 )
   QUIT

RETURN .F.

/*!

 \brief make message string for message box
 \param(IN) oerror
 \return string

*/
STATIC FUNCTION hql_ErrorMessage( oError )
   LOCAL cMessage

   // start error message
   cMessage := IIF( oError:severity > ES_WARNING, "Error", "Warning" ) + " "

   // add subsystem name if available
   IF ( hb_IsString( oError:subsystem ) )
      cMessage += oError:subsystem()
   ELSE
      cMessage += "???"
   ENDIF

   // add subsystem's error code if available
   IF ( hb_IsNumeric( oError:subCode ) )
      cMessage += "/" + hb_ntos( oError:subCode )
   ELSE
      cMessage += "/???"
   ENDIF

   // add error description if available
   IF ( hb_IsString( oError:description ) )
      cMessage += "  " + oError:description
   ENDIF

   // add either filename or operation
   IF ( !EMPTY( oError:filename ) )
      cMessage += ": " + oError:filename
   ENDIF
   IF ( !EMPTY( oError:operation ) )
      cMessage += ": " + oError:operation
   ENDIF

RETURN cMessage

/*!

 \brief write log file
 \param(IN) oerror
 \return

*/
STATIC PROCEDURE hql_WriteErrorLog( oError )
   LOCAL cPath, cFile, cExt
   LOCAL nAttr := hb_BitOr( FO_READWRITE, HB_FO_CREAT, HB_FO_TRUNC, FO_DENYWRITE )   //, FO_EXCLUSIVE )
   LOCAL pFile
   LOCAL aStack, cItem

   hb_FnameSplit( hb_ProgName(), @cPath, @cFile, @cExt )
   cFile := hb_FnameMerge( cPath, "hqlerror", "log" )

   pFile := hb_VfOpen( cFile, nAttr )

   IF ( hb_IsPointer(pFile) )
      hb_VfSeek( pFile, 0, FS_END )
      hql_VfWrite( pFile, PADC( " HQL Error log ", 40, "-" ) )

      // write date/time when occured
      hql_VfWrite( pFile, "Error occurred at..: " + hb_TtoS(hb_DateTime()) )

      hql_VfWrite( pFile, "Severity ..........: " + hb_NtoS(oError:Severity()) )
      hql_VfWrite( pFile, "Generic code ......: " + hb_NtoS(oError:GenCode()) )
      hql_VfWrite( pFile, "Subsystem Call ....: " + oError:SubSystem() )
      hql_VfWrite( pFile, "System Code .......: " + hb_NtoS( oError:SubCode() ) )
      hql_VfWrite( pFile, "Recovery available.: " + hb_ValToExp( oError:CanDefault() ) )
      hql_VfWrite( pFile, "Retry available....: " + hb_ValToExp( oError:CanRetry() ) )
      hql_VfWrite( pFile, "Description .......: " + oError:Description() )
      hql_VfWrite( pFile, "Operation .........: " + oError:Operation() )
      hql_VfWrite( pFile, "Arguments .........: " + hql_ErrArg2String( oError ) )
      hql_VfWrite( pFile, "Involved File .....: " + oError:Filename() )
      hql_VfWrite( pFile, "Dos Error Code ....: " + hb_NtoS(oError:OsCode()) )

      aStack := hql_ErrorStack()
      FOR EACH cItem IN aStack
         hql_VfWrite( pFile, cItem )
      NEXT

      hql_VfWrite( pFile, REPLICATE("-", 40) )

      hb_VfFlush( pFile )
      hb_VfCommit( pFile )
      hb_VfClose( pFile )
   ENDIF

   pFile := NIL
RETURN

/*!

 \brief helper to write out text
 \param(IN) pointer, string, bool
 \return

*/
STATIC PROCEDURE hql_VfWrite( pFile, cText, lEol )
   lEol := hb_DefaultValue(lEol, .T.)
   cText += IIF( lEol, hb_Eol(), "" )
   hb_VfWrite( pFile, cText, hb_Blen(cText) )
RETURN

/*!

 \brief helper to convert arguments (error) array into string
 \param(IN) oerror
 \return string

*/
STATIC FUNCTION hql_ErrArg2String( oError )
   LOCAL xArg
   LOCAL cString := ""

   IF ( hb_IsArray(oError:Args) )
      FOR EACH xArg IN oError:Args
         cString += " [" + STR( xArg:__EnumIndex(), 2 ) + "] = Type: " + VALTYPE( xArg )
         IF xArg != NIL
            cString +=  " Val: " + hb_CStr( xArg )
         ENDIF
      NEXT
   ENDIF

RETURN cString

/*!

 \brief helper to create a stack for any procedure involved on error
 \param(IN)
 \return array

*/
STATIC FUNCTION hql_ErrorStack()
   LOCAL aStack := {}
   LOCAL nX

   nX := 1
   WHILE ( !EMPTY(PROCNAME(++nX)) )
      // to avoid these functions
      IF ( hb_Ati("hqlErrorSys", PROCNAME(nX)) == 0 .AND. hb_Ati("hql_DefError", PROCNAME(nX)) == 0 )
         AADD( aStack, hb_StrFormat( "Called from %1$s(%2$d)  ", PROCNAME( nX ), PROCLINE( nX ) ) )
      ENDIF
   ENDDO

RETURN aStack

/*!

 \brief show box message
 \param(IN)
 \return numeric

*/
STATIC FUNCTION hql_ShowErrorMessage( cMessage, cDOSError, oError )
   LOCAL aStack := hql_ErrorStack()
   LOCAL cStack, cItem
   LOCAL cMsg, nButtons, nReturn, oObject

   cStack := ""
   FOR EACH cItem IN aStack
      cStack += cItem + hb_Eol()
   NEXT
   cMsg := cMessage + IIF( hb_IsString( cDOSError ), hb_Eol() + cDOSError, "" )

   nButtons := QMessageBox_Abort
   IF ( oError:canRetry )
      nButtons := hb_BitOr( nButtons, QMessageBox_Retry )
   ENDIF
   IF ( oError:canDefault )
      nButtons := hb_BitOr( nButtons, QMessageBox_Ignore )
   ENDIF

   WITH OBJECT oObject := QMessageBox()
      :setWindowTitle( "HQL error system" )
      :setWindowIcon( QIcon( QMessageBox_Critical ) )
      :setText( cMsg )
      :setDetailedText( cStack )
      :setStandardButtons( nButtons )
      :setDefaultButton( QMessageBox_Abort )
   END WITH

   nReturn := oObject:exec()

   oObject := NIL

RETURN nReturn

/*!

 \brief handling and perfects ERRORNEW object

*/
FUNCTION hqlErrorNew( nSubCode, cOperation, aArgs, cDescription )
   LOCAL oError := ERRORNEW()

   nSubCode := hb_DefaultValue(nSubCode, 7000)

   // default values
   oError:CanDefault    := .F.
   oError:CanRetry      := .F.
   oError:CanSubstitute := .F.
   oError:GenCode       := 1000
   oError:Severity      := ES_ERROR
   oError:SubSystem     := "HQLERR"

   SWITCH nSubCode
   CASE 7001
      oError:Description := "HQL is not started"
      oError:SubCode := 7001
      EXIT
   CASE 7002
      oError:Description := "HQL already started"
      oError:SubCode := 7002
      EXIT
   CASE 7003
      oError:Description := "Control name already defined"
      oError:SubCode := 7003
      EXIT
   CASE 7004
      oError:Description := "Message not found"
      oError:SubCode := 7004
      EXIT
   CASE 7005
      oError:Description := "HQLMAINWINDOW already defined"
      oError:SubCode := 7005
      EXIT
   CASE 7006
      oError:Description := "HQLMAINWINDOW is NOT defined"
       oError:SubCode := 7006
     EXIT
   CASE 7007
      oError:Description := "Valid parent required"
      oError:SubCode := 7007
      EXIT
   CASE 7008
      oError:Description := "Parent not required"
      oError:SubCode := 7008
      EXIT
   OTHERWISE
      IF ( hb_IsString( cDescription ) )
         oError:Description := cDescription
      ELSE
         oError:Description := "HQL generic error"
      ENDIF
      oError:SubCode := 7000
   ENDSWITCH

   IF ( hb_IsString( cOperation ) )
      oError:Operation := cOperation
   ENDIF

   IF ( hb_IsArray( aArgs ) )
      oError:Args := aArgs
   ELSEIF ( hb_IsString( aArgs ) )
      oError:Args := { aArgs }
   ENDIF

RETURN oError
