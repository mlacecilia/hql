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

 \brief show msg info and returns button pressed
 \param(IN)
 \return numeric

*/
FUNCTION hql_MsgInfo( cText, cTitle, cDetail, cInfo )
   LOCAL nRet
   nRet := hql_msgBox( cText, cTitle, cDetail, cInfo, /*winIco*/, QMessageBox_Information, QMessageBox_Ok, QMessageBox_NoButton )
   //nRet := hql_msgBox( cText, cTitle, cDetail, cInfo, /*winIco*/, ":/hqlres/information", QMessageBox_Ok, QMessageBox_NoButton )
RETURN nRet

/*!

 \brief show msg stop and returns button pressed
 \param(IN)
 \return numeric

*/
FUNCTION hql_MsgStop( cText, cTitle, cDetail, cInfo )
   LOCAL nRet
   nRet := hql_msgBox( cText, cTitle, cDetail, cInfo, /*winIco*/, QMessageBox_Critical, QMessageBox_Ok, QMessageBox_NoButton )
   //nRet := hql_msgBox( cText, cTitle, cDetail, cInfo, /*winIco*/, ":/hqlres/critical", QMessageBox_Ok, QMessageBox_NoButton )
RETURN nRet

/*!

 \brief show msg warning and returns button pressed
 \param(IN)
 \return numeric

*/
FUNCTION hql_MsgWarn( cText, cTitle, cDetail, cInfo )
   LOCAL nRet
   nRet := hql_msgBox( cText, cTitle, cDetail, cInfo, /*winIco*/, QMessageBox_Warning, QMessageBox_Ok, QMessageBox_NoButton )
   //nRet := hql_msgBox( cText, cTitle, cDetail, cInfo, /*winIco*/, ":/hqlres/warning", QMessageBox_Ok, QMessageBox_NoButton )
RETURN nRet

/*!

 \brief show msg Yes/No and returns button pressed
 \param(IN)
 \return numeric

*/
FUNCTION hql_MsgYesNo( cText, cTitle, cDetail, cInfo )
   LOCAL nRet
   LOCAL nStdButtons := hb_BitOr( QMessageBox_Yes, QMessageBox_No )
   nRet := hql_msgBox( cText, cTitle, cDetail, cInfo, /*winIco*/, QMessageBox_Question, nStdButtons, QMessageBox_No )
   //nRet := hql_msgBox( cText, cTitle, cDetail, cInfo, /*winIco*/, ":/hqlres/question", nStdButtons, QMessageBox_No )
RETURN nRet
/*!

 \brief show msg box and returns button pressed
 \param(IN)
 \return numeric

*/

FUNCTION hql_msgBox( cText, cTitle, cDetail, cInfo, winIco, signIco, nButtons, nDefault )
   LOCAL oParent
   LOCAL oMsg
   LOCAL nRet

   cText := hb_DefaultValue( cText, "" )
   nButtons := hb_DefaultValue( nButtons, QMessageBox_Ok )
   nDefault := hb_DefaultValue( nDefault, QMessageBox_NoButton )

   IF ( !hb_IsObject( oParent := HqlQApplication:focusWidget() ) )
      IF ( !hb_IsObject( oParent := HqlQApplication:focusWindow() ) )
         oParent := HqlQApplication
      ENDIF
   ENDIF

   oMsg := hqlMessageBox( /*name*/, oParent )
   oMsg:setText( cText )
   oMsg:setStandardButtons( nButtons )
   oMsg:setDefaultButton( nDefault )

   IF ( winIco == NIL )
      oMsg:setWindowIcon( QIcon( ":/hqlres/HQL96" ) )
   ELSEIF ( hb_IsString( winIco ) )
      oMsg:setWindowIcon( QIcon( winIco ) )
   ENDIF

   IF ( hb_IsString( cTitle ) )
      oMsg:setWindowTitle( cTitle )
   ENDIF

   IF ( hb_IsString( cDetail ) )
      oMsg:setDetailedText( cDetail )
   ENDIF

   IF ( hb_IsString( cInfo ) )
      oMsg:setInformativeText( cInfo )
   ENDIF

   IF ( hb_IsNumeric( signIco ) )
      oMsg:setIcon( signIco )
   ELSEIF ( hb_IsString( signIco ) )
      oMsg:setIconPixmap( QPixmap( signIco ) )
   ENDIF

   nRet := oMsg:hqlActivate()

RETURN nRet
