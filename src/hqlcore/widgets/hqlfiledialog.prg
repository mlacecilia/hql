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
#include "hbclass.ch"
#include "hqlhbqt.ch"

/*!

 \brief Returns a new hql_fileDialog object instance

*/
FUNCTION hqlFileDialog( ... )
RETURN hql_fileDialog():new( ... )

/*!

 \brief define hql_fileDialog class

*/
CLASS hql_fileDialog INHERIT hb_QFileDialog, hql_abs0030

   EXPORTED:
   METHOD init
   METHOD hqlActivate
   METHOD hqlOnFilesSelected
   METHOD hqlOnUrlsSelected

   PROTECTED:
   VAR bHqlFilesSelected                  INIT NIL
   VAR bHqlUrlsSelected                   INIT NIL
   SIGNAL __hql_QFilesSelected
   SIGNAL __hql_QUrlsSelected

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance for given [name], [Qt flags]
 \param(IN) string, ...
 \return self

*/
METHOD init( cName, ... ) CLASS hql_fileDialog

   ::QFileDialog:init( ... )
   ::setWindowIcon( QIcon( ":/hqlres/HQL96" ) )
   /* WARNING: using QFileDialog_DontUseNativeDialog, Qt assign "QFileDialog" as objectName
      so, when parent given duplicateName returns .T. IOW hql error created */
   ::setOption( QFileDialog_DontUseNativeDialog, .T. )

   IF ( !::__hqlHasParent() )
      ::__hqlNestedWithSetParent()
   ENDIF

   ::__hqlAssignObjectName( cName )
   ::__hqlConnect()

RETURN self

/*!

 \brief Activate (show/rise) form
 \param(IN)
 \return numeric

*/
METHOD hqlActivate() CLASS hql_fileDialog

   /*IF ( !::lHqlActivated )
      ::__hqlWinPositioning()
   ENDIF*/

   ::show()

   /*IF ( !::lHqlActivated .AND. hb_IsObject( ::oHqlSplashScreen ) )
      ::oHqlSplashScreen:finish( Self )
   ENDIF*/

   ::raise()

   ::activateWindow()

   IF ( ::lHqlActivated )
      HqlFw:processEvents( /*nFlags*/, /*nMsecs*/ )
   ELSE
      ::lHqlActivated := .T.
      ::nHqlReleaseCode := ::exec()
   ENDIF

RETURN ::nHqlReleaseCode

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnFilesSelected( arg1 ) CLASS hql_fileDialog
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlFilesSelected := arg1
      IF ( hb_IsEvalItem(::bHqlFilesSelected) )
         ::connect( "filesSelected(QStringList)" , { |oStringList| ::__hql_QFilesSelected(oStringList) } )
      ELSE
         ::disconnect( "filesSelected(QStringList)" )
      ENDIF
   ENDIF
RETURN self

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnUrlsSelected( arg1 ) CLASS hql_fileDialog
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlUrlsSelected := arg1
      IF ( hb_IsEvalItem(::bHqlUrlsSelected) )
         ::connect( "urlselected(QList)" , { |oList| ::__hql_QUrlsSelected(oList) } )
      ELSE
         ::disconnect( "urlselected(QList)" )
      ENDIF
   ENDIF
RETURN self

// ==================== PROTECTED section ====================

// ==================== SLOTS/EVENTS section ====================

/*!

 \brief [PROTECTED] signal/event handler
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
METHOD __hql_QFilesSelected( oColor ) CLASS hql_fileDialog
   IF ( hb_IsEvalItem(::bHqlFilesSelected) )
      EVAL( ::bHqlFilesSelected, oColor, Self )  // Self always as last
   ENDIF
RETURN .F.

/*!

 \brief [PROTECTED] signal/event handler
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
SIGNAL __hql_QUrlsSelected( oList ) CLASS hql_fileDialog
   IF ( hb_IsEvalItem( ::bHqlUrlsSelected ) )
      EVAL( ::bHqlUrlsSelected, oList, Self )  // Self always as last
   ENDIF
RETURN .F.

// ==================== HIDDEN section ====================
