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

 \brief Returns a new hql_fontDialog object instance

*/
FUNCTION hqlFontDialog( ... )
RETURN hql_fontDialog():new( ... )

/*!

 \brief define hql_fontDialog class

*/
CLASS hql_fontDialog INHERIT hb_QFontDialog, hql_abs0030

   EXPORTED:
   METHOD init
   METHOD hqlActivate
   METHOD hqlOnFontSelected

   PROTECTED:
   VAR bHqlFontSelected                   INIT NIL
   SIGNAL __hql_QFontSelected

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance for given [name], [Qt flags]
 \param(IN) string, ...
 \return self

*/
METHOD init( cName, ... ) CLASS hql_fontDialog

   ::QFontDialog:init( ... )
   ::setWindowIcon( QIcon( ":/hqlres/HQL96" ) )
   /* ::setOption( QFontDialog_DontUseNativeDialog, .T. ) */
   ::setOptions( hb_BitOr( QFontDialog_DontUseNativeDialog, QFontDialog_ScalableFonts, QFontDialog_NonScalableFonts, QFontDialog_MonospacedFonts, QFontDialog_ProportionalFonts ) )

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
METHOD hqlActivate() CLASS hql_fontDialog

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
   WARNING 2019 signal is NOT emitted by HbQt or Qt
 \param(IN) block
 \return Self

*/
METHOD hqlOnFontSelected( arg1 ) CLASS hql_fontDialog
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlFontSelected := arg1
      IF ( hb_IsEvalItem(::bHqlFontSelected) )
         ::connect( "fontSelected(QFont)" , { |oFont| ::__hql_QFontSelected(oFont) } )
      ELSE
         ::disconnect( "fontSelected(QFont)" )
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
METHOD __hql_QFontSelected( oFont ) CLASS hql_fontDialog
   IF ( hb_IsEvalItem(::bHqlFontSelected) )
      EVAL( ::bHqlFontSelected, oFont, Self )  // Self always as last
   ENDIF
RETURN .F.

// ==================== HIDDEN section ====================
