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

 \brief Returns a new hql_menu object instance

*/
FUNCTION hqlMenu( ... )
RETURN hql_menu():new( ... )

/*!

 \brief define hql_menu class
   WARNING to remember: from Qt doc "Although a popup menu is always a top-level widget"

*/
CLASS hql_menu INHERIT hb_QMenu, hql_abs0050

   EXPORTED:
   METHOD init
   METHOD hqlAddToRecent
   METHOD hqlClearRecent
   METHOD hqlIsRecentEnabled              INLINE ( ::lHqlRecentEnabled )
   METHOD hqlIsVisible                    // trik to know if Qmenu hide
   METHOD hqlOnRecentSelected
   METHOD hqlRecentEnabled
   METHOD hqlRecentList                   INLINE ( ACLONE(::aHqlRecents) )
   METHOD hqlRecentSize                   INLINE ( LEN(::aHqlRecents) )
   METHOD hqlSetMaxRecents
   METHOD hqlSetRecentList
   METHOD hqlSetVisible                   // trik to hide a QMenu; taken from internet because (seems) QMenu::setVisible(false) doesn't works

   PROTECTED:
   VAR nHqlMaxRecents                     INIT 10
   VAR aHqlRecents                        INIT {}
   VAR lHqlRecentEnabled                  INIT .F.
   DATA bHqlRecentSelected                INIT NIL
   METHOD __hqlClearHelper
   METHOD __hqlSlotRecent
   SIGNAL __hql_QAboutToShow

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance for given [name], [Qt flags]
 \param(IN) string, ...
 \return self

*/
METHOD init( cName, ... ) CLASS hql_menu

   ::QMenu:init( ... )

   IF ( !::__hqlHasParent() )
      ::__hqlNestedWithSetParent()
   ENDIF

   ::__hqlAssignObjectName( cName )
   ::__hqlConnect()

RETURN self

/*!

 \brief add string to recent menu
 \param(IN) string
 \return Self

*/
METHOD hqlAddToRecent( cString ) CLASS hql_menu
   LOCAL aTemp := {}
   LOCAL nAt
   LOCAL oAction

   cString := hb_DefaultValue(cString, "")

   IF ( ::hqlIsRecentEnabled() .AND. !EMPTY(cString) )

      ::__hqlClearHelper() // read notes

      ::clear()

      AADD( aTemp, cString )

      // make a temporary list copying from current and chekc if it's too long and dulicates
      FOR nAt := 1 TO ::hqlRecentSize()
         // to avoid a very long list
         IF ( LEN(aTemp) < ::nHqlMaxRecents )
            // avoids duplicates
            IF ( !(::aHqlRecents[ nAt ] == cString) )
               AADD( aTemp, ::aHqlRecents[ nAt ] )
            ENDIF
         ENDIF
      NEXT nAt

      ::aHqlRecents := ACLONE( aTemp )

      FOR nAt := 1 TO ::hqlRecentSize()
         WITH OBJECT oAction := hqlAction( /*name*/, Self )
            :setText( ::aHqlRecents[ nAt ] )
            :hqlOnTriggered( { |lBool,oSelf| ::__hqlSlotRecent(lBool,oSelf) } )
         END WITH
         ::addAction( oAction )
      NEXT nAt

      // menu is enabled only if list > 0
      ::setEnabled( ( ::hqlRecentSize() > 0 ) )

   ENDIF

RETURN Self

/*!

 \brief cleare recent list and current menu
 \param(IN) none
 \return Self

*/
METHOD hqlClearRecent() CLASS hql_menu
   ::__hqlClearHelper() // read notes
   ::clear()
   ::aHqlRecents := {}
   ::setEnabled( .F. )
RETURN Self

/*!

 \brief returns true if menu is visible otherwise false
 \param(IN) none
 \return bool

*/
METHOD hqlIsVisible() CLASS hql_menu
RETURN ::menuAction:isVisible()

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnRecentSelected( arg1 ) CLASS hql_menu
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlRecentSelected := arg1
   ENDIF
RETURN self

/*!

 \brief Sets recents menu enabled
 \param(IN) numeric
 \return Self

*/
METHOD hqlRecentEnabled( arg1 ) CLASS hql_menu
   IF ( hb_IsLogical(arg1) )
      ::lHqlRecentEnabled := arg1
      IF ( ::lHqlRecentEnabled )
         ::connect( "aboutToShow()", { || ::__hql_QAboutToShow() } )
      ELSE
         ::disconnect( "aboutToShow()" )
      ENDIF
   ENDIF
RETURN Self

/*!

 \brief Sets maximum recents list
 \param(IN) numeric
 \return Self

*/
METHOD hqlSetMaxRecents( arg1 ) CLASS hql_menu
   IF ( hb_IsNumeric(arg1) .AND. arg1 >= 0 )
      ::nHqlMaxRecents := arg1
   ENDIF
RETURN self

/*!

 \brief set/get block
 \param[in] block | NIL
 \return Self

*/
METHOD hqlSetRecentList( aList ) CLASS hql_menu
   LOCAL nAt
   LOCAL oAction

   aList := hb_DefaultValue(aList, {})

   IF ( ::hqlIsRecentEnabled() )
      ::hqlClearRecent()

      FOR nAt := 1 TO LEN( aList )
         IF ( hb_IsString(aList[ nAt ]) .AND. !EMPTY(aList[ nAt ]) )
            // to avoid a very long list
            IF ( LEN(::aHqlRecents) < ::nHqlMaxRecents )
               // to avoid duplicates
               IF ( ASCAN( ::aHqlRecents, { |e_| e_ == aList[ nAt ] } ) == 0 )
                  AADD( ::aHqlRecents, aList[ nAt ] )
               ENDIF
            ENDIF
         ENDIF
      NEXT nAt

      // creates actions
      FOR nAt := 1 TO ::hqlRecentSize()
         WITH OBJECT oAction := hqlAction( /*name*/, Self )
            :setText( ::aHqlRecents[ nAt ] )
            :hqlOnTriggered( { |lBool,oSelf| ::__hqlSlotRecent(lBool,oSelf) } )
         END WITH
         ::addAction( oAction )
      NEXT nAt

      // menu is enabled only if list > 0
      ::setEnabled( ( ::hqlRecentSize() > 0 ) )

   ENDIF

RETURN Self

/*!

 \brief set Qmenu visible or not
 \param(IN) bool
 \return SELF

*/
METHOD hqlSetVisible( arg1 ) CLASS hql_menu
   IF ( hb_IsLogical(arg1) )
      ::menuAction:setVisible( arg1 )
   ENDIF
RETURN Self

// ==================== PROTECTED section ====================

/*!

 \brief [PROTECTED] helper to well clear menu; when :clear() is performed, HbQt does'nt clear very well childrens.
 \param(IN) none
 \return NIL

*/
METHOD __hqlClearHelper() CLASS hql_menu
   LOCAL oActionsList
   LOCAL nAt

   oActionsList := ::actions()
   FOR nAt := 0 TO (oActionsList:size()-1)
      oActionsList:at( nAt ):disconnect()
   NEXT nAt

RETURN NIL

/*!

 \brief [PROTECTED] handle event/signal
 \param(IN) ... based on event/signal
 \return object | NIL

*/
METHOD __hqlSlotRecent( lBool, oAction ) CLASS hql_menu

   IF ( ::hqlIsRecentEnabled() )
      IF ( hb_IsEvalItem(::bHqlRecentSelected) )
         EVAL( ::bHqlRecentSelected, lBool, oAction )  // Self(oAction) always as last
      ENDIF
   ENDIF

RETURN NIL

// ==================== SLOTS/EVENTS section ====================

/*!

 \brief [PROTECTED] handle event/signal
 \param(IN) ... based on event/signal
 \return object | NIL

*/
METHOD __hql_QAboutToShow() CLASS hql_menu
   IF ( ::hqlIsRecentEnabled() )
      ::setEnabled( ( ::hqlRecentSize > 0 ) )
   ENDIF

RETURN .F.      // .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog

// ==================== HIDDEN section ====================
