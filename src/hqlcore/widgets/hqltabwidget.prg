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

 \brief Returns a new hql_tabWidget object instance

*/
FUNCTION hqlTabWidget( ... )
RETURN hql_tabWidget():new( ... )

/*!

 \brief define hql_tabWidget class

*/
CLASS hql_tabWidget INHERIT hb_QTabWidget, hql_abs0001

   EXPORTED:
   METHOD init
   METHOD hqlAddTab
   METHOD hqlInsertTab
   METHOD hqlOnCurrentChanged
   METHOD hqlWidget

   PROTECTED:
   VAR bHqlCurrentChanged                 INIT NIL
   METHOD __hqlValueGet
   METHOD __hqlValueSet
   SIGNAL __hql_QCurrentChanged

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance for given [name], [Qt flags]
 \param(IN) string, ...
 \return self

*/
METHOD init( cName, ... ) CLASS hql_tabWidget

   ::QTabWidget:init( ... )

   IF ( !::__hqlHasParent() )
      ::__hqlNestedWithSetParent()
   ENDIF

   ::__hqlAssignObjectName( cName )
   ::__hqlConnect()

RETURN self

/*!

 \brief add new tab with given name, label and icon
 \param(IN) string, string, [icon]
 \return widget

*/
METHOD hqlAddTab( cName, cLabel, xIcon ) CLASS hql_tabWidget
RETURN ::hqlInsertTab( cName, cLabel, xIcon, -1 )

/*!

 \brief add new tab with given name, label and icon before given index
 \param(IN) string, string, [icon], numeric
 \return widget

*/
METHOD hqlInsertTab( cName, cLabel, xIcon, nIndex ) CLASS hql_tabWidget
   LOCAL oObject
   LOCAL nAt

   cLabel := hb_DefaultValue(cLabel, "")

   oObject := hqlWidget( cName, Self )

   // check if name already exists
   IF ( LEN(ALLTRIM(oObject:objectName())) > 0 )
      FOR nAt := 0 TO (::count()-1)
         IF ( UPPER(::widget(nAt):objectName()) == UPPER(oObject:objectName()) )
            hqlThrow( hqlErrorNew( 7003, PROCNAME(), { cName } ) )
         ENDIF
      NEXT nAt
   ENDIF

   nIndex := IIF( hb_IsNumeric( nIndex ), nIndex, -1 )

   // From Qt docs: If you call insertTab() after show(), the layout system will try to adjust to the changes in its widgets hierarchy and may cause flicker.
   // To prevent this, you can set the QWidget::updatesEnabled property to false prior to changes; remember to set the property to true when the changes are done, making the widget receive paint events again.

   ::setUpdatesEnabled( .F. )

   IF ( xIcon == NIL )
      ::insertTab( nIndex, oObject, cLabel )
   ELSE
      ::insertTab( nIndex, oObject, QIcon( xIcon ), cLabel )
   ENDIF

   ::setUpdatesEnabled( .T. )

RETURN oObject

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnCurrentChanged( arg1 ) CLASS hql_tabWidget
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1) .OR. arg1 == NIL ) )
      ::bHqlCurrentChanged := arg1
      IF ( hb_IsEvalItem(::bHqlCurrentChanged) )
         ::connect( "currentChanged(int)" , { |nInt| ::__hql_QCurrentChanged(nInt) } )
      ELSE
         ::disconnect( "currentChanged(int)" )
      ENDIF
   ENDIF
RETURN self

/*!

 \brief helper to get widget at: given position or given name
 \param(IN) numeric | string
 \return object OR nil (not found)

*/
METHOD hqlWidget( arg1 ) CLASS hql_tabWidget
   LOCAL nI

   IF ( hb_IsNumeric(arg1) )
      RETURN ::widget( arg1 )
   ENDIF

   IF ( hb_IsString(arg1) )
      FOR nI := 0 TO (::count()-1)
         IF ( UPPER(::widget(nI):objectName()) == UPPER(arg1) )
            RETURN ::widget(nI)
         ENDIF
      NEXT
   ENDIF

RETURN NIL

// ==================== PROTECTED section ====================

/*!

 \brief [PROTECTED] get text. WARNING using C style (0 based)
 \param(IN) none
 \return numeric

*/
METHOD __hqlValueGet() CLASS hql_tabWidget
RETURN (::currentIndex())

/*!

 \brief [PROTECTED] set text
 \param(IN) numeric
 \return NIL

*/
METHOD __hqlValueSet( arg1 ) CLASS hql_tabWidget
   IF ( hb_IsNumeric(arg1) )
      ::setCurrentIndex(arg1)
   ENDIF
RETURN NIL

// ==================== SLOTS/EVENTS section ====================

/*!

 \brief [PROTECTED] signal/event handler
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
SIGNAL __hql_QCurrentChanged( nInt ) CLASS hql_tabWidget
   IF ( hb_IsEvalItem( ::bHqlCurrentChanged ) )
      EVAL( ::bHqlCurrentChanged, nInt, Self )  // Self always as last
   ENDIF
RETURN .F.

// ==================== HIDDEN section ====================
