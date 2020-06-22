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

 \brief Returns a new hql_mainWindow object instance

*/
FUNCTION hqlMainWindow( ... )
RETURN hql_mainWindow():new( ... )

/*!

 \brief define hql_mainWindow class

*/
CLASS hql_mainWindow INHERIT hb_QMainWindow, hql_abs0020

   EXPORTED:
   METHOD init
   METHOD hqlActivate
   METHOD hqlCloseAll                                       // n.b. PAY ATTENTION: it's not Qt closeAllWindows

   PROTECTED:

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance for given [name], [Qt flags]
 \param(IN) string, ...
 \return self

*/
METHOD init( cName, ... ) CLASS hql_mainWindow

   IF ( HqlFw:isMainWindowDefined() )
      hqlThrow( hqlErrorNew( 7005, PROCNAME() ) )
   ENDIF

   ::QMainWindow:init( ... )
   ::setAttribute( Qt_WA_DeleteOnClose )

   ::__hqlAssignObjectName( cName )
   ::__hqlConnect()

RETURN self

/*!

 \brief Activate (show/rise) form
 \param(IN)
 \return numeric

*/
METHOD hqlActivate() CLASS hql_mainWindow

   IF ( !::lHqlActivated )
      ::__hqlWinPositioning()
   ENDIF

   ::show()

   IF ( !::lHqlActivated .AND. hb_IsObject( ::oHqlSplashScreen ) )
      ::oHqlSplashScreen:finish( Self )
   ENDIF

   ::raise()

   ::activateWindow()

   IF ( ::lHqlActivated )
      HqlFw:processEvents( /*nFlags*/, /*nMsecs*/ )
   ELSE
      ::lHqlActivated := .T.
      ::nHqlReleaseCode := HqlFw:QtApplication():exec()
   ENDIF

RETURN ::nHqlReleaseCode

/*!

 \brief close all childrens (window) and toplevelWidgets but leave hqlMainWindow alive
 \param[in] none
 \return Self

*/
METHOD hqlCloseAll() CLASS hql_mainWindow

   LOCAL oList
   LOCAL nAt

   // send close signal to all Hql *window* childrens before; pay attention: real Qt class name used else it doesn't works :-)
   oList := ::children()
   FOR nAt := (oList:count()-1) TO 0 STEP -1
      IF ( hql_IsDerived(oList:at(nAt), "QMainWindow") .OR. hql_IsDerived(oList:at(nAt), "QDialog") )
         oList:at(nAt):close()
      ENDIF
   NEXT nAt

   // send close signal to all toplevelwidget BUT discards hql_mainWindow
   oList := HqlFw:QtApplication:topLevelWidgets()
   FOR nAt := (oList:count()-1) TO 0 STEP -1
      IF ( !hql_IsDerived(oList:at(nAt), "hql_mainWindow") )
         // next test is required because "QMenu" are always topLevelWidgets (see Qt docs QMenu)
         IF ( hql_IsDerived(oList:at(nAt), "QWidget") .AND. oList:at(nAt):isWindow() .AND. !hql_IsDerived(oList:at(nAt), "QMenu") )
            oList:at(nAt):close()
         ENDIF
      ENDIF
   NEXT nAt

RETURN Self

// ==================== PROTECTED section ====================

// ==================== SLOTS/EVENTS section ====================

// ==================== HIDDEN section ====================
