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

 \brief Returns a new hql_modalWindow object instance

*/
FUNCTION hqlModalWindow( ... )
RETURN hql_modalWindow():new( ... )

/*!

 \brief define hql_modalWindow class

*/
CLASS hql_modalWindow INHERIT hb_QMainWindow, hql_abs0020

   EXPORTED:
   METHOD init
   METHOD hqlActivate
   METHOD hqlRelease

   PROTECTED:

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance for given [name], [Qt flags]
 \param(IN) string, ...
 \return self

*/
METHOD init( cName, ... ) CLASS hql_modalWindow

   ::QMainWindow:init( ... )
   ::setAttribute( Qt_WA_DeleteOnClose )

   IF ( !::__hqlHasParent() )
      ::__hqlNestedWithSetParent()
   ENDIF
   IF ( ::__hqlHasParent() )
      ::setWindowModality( Qt_WindowModal )
   ELSE
      ::setWindowModality( Qt_ApplicationModal )
   ENDIF
   // Hql assume MAIN EVENT LOOP is running IF hqlMainWindow is defined so, I need a local event loop
   IF ( HqlFw:isMainWindowDefined() )
      ::oHqlEventLoop := QEventLoop( Self )
   ENDIF

   ::__hqlAssignObjectName( cName )
   ::__hqlConnect()

RETURN self

/*!

 \brief Activate (show/rise) form
 \param(IN)
 \return numeric

*/
METHOD hqlActivate() CLASS hql_modalWindow

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
      IF ( ::__hqlHasEventLoop() )
         ::oHqlEventLoop:processEvents()
      ELSE
         HqlFw:processEvents( /*nFlags*/, /*nMsecs*/ )
      ENDIF
   ELSE
      ::lHqlActivated := .T.
      IF ( ::__hqlHasEventLoop() )
         ::nHqlReleaseCode := ::oHqlEventLoop:exec()
      ELSE
         ::nHqlReleaseCode := HqlFw:QtApplication():exec()
      ENDIF
   ENDIF

RETURN ::nHqlReleaseCode

/*!

 \brief Release form
 \param(IN) numeric
 \return self

*/
METHOD hqlRelease( nExit ) CLASS hql_modalWindow
   ::nHqlReleaseCode := hb_DefaultValue(nExit, 0)
   IF ( ::__hqlHasEventLoop() )
      ::oHqlEventLoop:exit( ::nHqlReleaseCode )
      ::close()
   ELSE
      ::close()
   ENDIF
RETURN self

// ==================== PROTECTED section ====================

// ==================== SLOTS/EVENTS section ====================

// ==================== HIDDEN section ====================
