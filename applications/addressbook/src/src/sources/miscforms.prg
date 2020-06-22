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
#include "addrbook.ch"

/*!

 \brief

*/
CREATE CLASS basic_form INHERIT basic_program

   EXPORTED:
   METHOD activate
   METHOD hasWindow
   METHOD parentWindow
   METHOD prepareUi
   METHOD refreshUi
   METHOD window                          INLINE ::oWindow

   PROTECTED:
   DATA oWindow                           INIT NIL
   METHOD __createWin
   METHOD __onActivate
   METHOD __onClose

END CLASS

METHOD activate() CLASS basic_form

   IF ::hasWindow()
      RETURN ::oWindow:hqlActivate()
   ENDIF

RETURN 0

METHOD hasWindow() CLASS basic_form
RETURN hb_IsObject( ::oWindow )

METHOD parentWindow() CLASS basic_form
   IF ::hasParent() .AND. __objHasMsg( ::parent(), "window" )
      RETURN ::parent:window()
   END IF
RETURN NIL

METHOD prepareUi() CLASS basic_form
   ::__createWin()
RETURN Self

METHOD refreshUi() CLASS basic_form
RETURN Self

METHOD __createWin() CLASS basic_form
RETURN NIL

METHOD __onActivate() CLASS basic_form
RETURN NIL

METHOD __onClose() CLASS basic_form
RETURN NIL

//////////////////////////////////////// \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

/*!

 \brief

*/
CREATE CLASS basic_program

   EXPORTED:
   METHOD hasParent
   METHOD parent
   METHOD setParent

   PROTECTED:
   DATA oParent                           INIT NIL

END CLASS

METHOD hasParent() CLASS basic_program
RETURN hb_IsObject( ::oParent )

METHOD parent() CLASS basic_program
RETURN ::oParent

METHOD setParent( ... ) CLASS basic_program

   IF hb_IsObject( hb_Pvalue(1) )
      ::oParent:= hb_Pvalue(1)
   ENDIF

RETURN Self
