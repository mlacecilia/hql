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
#include "cardgames.ch"

/*!

 \brief Returns a new g_card object instance

*/
FUNCTION gCard( ... )
RETURN g_card():new( ... )

/*!

 \brief define g_card class

*/
CLASS g_card

   EXPORTED:
   METHOD init
   METHOD back                            INLINE ::nBack
   METHOD backImage                       INLINE ::cBackImage
   METHOD face                            INLINE ::nFace
   METHOD frontImage                      INLINE ::cFrontImage
   METHOD imageShown
   METHOD isFaceUp                        INLINE ( ::nFace == FACE_FRONT )
   METHOD isValid                         INLINE ( ::nSuit > 0 .AND. ::nRank > 0 )
   METHOD flip
   METHOD rank                            INLINE ::nRank
   METHOD suit                            INLINE ::nSuit
   METHOD setFace
   METHOD setValue
   METHOD value                           INLINE ::nValue
   METHOD weight

   PROTECTED:
   VAR cBackImage                         INIT ""
   VAR cFrontImage                        INIT ""
   VAR nBack                              INIT BACK_1
   VAR nFace                              INIT FACE_BACK
   VAR nRank                              INIT 0
   VAR nSuit                              INIT 0
   VAR nValue                             INIT 0
   METHOD __setBack
   METHOD __setImages
   METHOD __setRank
   METHOD __setSuit

   HIDDEN:

   METHOD __h_parsingInit
ENDCLASS

/*!

 \brief initialize object instance for given [suit], [rank], [value]
 \param(IN) numeric, numeric, numeric
 \return self

*/
METHOD init( ... ) CLASS g_card
   ::__h_parsingInit( ... )
RETURN self

/*!

 \brief flip card; face up or back
 \param(IN)
 \return self

*/
METHOD flip() CLASS g_card
   ::nFace := IIF ( ::nFace == FACE_BACK, FACE_FRONT, FACE_BACK )
RETURN self

/*!

 \brief returns image source based on current face status
 \param(IN)
 \return string

*/
METHOD imageShown() CLASS g_card
   IF ( ::nFace == FACE_BACK )
      RETURN ::cBackImage
   ENDIF
RETURN ::cFrontImage

/*!

 \brief set face to be shown: front = .T., back := .F.
 \param(IN) bool
 \return self

*/
METHOD setFace( arg1 ) CLASS g_card
   IF ( hb_IsNumeric(arg1) .AND. (arg1 == FACE_BACK .OR. arg1 == FACE_FRONT) )
      ::nFace := arg1
   ENDIF
RETURN self

/*!

 \brief set card value based on game rules
 \param(IN) numeric
 \return self

*/
METHOD setValue( arg1 ) CLASS g_card
   IF ( hb_IsNumeric(arg1) .AND. arg1 >= 0 )
      ::nValue := arg1
   ENDIF
RETURN self

// ==================== PROTECTED section ====================

/*!

 \brief [PROTECTED] set card rank.
 \param(IN) numeric
 \return NIL

*/
METHOD __setBack( arg1 ) CLASS g_card
   IF ( hb_IsNumeric(arg1) .AND. arg1 >= BACK_MIN .AND. arg1 <= BACK_MAX )
      ::nBack := arg1
   ENDIF
RETURN NIL

/*!

 \brief [PROTECTED] set card images
 \param(IN)
 \return NIL

*/
METHOD __setImages() CLASS g_card
   ::cFrontImage := getFrontImage( ::nSuit, ::nRank )
   ::cBackImage := getBackImage( ::nBack )
RETURN NIL

/*!

 \brief [PROTECTED] set card rank.
 \param(IN) numeric
 \return NIL

*/
METHOD __setRank( arg1 ) CLASS g_card
   IF ( hb_IsNumeric(arg1) .AND. arg1 >= RANK_MIN .AND. arg1 <= RANK_MAX )
      ::nRank := arg1
   ENDIF
RETURN NIL

/*!

 \brief [PROTECTED] set card suit.
 \param(IN) numeric
 \return NIL

*/
METHOD __setSuit( arg1 ) CLASS g_card
   IF ( hb_IsNumeric(arg1) .AND. arg1 >= SUIT_MIN .AND. arg1 <= SUIT_MAX )
      ::nSuit := arg1
   ENDIF
RETURN NIL

/*!

 \brief [PROTECTED] returns card weight based on back, suit, rank. Norrmally used to sort g_hand
 \param(IN)
 \return numeric

*/
METHOD weight() CLASS g_card
   LOCAL nWeight := 0
   IF ( ::isValid() )
      nWeight := ( ::nBack * 1000 ) + ( ::nSuit * 100 ) + ::nRank
   ENDIF
RETURN nWeight

// ==================== HIDDEN section ====================

/*!

 \brief [HIDDEN] parsing init arguments
 \param(IN) ...
 \return NIL

*/
METHOD __h_parsingInit( nSuit, nRank, nBack, nValue ) CLASS g_card
   ::__setSuit( nSuit )
   ::__setRank( nRank )
   ::__setBack( nBack )
   ::setValue( nValue )
   ::__setImages()
RETURN NIL
