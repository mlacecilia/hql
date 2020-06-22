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
#include "cardgames.ch"

FUNCTION getBackImage( nBack )
   LOCAL cBaseName, cDir

   nBack := hb_DefaultValue(nBack, -1) // to force numeric but out of range

   SWITCH nBack
   CASE BACK_1
   CASE BACK_2
   CASE BACK_3
   CASE BACK_4
   CASE BACK_5
   CASE BACK_6
      cBaseName := "back" + STRZERO( nBack, 1, 0 )
      EXIT
   OTHERWISE
      cBaseName := "back" + STRZERO( BACK_1, 1, 0 )
   ENDSWITCH

   cDir := hb_DirSepAdd( hqlSconfig():get("cardsdir", ".") )
   cBaseName := hb_FnameMerge( cDir, cBaseName, "png" )

RETURN cBaseName

FUNCTION getFrontImage( nSuit, nRank )
   LOCAL cBaseName, lValid, cDir

   nSuit := hb_DefaultValue(nSuit, -1) // to force numeric but out of range
   nRank := hb_DefaultValue(nRank, -1) // to force numeric but out of range

   SWITCH nSuit
   CASE SUIT_HEARTS
   CASE SUIT_DIAMONDS
   CASE SUIT_CLUBS
   CASE SUIT_SPADES
      cBaseName := STRZERO( nSuit, 1, 0 )
      lValid := .T.
      EXIT
   OTHERWISE
      cBaseName := ""
      lValid := .F.
   ENDSWITCH

   IF ( lValid )
      SWITCH nRank
      CASE RANK_ACE
      CASE RANK_2
      CASE RANK_3
      CASE RANK_4
      CASE RANK_5
      CASE RANK_6
      CASE RANK_7
      CASE RANK_8
      CASE RANK_9
      CASE RANK_10
      CASE RANK_JACK
      CASE RANK_QUEEN
      CASE RANK_KING
         cBaseName += STRZERO( nRank, 2, 0 )
         lValid := .T.
         EXIT
      OTHERWISE
         cBaseName := ""
         lValid := .F.
      ENDSWITCH
   ENDIF

   cDir := hb_DirSepAdd( hqlSconfig():get("cardsdir", ".") )
   IF ( lValid )
      cBaseName := hb_FnameMerge( cDir, cBaseName, "png" )
   ELSE
      cBaseName := hb_FnameMerge( cDir, "nullcard", "png" )
   ENDIF

RETURN cBaseName
