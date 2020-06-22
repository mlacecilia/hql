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
#include "hqlinclude.ch"

/*!

 \brief [public_function] creates hql_movie object
 \param[in] string name OR NIL (MANDATORY)
 \param[in] other Qt arguments
 \return hql_movie object

*/
FUNCTION moviePlayer( ... )
RETURN movie_player():new( ... )

/*!

 \brief movie_player class definition
   from http://doc.qt.io/qt-5/qtwidgets-widgets-movie-movieplayer-cpp.html

*/
CREATE CLASS movie_player INHERIT hql_widget

   EXPORTED:
   METHOD init
   METHOD openFile

   PROTECTED:
   DATA currentMovieDirectory             INIT ""
   DATA movieLabel                        INIT NIL
   DATA movie                             INIT NIL
   DATA openButton                        INIT NIL
   DATA playButton                        INIT NIL
   DATA pauseButton                       INIT NIL
   DATA stopButton                        INIT NIL
   DATA fitCheckBox                       INIT NIL
   DATA frameSlider                       INIT NIL
   DATA speedSpinBox                      INIT NIL
   DATA frameLabel                        INIT NIL
   DATA speedLabel                        INIT NIL
   METHOD __createControls
   METHOD __createButtons
   METHOD __open
   METHOD __goToFrame
   METHOD __fitToWindow
   METHOD __updateButtons
   METHOD __updateFrameSlider

   HIDDEN:

END CLASS

/*!

 \brief initialize object instance
 \param[in] string name OR NIL (MANDATORY)
 \param[in] other Qt arguments
 \return SELF

*/
METHOD init( ... ) CLASS movie_player
   LOCAL oLayout

   ::hql_widget:init( ... )
   oLayout := HqlVBoxLayout( /*name*/ )    // QVBoxLayout()
   ::setLayout( oLayout )

   ::movie := hqlMovie( /*name*/, Self )
   ::movie:setCacheMode( QMovie_CacheAll )

   WITH OBJECT ::movieLabel := hqlLabel( /*name*/, self )
      :hqlAddMeToLayout( oLayout )
      :setText( "No movie loaded" )
      :setAlignment( Qt_AlignCenter )
      :setSizePolicy( QSizePolicy_Ignored, QSizePolicy_Ignored )
      :setBackgroundRole( QPalette_Dark )
      :setAutoFillBackground( .T. )
   END WITH

   ::currentMovieDirectory := "movies"

   ::movie:hqlOnFrameChanged( { || ::__updateFrameSlider() } )
   ::movie:hqlOnSateChanged( { || ::__updateButtons() } )

   oLayout:addLayout( ::__createControls() )
   oLayout:addLayout( ::__createButtons() )

   ::__updateFrameSlider()

   ::__updateButtons()

//   ::setWindowTitle( "Movie Player" )

   ::setMinimumSize( QSize(600, 600) )

RETURN Self

/*!

 \brief
 \param[in]
 \return

*/
METHOD openFile( filename ) CLASS movie_player

    hb_Default( @filename, "" )

    ::currentMovieDirectory := QFileInfo( fileName ):path()

    ::movie:stop()
    ::movieLabel:setMovie( ::movie )
    ::movie:setFileName( fileName )
    ::movie:start()

    ::__updateFrameSlider()
    ::__updateButtons()

RETURN Self

// ==================== PROTECTED section ====================

/*!

 \brief
 \param[in]
 \return

*/
METHOD __open() CLASS movie_player

   LOCAL fileName := QFileDialog():getOpenFileName( Self, "Open a Movie", ::currentMovieDirectory )

   IF ( !EMPTY( fileName ) )
      ::openFile( fileName )
   ENDIF

RETURN NIL

/*!

 \brief
 \param[in]
 \return

*/
METHOD __goToFrame( frame ) CLASS movie_player
   ::movie:jumpToFrame( frame )
RETURN NIL

/*!

 \brief
 \param[in]
 \return

*/
METHOD __fitToWindow() CLASS movie_player
   ::movieLabel:setScaledContents( ::fitCheckBox:isChecked() )
RETURN NIL

/*!

 \brief
 \param[in]
 \return

*/
METHOD __updateFrameSlider() CLASS movie_player

   LOCAL hasFrames := ( ::movie:currentFrameNumber() >= 0 )

   IF ( hasFrames )
      IF ( ::movie:frameCount() > 0 )
         ::frameSlider:setMaximum( ::movie:frameCount() - 1 )
      ELSE
         IF ( ::movie:currentFrameNumber() > ::frameSlider:maximum() )
            ::frameSlider:setMaximum( ::movie:currentFrameNumber() )
         ENDIF
         ::frameSlider:setValue( ::movie:currentFrameNumber() )
      ENDIF
   ELSE
      ::frameSlider:setMaximum( 0 )
   ENDIF

   ::frameLabel:setEnabled( hasFrames )
   ::frameSlider:setEnabled( hasFrames )

RETURN NIL

/*!

 \brief
 \param[in]
 \return

*/
METHOD __updateButtons() CLASS movie_player

   ::playButton:setEnabled( ::movie:isValid() .AND. ::movie:frameCount() != 1 .AND. ::movie:state() == QMovie_NotRunning )
   ::pauseButton:setEnabled( ::movie:state() != QMovie_NotRunning )
   ::pauseButton:setChecked( ::movie:state() == QMovie_Paused )
   ::stopButton:setEnabled( ::movie:state() != QMovie_NotRunning )

RETURN NIL

/*!

 \brief
 \param[in]
 \return

*/
METHOD __createControls() CLASS movie_player
   LOCAL oLayout

   ::fitCheckBox := hqlCheckBox( /*name*/, "Fit to Window", Self )   // QCheckBox( "Fit to Window" )
   ::fitCheckBox:hqlOnClicked( { || ::__fitToWindow() } )

   ::frameLabel := hqlLabel( /*name*/, "Current frame", self )      //  QLabel( "Current frame:" )

   ::frameSlider := hqlSlider( /*name*/, Qt_Horizontal, self )       // QSlider( Qt_Horizontal )
   ::frameSlider:setTickPosition( QSlider_TicksBelow )
   ::frameSlider:setTickInterval( 10 )
   ::frameSlider:hqlOnValueChanged( { |nInt| ::__goToFrame( nInt) } )

   ::speedLabel := hqlLabel( /*name*/, "Speed:", self )              // QLabel( "Speed:" )

   ::speedSpinBox := hqlSpinBox( /*name*/, self )                     // QSpinBox()
   ::speedSpinBox:setRange( 1, 9999 )
   ::speedSpinBox:setValue( 100 )
   ::speedSpinBox:setSuffix( "%" )
   ::speedSpinBox:hqlOnValueChanged( { |nInt| ::movie:setSpeed(nInt) } )

   oLayout := hqlGridLayout( /*name*/ )               // QGridLayout()
   oLayout:addWidget( ::fitCheckBox, 0, 0, 1, 2 )
   oLayout:addWidget( ::frameLabel, 1, 0 )
   oLayout:addWidget( ::frameSlider, 1, 1, 1, 2 )
   oLayout:addWidget( ::speedLabel, 2, 0 )
   oLayout:addWidget( ::speedSpinBox, 2, 1 )

RETURN oLayout

/*!

 \brief
 \param[in]
 \return

*/
METHOD __createButtons() CLASS movie_player
   LOCAL iconSize := QSize( 36, 36 )
   LOCAL oLayout

   ::openButton := hqlToolButton( /*name*/ ) // QToolButton()
   ::openButton:setIcon( ::style():standardIcon( QStyle_SP_DialogOpenButton ) )
   ::openButton:setIconSize( iconSize )
   ::openButton:setToolTip( "Open File" )
   ::openButton:hqlOnClicked( { || ::__open() } )

   ::playButton := hqlToolButton( /*name*/ ) // QToolButton()
   ::playButton:setIcon( ::style():standardIcon( QStyle_SP_MediaPlay ) )
   ::playButton:setIconSize( iconSize )
   ::playButton:setToolTip( "Play" )
   ::playButton:hqlOnClicked( { || ::movie:start() } )

   ::pauseButton := hqlToolButton( /*name*/ ) // QToolButton()
   ::pauseButton:setCheckable( .T. )
   ::pauseButton:setIcon( ::style():standardIcon( QStyle_SP_MediaPause ) )
   ::pauseButton:setIconSize( iconSize )
   ::pauseButton:setToolTip( "Pause" )
   ::pauseButton:hqlOnToggled({ |bool| ::movie:setPaused(bool) } )

   ::stopButton := hqlToolButton( /*name*/ ) // QToolButton()
   ::stopButton:setIcon( ::style():standardIcon( QStyle_SP_MediaStop ) )
   ::stopButton:setIconSize( iconSize )
   ::stopButton:setToolTip( "Stop" )
   ::stopButton:hqlOnClicked( { || ::movie:stop() } )

   oLayout := hqlHBoxLayout( /*name*/ ) // QHBoxLayout()
   oLayout:addStretch()
   oLayout:addWidget( ::openButton )
   oLayout:addWidget( ::playButton )
   oLayout:addWidget( ::pauseButton )
   oLayout:addWidget( ::stopButton )
   oLayout:addStretch()

RETURN oLayout

// ==================== EVENT & SIGNAL slots section ====================

// ==================== HIDDEN section ====================
