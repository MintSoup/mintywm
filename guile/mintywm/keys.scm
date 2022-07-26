;; This file is part of mintywm.
;;
;; mintywm is free software: you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; mintywm is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
;; PARTICULAR PURPOSE. See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; mintywm. If not, see <https://www.gnu.org/licenses/>.

;; Commentary

;; This file contains code that handles keyboard input.

;; Code

(define-module (mintywm keys)
	#:use-module (srfi srfi-1)
	#:use-module (srfi srfi-17)
	#:use-module (al plists)
	#:use-module (mintywm core))

(define-public before-keypress-functions '())
(define-public after-keypress-functions '())

(define key-binds '())

(define (make-key-plist modifiers keycode function . args)
	`(#:modifiers ,modifiers #:keycode ,keycode #:function ,function #:args ,args))


(define-public ShiftMask		(ash 1 0))
(define-public LockMask		(ash 1 1))
(define-public ControlMask		(ash 1 2))
(define-public Mod1Mask		(ash 1 3))
(define-public Mod2Mask		(ash 1 4))
(define-public Mod3Mask		(ash 1 5))
(define-public Mod4Mask		(ash 1 6))
(define-public Mod5Mask		(ash 1 7))

(define-public XKCD_Escape 9)
(define-public XKCD_1 10)
(define-public XKCD_2 11)
(define-public XKCD_3 12)
(define-public XKCD_4 13)
(define-public XKCD_5 14)
(define-public XKCD_6 15)
(define-public XKCD_7 16)
(define-public XKCD_8 17)
(define-public XKCD_9 18)
(define-public XKCD_0 19)
(define-public XKCD_minus 20)
(define-public XKCD_equal 21)
(define-public XKCD_BackSpace 22)
(define-public XKCD_Tab 23)
(define-public XKCD_q 24)
(define-public XKCD_w 25)
(define-public XKCD_e 26)
(define-public XKCD_r 27)
(define-public XKCD_t 28)
(define-public XKCD_y 29)
(define-public XKCD_u 30)
(define-public XKCD_i 31)
(define-public XKCD_o 32)
(define-public XKCD_p 33)
(define-public XKCD_bracketleft 34)
(define-public XKCD_bracketright 35)
(define-public XKCD_Return 36)
(define-public XKCD_Control_L 37)
(define-public XKCD_a 38)
(define-public XKCD_s 39)
(define-public XKCD_d 40)
(define-public XKCD_f 41)
(define-public XKCD_g 42)
(define-public XKCD_h 43)
(define-public XKCD_j 44)
(define-public XKCD_k 45)
(define-public XKCD_l 46)
(define-public XKCD_semicolon 47)
(define-public XKCD_apostrophe 48)
(define-public XKCD_grave 49)
(define-public XKCD_Shift_L 50)
(define-public XKCD_backslash 51)
(define-public XKCD_z 52)
(define-public XKCD_x 53)
(define-public XKCD_c 54)
(define-public XKCD_v 55)
(define-public XKCD_b 56)
(define-public XKCD_n 57)
(define-public XKCD_m 58)
(define-public XKCD_comma 59)
(define-public XKCD_period 60)
(define-public XKCD_slash 61)
(define-public XKCD_Shift_R 62)
(define-public XKCD_KP_Multiply 63)
(define-public XKCD_Alt_L 64)
(define-public XKCD_space 65)
(define-public XKCD_Caps_Lock 66)
(define-public XKCD_F1 67)
(define-public XKCD_F2 68)
(define-public XKCD_F3 69)
(define-public XKCD_F4 70)
(define-public XKCD_F5 71)
(define-public XKCD_F6 72)
(define-public XKCD_F7 73)
(define-public XKCD_F8 74)
(define-public XKCD_F9 75)
(define-public XKCD_F10 76)
(define-public XKCD_Num_Lock 77)
(define-public XKCD_Scroll_Lock 78)
(define-public XKCD_KP_Home 79)
(define-public XKCD_KP_Up 80)
(define-public XKCD_KP_Prior 81)
(define-public XKCD_KP_Subtract 82)
(define-public XKCD_KP_Left 83)
(define-public XKCD_KP_Begin 84)
(define-public XKCD_KP_Right 85)
(define-public XKCD_KP_Add 86)
(define-public XKCD_KP_End 87)
(define-public XKCD_KP_Down 88)
(define-public XKCD_KP_Next 89)
(define-public XKCD_KP_Insert 90)
(define-public XKCD_KP_Delete 91)
(define-public XKCD_ISO_Level3_Shift 92)
(define-public XKCD_less 94)
(define-public XKCD_F11 95)
(define-public XKCD_F12 96)
(define-public XKCD_Katakana 98)
(define-public XKCD_Hiragana 99)
(define-public XKCD_Henkan_Mode 100)
(define-public XKCD_Hiragana_Katakana 101)
(define-public XKCD_Muhenkan 102)
(define-public XKCD_KP_Enter 104)
(define-public XKCD_Control_R 105)
(define-public XKCD_KP_Divide 106)
(define-public XKCD_Print 107)
(define-public XKCD_Alt_R 108)
(define-public XKCD_Linefeed 109)
(define-public XKCD_Home 110)
(define-public XKCD_Up 111)
(define-public XKCD_Prior 112)
(define-public XKCD_Left 113)
(define-public XKCD_Right 114)
(define-public XKCD_End 115)
(define-public XKCD_Down 116)
(define-public XKCD_Next 117)
(define-public XKCD_Insert 118)
(define-public XKCD_Delete 119)
(define-public XKCD_XF86AudioMute 121)
(define-public XKCD_XF86AudioLowerVolume 122)
(define-public XKCD_XF86AudioRaiseVolume 123)
(define-public XKCD_XF86PowerOff 124)
(define-public XKCD_KP_Equal 125)
(define-public XKCD_plusminus 126)
(define-public XKCD_Pause 127)
(define-public XKCD_XF86LaunchA 128)
(define-public XKCD_KP_Decimal 129)
(define-public XKCD_Hangul 130)
(define-public XKCD_Hangul_Hanja 131)
(define-public XKCD_Super_L 133)
(define-public XKCD_Super_R 134)
(define-public XKCD_Menu 135)
(define-public XKCD_Cancel 136)
(define-public XKCD_SunProps 138)
(define-public XKCD_Undo 139)
(define-public XKCD_SunFront 140)
(define-public XKCD_XF86Copy 141)
(define-public XKCD_XF86Open 142)
(define-public XKCD_XF86Paste 143)
(define-public XKCD_Find 144)
(define-public XKCD_XF86Cut 145)
(define-public XKCD_Help 146)
(define-public XKCD_XF86MenuKB 147)
(define-public XKCD_XF86Calculator 148)
(define-public XKCD_XF86Sleep 150)
(define-public XKCD_XF86WakeUp 151)
(define-public XKCD_XF86Explorer 152)
(define-public XKCD_XF86Send 153)
(define-public XKCD_XF86Xfer 155)
(define-public XKCD_XF86Launch1 156)
(define-public XKCD_XF86Launch2 157)
(define-public XKCD_XF86WWW 158)
(define-public XKCD_XF86DOS 159)
(define-public XKCD_XF86ScreenSaver 160)
(define-public XKCD_XF86RotateWindows 161)
(define-public XKCD_XF86TaskPane 162)
(define-public XKCD_XF86Mail 163)
(define-public XKCD_XF86Favorites 164)
(define-public XKCD_XF86MyComputer 165)
(define-public XKCD_XF86Back 166)
(define-public XKCD_XF86Forward 167)
(define-public XKCD_XF86Eject 169)
(define-public XKCD_XF86AudioNext 171)
(define-public XKCD_XF86AudioPlay 172)
(define-public XKCD_XF86AudioPrev 173)
(define-public XKCD_XF86AudioStop 174)
(define-public XKCD_XF86AudioRecord 175)
(define-public XKCD_XF86AudioRewind 176)
(define-public XKCD_XF86Phone 177)
(define-public XKCD_XF86HomePage 180)
(define-public XKCD_XF86Reload 181)
(define-public XKCD_XF86Close 182)
(define-public XKCD_XF86ScrollUp 185)
(define-public XKCD_XF86ScrollDown 186)
(define-public XKCD_parenleft 187)
(define-public XKCD_parenright 188)
(define-public XKCD_XF86New 189)
(define-public XKCD_XF86Launch5 192)
(define-public XKCD_XF86Launch6 193)
(define-public XKCD_XF86Launch7 194)
(define-public XKCD_XF86Launch8 195)
(define-public XKCD_XF86Launch9 196)
(define-public XKCD_XF86AudioMicMute 198)
(define-public XKCD_XF86TouchpadToggle 199)
(define-public XKCD_XF86TouchpadOn 200)
(define-public XKCD_XF86TouchpadOff 201)
(define-public XKCD_Mode_switch 203)
(define-public XKCD_NoSymbol 204)
(define-public XKCD_XF86AudioPause 209)
(define-public XKCD_XF86Launch3 210)
(define-public XKCD_XF86Launch4 211)
(define-public XKCD_XF86LaunchB 212)
(define-public XKCD_XF86Suspend 213)
(define-public XKCD_XF86AudioForward 216)
(define-public XKCD_XF86WebCam 220)
(define-public XKCD_XF86AudioPreset 221)
(define-public XKCD_XF86Messenger 224)
(define-public XKCD_XF86Search 225)
(define-public XKCD_XF86Go 226)
(define-public XKCD_XF86Finance 227)
(define-public XKCD_XF86Game 228)
(define-public XKCD_XF86Shop 229)
(define-public XKCD_XF86MonBrightnessDown 232)
(define-public XKCD_XF86MonBrightnessUp 233)
(define-public XKCD_XF86AudioMedia 234)
(define-public XKCD_XF86Display 235)
(define-public XKCD_XF86KbdLightOnOff 236)
(define-public XKCD_XF86KbdBrightnessDown 237)
(define-public XKCD_XF86KbdBrightnessUp 238)
(define-public XKCD_XF86Reply 240)
(define-public XKCD_XF86MailForward 241)
(define-public XKCD_XF86Save 242)
(define-public XKCD_XF86Documents 243)
(define-public XKCD_XF86Battery 244)
(define-public XKCD_XF86Bluetooth 245)
(define-public XKCD_XF86WLAN 246)
(define-public XKCD_XF86UWB 247)
(define-public XKCD_XF86Next_VMode 249)
(define-public XKCD_XF86Prev_VMode 250)
(define-public XKCD_XF86MonBrightnessCycle 251)
(define-public XKCD_XF86BrightnessAuto 252)
(define-public XKCD_XF86DisplayOff 253)
(define-public XKCD_XF86WWAN 254)
(define-public XKCD_XF86RFKill 255)

(define-syntax match-keybind
	(syntax-rules  ()
		[(_ pl mods kc t f ...)
		 (if (and
			  (= (plist-get pl #:modifiers) mods)
			  (= (plist-get pl #:keycode) kc))
			 t
			 f ...)]))

(define-public (bind-key modifiers keycode function . args)
	(set! key-binds
		  (cons (apply make-key-plist modifiers keycode function args)
				key-binds))
	(grab-key modifiers keycode))


(define-public (unbind-key modifiers keycode)
	(set! key-binds
		  (filter!
		   (lambda (pl)
			   (match-keybind pl modifiers keycode
				   (begin
					   (ungrab-key modifiers keycode)
					   #f)
				   #t))
		   key-binds)))

(define (key-press keycode modifiers)
	(for-each (lambda (fn) (apply fn keycode modifiers))
			  before-keypress-functions)

	(for-each
	 (lambda (pl)
		 (match-keybind pl modifiers keycode
			 (apply (plist-get pl #:function)
					(plist-get pl #:args))))
	 key-binds)

	(for-each (lambda (fn) (apply fn keycode modifiers))
			  after-keypress-functions))
