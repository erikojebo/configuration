; To run his script you need to have Autohotkey_L

; Maybe add condition that scroll lock should be on to enable the shortcuts?
; That would enable to rebind even the most standard Windows controls since
; it could easily be switched off, for example when pair programming

; Combine this script with KeyTweak to remap caps lock to the Right control
; button. This allows for using the left control button to access standard
; Windows hotkeys, such as copy and paste, while still having the option
; to add emacs style hotkeys, such as ctrl-a => home

; Increase the maximum allowed commands per time interval to allow
; holding CapsLock+n for example
#MaxHotkeysPerInterval 200

; Use 'contains' as the mach mode for window titles
SetTitleMatchMode, 2

; Bind alternative hotkeys to turn caps lock on and off, since the caps lock
; key is rebound to RCtrl
RCtrl & Tab::
capsLockIsOn := GetKeyState("CapsLock", "T")
if (capsLockIsOn)
{
  SetCapsLockState, off
}
else
{
  SetCapsLockState, on
}
return

;; Hotkeys enabled in all applications except those with emacs like keybindings
#If !IsLinuxApplicationActive()
RCtrl & n::Send {Down}
RCtrl & p::Send {Up}
RCtrl & a::Send {Home}
RCtrl & e::Send {End}
RCtrl & b::Send {Left}
RCtrl & f::Send {Right}

; Delete char/word
RCtrl & d::Send {Del}
!d::Send ^{Del}

; Alt + f/b => Back/forward word
!f::Send ^{Right}
!b::Send ^{Left}

+!f::Send +!{Right}
+!b::Send +!{Left}

; Caps lock + Q => Left click menu
RCtrl & q::Send +{F10}

; Scroll up/down
RCtrl & v::Send {PgDn}
!v::Send {PgUp}

; Beginning/end of document
!SC056::Send ^{Home}
+!SC056::Send ^{End}

; Escape
RCtrl & g::Send {Esc}

; Undo
RCtrl & _::Send ^z

; Copy
Alt & w::Send ^c

; Cut
RCtrl & w::Send ^x

; Paste
RCtrl & y::Send ^v

; Yank rest of line
RCtrl & k::Send +{End}^x


;; Hotkeys for only Visual Studio
#If IsVisualStudioActive()

; Incremental search / C-x-s save
RCtrl & x::
emacsSaveBegun = 1
Return

RCtrl & s::
If emacsSaveBegun = 1
{
  emacsSaveBegun = 0
  Send ^s  
}
Else
  Send ^i
Return

RCtrl & r::Send +^i

;; Hotkeys for all applications except Visual Studio and those with emacs like keybindings
#If !IsVisualStudioActive() and !IsLinuxApplicationActive()
^l::Send {Home}+{End}+{Right}^x
+^l::Send {Home}+{End}+{Right}{Del}

;; Hotkeys that should be applied to all applications
#If

; Alt + n/p => Scroll
!n::Send {WheelDown}
!p::Send {WheelUp}

RAlt & d::Send <
RAlt & f::Send >


;; Modifications for using the Kinesis Advantage
RCtrl & Enter::AltTab


;; Helper methods
IsLinuxApplicationActive()
{
  return WinActive("emacs") or WinActive("Conkeror") or WinActive("bash") or WinActive("Ubuntu")
}

IsVisualStudioActive()
{
  return WinActive("Visual Studio")
}