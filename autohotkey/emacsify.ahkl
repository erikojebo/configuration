; To run his script you need to have Autohotkey_L

; Maybe add conditoin that scroll lock should be on to enable the shortcuts?
; That would enable to rebind even the most standard Windows controls since
; it could easily be switched off, for example when pair programming

; Combine this script with KeyTweak to remap caps lock to the Right control
; button. This allows for using the left control button to access standard
; Windows hotkeys, such as copy and paste, while still having the option
; to add emacs style hotkeys, such as ctrl-a => home

; Increase the maximum allowed commands per time interval to allow
; holding CapsLock+n for example
#MaxHotkeysPerInterval 200

; Use 'conains' as the mach mode for window titles
SetTitleMatchMode, 2

; Bind alternative hotkeys to turn caps lock on and off, since the caps lock
; key is rebound to RCtrl
ScrollLock::CapsLock
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

RCtrl & k::Send +{End}^x
RCtrl & y::Send ^v

; Scroll up/down
RCtrl & v::Send {PgDn}
!v::Send {PgUp}

; Beginning/end of document
!SC056::Send ^{Home}
+!SC056::Send ^{End}

;; Hotkeys for all applications except Visual Studio and those with emacs like keybindings
#If !IsVisualStudioActive() and !IsLinuxApplicationActive()
^l::Send {Home}+{End}+{Right}^x
+^l::Send {Home}+{End}+{Right}{Del}


;; Hotkeys that should be applied to all applications
#If

; Alt + n/p => Scroll
!n::Send {WheelDown}
!p::Send {WheelUp}


;; Helper methods
IsLinuxApplicationActive() 
{
  return WinActive("emacs") or WinActive("Conkeror") or WinActive("bash")
}

IsVisualStudioActive()
{
  return WinActive("Visual Studio")
}