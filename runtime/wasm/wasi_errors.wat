;; Wasm_of_ocaml runtime support
;; http://www.ocsigen.org/js_of_ocaml/
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, with linking exception;
;; either version 2.1 of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

(module
(@if $wasi
(@then
   (type $block (array (mut (ref eq))))
   (type $bytes (array (mut i8)))
   (global (export "error_messages") (ref $block)
      (array.new_fixed $block 77
         (@string "Success")
         (@string "Argument list too long")
         (@string "Permission denied")
         (@string "Address in use")
         (@string "Address not available")
         (@string "Address family not supported")
         (@string "Resource unavailable, or operation would block")
         (@string "Connection already in progress")
         (@string "Bad file descriptor")
         (@string "Bad message")
         (@string "Device or resource busy")
         (@string "Operation canceled")
         (@string "No child processes")
         (@string "Connection aborted")
         (@string "Connection refused")
         (@string "Connection reset")
         (@string "Resource deadlock would occur")
         (@string "Destination address required")
         (@string "Mathematics argument out of domain of function")
         (@string "Reserved")
         (@string "File exists")
         (@string "Bad address")
         (@string "File too large")
         (@string "Host is unreachable")
         (@string "Identifier removed")
         (@string "Illegal byte sequence")
         (@string "Operation in progress")
         (@string "Interrupted function")
         (@string "Invalid argument")
         (@string "I/O error")
         (@string "Socket is connected")
         (@string "Is a directory")
         (@string "Too many levels of symbolic links")
         (@string "File descriptor value too large")
         (@string "Too many links")
         (@string "Message too large")
         (@string "Reserved")
         (@string "Filename too long")
         (@string "Network is down")
         (@string "Connection aborted by network")
         (@string "Network unreachable")
         (@string "Too many files open in system")
         (@string "No buffer space available")
         (@string "No such device")
         (@string "No such file or directory")
         (@string "Executable file format error")
         (@string "No locks available")
         (@string "Reserved")
         (@string "Not enough space")
         (@string "No message of the desired type")
         (@string "Protocol not available")
         (@string "No space left on device")
         (@string "Function not supported")
         (@string "The socket is not connected")
         (@string "Not a directory or a symbolic link to a directory")
         (@string "Directory not empty")
         (@string "State not recoverable")
         (@string "Not a socket")
         (@string "Not supported, or operation not supported on socket")
         (@string "Inappropriate I/O control operation")
         (@string "No such device or address")
         (@string "Value too large to be stored in data type")
         (@string "Previous owner died")
         (@string "Operation not permitted")
         (@string "Broken pipe")
         (@string "Protocol error")
         (@string "Protocol not supported")
         (@string "Protocol wrong type for socket")
         (@string "Result too large")
         (@string "Read-only file system")
         (@string "Invalid seek")
         (@string "No such process")
         (@string "Reserved")
         (@string "Connection timed out")
         (@string "Text file busy")
         (@string "Cross-device link")
         (@string "Capabilities insufficient")))
))
)
