;;
;; Copyright @ 2025 Oslo University Hospital
;;
;; This file is part of SystoleOS.
;;
;; SystoleOS is free software: you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later version.
;;
;; SystoleOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
;; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE. See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with SystoleOS. If not, see <https://www.gnu.org/licenses/>.
;;

(define-module (systole licenses)
  #:export (slul
            openhaptics-eula
            3ds-touch-driver-eula))

;; Guix does not export the license record constructor.
(define license (@@ (guix licenses) license))

(define slul
  (license "Simple Library Usage License"
           "https://svn.code.sf.net/p/teem/code/teem/trunk/LICENSE.txt"
           "A simple permissive license for library usage."))

;; 3D Systems proprietary EULA that ships with OpenHaptics 3.4 Developer
;; Edition and the Touch device driver.  The SDK and driver are freely
;; downloadable from 3D Systems' public S3 bucket but may only be used
;; under the terms printed in their respective README_INSTALL files.
;; These packages are non-free; they are provided in this channel to
;; make the JHU sawSensablePhantom stack buildable against the 3D
;; Systems Touch haptic device.
(define openhaptics-eula
  (license "OpenHaptics Developer Edition EULA"
           "https://www.3dsystems.com/software/openhaptics-developer-license"
           "3D Systems proprietary license for the OpenHaptics SDK."))

(define 3ds-touch-driver-eula
  (license "3D Systems Touch Device Driver EULA"
           "https://www.3dsystems.com/haptics-devices/touch"
           "3D Systems proprietary license for the Touch USB driver."))
