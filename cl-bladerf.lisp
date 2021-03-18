;;;; cl-bladeRF.lisp
;;
;;;; Copyright (c) 2021 Tichaona Kadzinga <tichaona@kadzinga.com>

(in-package #:cl-bladerf)

(define-foreign-library libbladerf
    (:unix (:or "libbladeRF.so.2" "libbladeRF.so"))  
    (t (:default "libbladeRF")))

(use-foreign-library libbladerf)
;; This is an opaque structure

(defcstruct (bladerf-device))

(defconstant LIBBLADERF_API_VERSION #x02000200)

(defcenum bladerf_backend
	:BLADERF_BACKEND_ANY
	:BLADERF_BACKEND_LINUX
	:BLADERF_BACKEND_LIBUSB
	:BLADERF_BACKEND_CYPRESS
	(:BLADERF_BACKEND_DUMMY #.100))

(defconstant BLADERF_DESCRIPTION_LENGTH 33)

(defconstant BLADERF_SERIAL_LENGTH 33)

(defcstruct bladerf_devinfo
	(backend bladerf_backend)
	(serial :char :count 33)
	(usb_bus :uint8)
	(usb_addr :uint8)
	(instance :unsigned-int)
	(manufacturer :char :count 33)
	(product :char :count 33))

(defcfun ("bladerf_open" bladerf_open) :int
  (device :pointer)
  (device_identifier :pointer))

(defcfun ("bladerf_close" bladerf_close) :void
  (device :pointer))

(defun bladerf-close (device)
  (bladerf_close (mem-ref device :pointer)))

(defcfun ("bladerf_open_with_devinfo" bladerf_open_with_devinfo) :int
  (device :pointer)
  (devinfo :pointer))

(defcfun ("bladerf_get_device_list" bladerf_get_device_list) :int
  (devices :pointer))

(defcfun ("bladerf_free_device_list" bladerf_free_device_list) :void
  (devices :pointer))

(defcfun ("bladerf_init_devinfo" bladerf_init_devinfo) :void
  (info :pointer))

(defcfun ("bladerf_get_devinfo" bladerf_get_devinfo) :int
  (dev :pointer)
  (info :pointer))

(defcfun ("bladerf_get_devinfo_from_str" bladerf_get_devinfo_from_str) :int
  (devstr :string)
  (info :pointer))

(defcfun ("bladerf_devinfo_matches" bladerf_devinfo_matches) :pointer
  (a :pointer)
  (b :pointer))

(defcfun ("bladerf_devstr_matches" bladerf_devstr_matches) :pointer
  (dev_str :string)
  (info :pointer))

(defcfun ("bladerf_backend_str" bladerf_backend_str) :string
  (backend bladerf_backend))

(defcfun ("bladerf_set_usb_reset_on_open" bladerf_set_usb_reset_on_open) :void
  (enabled :boolean))

(defun bladerf-open-device (ptr->device &optional (device-identifier ""))
  (bladerf_set_usb_reset_on_open 1)
  (with-foreign-string (cdevice-identifier device-identifier)
    (let ((status (bladerf_open ptr->device cdevice-identifier)))
      (if (< status 0)
	  (error "Failed to open device error: ~S" status)
	  (= status 0)))))


(defmacro with-bladerf-device ((device-handle device-identifier) &body body)
  (let ((ptr->device-ptr (gensym "*dev*")))
    `(let ((,ptr->device-ptr (foreign-alloc '(:pointer (:struct bladerf-device)))))
       (bladerf_set_usb_reset_on_open t)
       (with-foreign-string (cdevice-identifier ,device-identifier)
	 (if (< (bladerf_open ,ptr->device-ptr cdevice-identifier) 0)
	     (error "Failed to open device")
	     (let ((,device-handle (mem-ref ,ptr->device-ptr :pointer)))
	       (unwind-protect
		    (progn ,@body)
		 (bladerf_close ,device-handle)
		 (foreign-free ,ptr->device-ptr))))))))


(defcstruct bladerf_range
  (min :int64)
  (max :int64)
  (step :int64)
  (scale :float))

(defcstruct bladerf_serial
  (serial :char :count 33))

(defcstruct bladerf_version
  (major :uint16)
  (minor :uint16)
  (patch :uint16)
  (describe :string))

(defcenum bladerf_fpga_size
	(:BLADERF_FPGA_UNKNOWN #.0)
	(:BLADERF_FPGA_40KLE #.40)
	(:BLADERF_FPGA_115KLE #.115)
	(:BLADERF_FPGA_A4 #.49)
	(:BLADERF_FPGA_A9 #.301))

(defcenum bladerf_dev_speed
	:BLADERF_DEVICE_SPEED_UNKNOWN
	:BLADERF_DEVICE_SPEED_HIGH
	:BLADERF_DEVICE_SPEED_SUPER)

(defcenum bladerf_fpga_source
	(:BLADERF_FPGA_SOURCE_UNKNOWN #.0)
	(:BLADERF_FPGA_SOURCE_FLASH #.1)
	(:BLADERF_FPGA_SOURCE_HOST #.2))

;;deprecated New code should use bladerf_get_serial_struct instead.
;(defcfun ("bladerf_get_serial" bladerf_get_serial) :int
;  (dev :pointer)
;  (serial :string))

(defcfun ("bladerf_get_serial_struct" bladerf_get_serial_struct) :int
  (dev :pointer)
  (serial (:pointer (:struct bladerf_serial))))

(defun get-serial-struct (device)
  (let* ((serial-struct (foreign-alloc :pointer))
	 (status (bladerf_get_serial_struct (mem-ref device :pointer) (mem-ref serial-struct '(:pointer (:struct bladerf_serial)))))
	 (serial ""))
    (if (< status 0)
	(error "An error occured while trying to get serial number")
	(setf serial (foreign-string-to-lisp  (mem-aref serial-struct '(:pointer (:struct bladerf_serial))))))
    (foreign-free serial-struct)
    serial))

;;;; TODO 
(defun get-serial-struct-2 (device)
  (with-foreign-object (serial-struct :pointer)
    (let ((status (bladerf_get_serial_struct (mem-ref device :pointer) serial-struct)))
      (if (< status 0)
	  (error "Failed to get serial number error: ~S" status)
	  (foreign-string-to-lisp  (mem-aref serial-struct '(:pointer (:struct bladerf_serial))))))))

(defcfun ("bladerf_get_fpga_size" bladerf_get_fpga_size) :int
  (dev :pointer)
  (size (:pointer bladerf_fpga_size)))

;;Query a device's FPGA size
(defun get-fpga-size (ptr->device)
  (with-foreign-object (size '(:pointer bladerf_fpga_size))
    (let ((status (bladerf_get_fpga_size (mem-ref ptr->device :pointer) size)))
      (if (< status 0)
	  (error "Failed to get on-board FPGA size")
	  (mem-aref size :int)))))
  
(defcfun ("bladerf_get_flash_size" bladerf_get_flash_size) :int
  (dev :pointer)
  (size (:pointer :uint32))
  (is_guess (:pointer :bool)))

;;Query a device's Flash size
(defun get-flash-size (device)
  (with-foreign-objects ((size '(:pointer :uint32)) (is-guess '(:pointer :bool)))
    (let ((status (bladerf_get_flash_size (mem-ref device :pointer) size is-guess)))
      (if (< status 0)
	  (error "Failed to get on-board flash")
	  (if (mem-ref is-guess :bool)
	      (format t "The guessed flash size is ~a bytes" (mem-ref size :uint32))
	      (format t "The flash size is ~a bytes" (mem-ref size :uint32)))))))

(defcfun ("bladerf_fw_version" bladerf_fw_version) :int
  (dev :pointer)
  (version :pointer))

;;Query firmware version
(defun get-firmware-version (device)
  (with-foreign-object (version '(:pointer (:struct bladerf_version)))
    (let ((status (bladerf_fw_version (mem-ref device :pointer) version)))
      (if (< status 0)
	  (error "Failed to get on-board flash")
	  (with-foreign-slots ((describe) version (:struct bladerf_version))
	    describe)))))
	  
(defcfun ("bladerf_is_fpga_configured" bladerf_is_fpga_configured) :int
  (dev :pointer))

;;Check FPGA configuration status
(defun fpga-configured-p (device)
  (let ((status (bladerf_is_fpga_configured (mem-ref device :pointer))))
    (case status
      (0 nil)
      (1 t)
      (otherwise (error "Failed to check FPGA configuration status")))))

(defcfun ("bladerf_fpga_version" bladerf_fpga_version) :int
  (dev :pointer)
  (version :pointer))

;;Query FPGA version
(defun get-fpga-version (device)
  (with-foreign-object (version '(:pointer (:struct bladerf_version)))
    (let ((status (bladerf_fpga_version (mem-ref device :pointer) version)))
      (if (< status 0)
	  (error "Failed to get on-board flash")
	  (with-foreign-slots ((describe) version (:struct bladerf_version))
	    describe)))))

(defcfun ("bladerf_get_fpga_source" bladerf_get_fpga_source) :int
  (dev :pointer)
  (source :pointer))

;;Query FPGA configuration source
;;Determine whether the FPGA image was loaded from flash, or if it was
;;loaded from the host, by asking the firmware for the last-known FPGA
;;configuration source.
(defun get-fpga-source (device)
  (with-foreign-object (source '(:pointer bladerf_fpga_source))
    (let ((status (bladerf_get_fpga_source (mem-ref device :pointer) source)))
      (if (< status 0)
	  (error "Failed to get on-board FPGA size")
	  (mem-aref source 'bladerf_fpga_source)))))

(defcfun ("bladerf_device_speed" bladerf_device_speed) bladerf_dev_speed
  (dev :pointer))

;;Obtain the bus speed at which the device is operating
(defun get-device-speed (device)
  (bladerf_device_speed (mem-ref device :pointer)))

(defcfun ("bladerf_get_board_name" bladerf_get_board_name) :string
  (dev :pointer))

;;Get the board name
(defun get-board-name (device)
  (bladerf_get_board_name (mem-ref device :pointer)))

;; RX Channel Macro
(defun channel-rx (channel)
  (logior (ash channel 1) #x0))

;;TX Channel Macro
(defun channel-tx (channel)
  (logior (ash channel 1) #x1))

(defconstant BLADERF_DIRECTION_MASK #x1)

(defcenum bladerf_direction
	(:BLADERF_RX #.0)
	(:BLADERF_TX #.1))

(defcenum bladerf_channel_layout
	(:BLADERF_RX_X1 #.0)
	(:BLADERF_TX_X1 #.1)
	(:BLADERF_RX_X2 #.2)
	(:BLADERF_TX_X2 #.3))

(defcfun ("bladerf_get_channel_count" bladerf_get_channel_count) :uint
  (dev :pointer)
  (dir bladerf_direction))

;;Get the number of RX or TX channels supported by the given device
(defun get-channel-count (device direction)
  (bladerf_get_channel_count (mem-ref device :pointer) direction))

(defcenum bladerf_gain_mode
	:BLADERF_GAIN_DEFAULT
	:BLADERF_GAIN_MGC
	:BLADERF_GAIN_FASTATTACK_AGC
	:BLADERF_GAIN_SLOWATTACK_AGC
	:BLADERF_GAIN_HYBRID_AGC)

(defcstruct bladerf_gain_modes
	(name :string)
	(mode bladerf_gain_mode))

(defcfun ("bladerf_set_gain" bladerf_set_gain) :int
  (dev :pointer)
  (ch :int)
  (gain :int))

;;Set overall system gain
;; Use channel-rx and channel-tx for channel
;; eg (set-gain *dev* (channel-rx 0) 30)
;; On receive channels, 60 dB is the maximum gain level

(defun set-gain (device channel gain)  
  (let ((status (bladerf_set_gain (mem-ref device :pointer) channel gain)))
    (if (< status 0)
	(error "Failed to set gain error: ~S " status)
	t)))

(defcfun ("bladerf_get_gain" bladerf_get_gain) :int
  (dev :pointer)
  (ch :int)
  (gain :pointer))

;; Get overall system gain
(defun get-gain (device channel)
  (with-foreign-object (gain :pointer)
    (let ((status (bladerf_get_gain (mem-ref device :pointer) channel gain)))
      (if (< status 0)
	  (error "Failed to get gain")
	  (mem-ref gain :int)))))
      
(defcfun ("bladerf_set_gain_mode" bladerf_set_gain_mode) :int
  (dev :pointer)
  (ch :int)
  (mode bladerf_gain_mode))

;; Set gain control mode
(defun set-gain-mode (device channel mode)
  (bladerf_set_gain_mode (mem-ref device :pointer) channel mode))

(defcfun ("bladerf_get_gain_mode" bladerf_get_gain_mode) :int
  (dev :pointer)
  (ch :int)
  (mode :pointer))

;;Get gain control mode
(defun get-gain-mode (device channel)
  (with-foreign-object (mode :pointer)
    (let ((status (bladerf_get_gain_mode (mem-ref device :pointer) channel mode)))
      (if (< status 0)
	  (error "Failed to get gain control mode ~S" status)
	 (mem-ref mode 'bladerf_gain_mode)))))

(defcfun ("bladerf_get_gain_modes" bladerf_get_gain_modes) :int
  (dev :pointer)
  (ch :int)
  (modes :pointer))

;;Get available gain control modes 
(defun get-gain-modes (device channel)
  (with-foreign-object (modes :pointer)
    (let ((number-of-modes (bladerf_get_gain_modes (mem-ref device :pointer) channel modes)))
      (if (< number-of-modes 0)
	  (error "Failed to get available gain control modes error: ~S" number-of-modes)
	  (if (> number-of-modes 0)
	      (loop for i from 0 to (1- number-of-modes)
		 collecting
		   (mem-aref (mem-ref modes :pointer) '(:struct bladerf_gain_modes) i)))))))


(defcfun ("bladerf_get_gain_range" bladerf_get_gain_range) :int
  (dev :pointer)
  (ch :int)
  (range :pointer))

;;Get range of overall system gain
;;This may vary depending on the configured frequency, so it should be
;;checked after setting the desired frequency.
(defun get-gain-range (device channel)
  (with-foreign-object (range :pointer)
    (let ((status (bladerf_get_gain_range (mem-ref device :pointer) channel range)))
      (if (< status 0)
	  (error "Failed to get range of system gain error: ~S" status)
	  (mem-aref (mem-ref range :pointer) '(:struct bladerf_range))))))

(defcfun ("bladerf_set_gain_stage" bladerf_set_gain_stage) :int
  (dev :pointer)
  (ch :int)
  (stage :string)
  (gain :int))

;;Set the gain for a specific gain stage
(defun set-gain-stage (device channel stage gain)
  (with-foreign-string (cstage stage)
    (let ((status (bladerf_set_gain_stage (mem-ref device :pointer) channel cstage gain)))
      (if (< status 0)
	  (error "Failed to set gain for specific gain stage ~S" status)
	  t))))
    

(defcfun ("bladerf_get_gain_stage" bladerf_get_gain_stage) :int
  (dev :pointer)
  (ch :int)
  (stage :string)
  (gain :pointer))

;;Get the gain for a specific gain stage
(defun get-gain-stage (device channel stage)
  (with-foreign-string (cstage stage)
    (with-foreign-object (gain :pointer)
      (let ((status (bladerf_get_gain_stage (mem-ref device :pointer) channel cstage gain)))
	(if (< status 0)
	    (error "Failed to get gain for stage ~S error code: ~S" stage status)
	    (mem-ref gain :int))))))

(defcfun ("bladerf_get_gain_stage_range" bladerf_get_gain_stage_range) :int
  (dev :pointer)
  (ch :int)
  (stage :string)
  (range :pointer))

;;Get gain range of a specific gain stage
(defun get-gain-stage-range (device channel stage)
  (with-foreign-string (cstage stage)
    (with-foreign-object (range :pointer)
      (let ((status (bladerf_get_gain_stage_range (mem-ref device :pointer) channel cstage range)))
	(if (< status 0)
	    (error "Failed to get gain range of stage: ~S error: ~S" stage status)
	    (mem-aref (mem-ref range :pointer) '(:struct bladerf_range)))))))

(defcfun ("bladerf_get_gain_stages" bladerf_get_gain_stages) :int
  (dev :pointer)
  (ch :int)
  (stages :pointer)
  (count :pointer))

;;Get a list of available gain stages
(defun get-gain-stages (device channel)
  (with-foreign-objects ((stages :string) (count :pointer))
    (let ((no-of-gain-stages (bladerf_get_gain_stages (mem-ref device :pointer) channel stages count)))
      (if (< no-of-gain-stages 0)
	  (error "Failed to get list of available gain stages error: ~S" no-of-gain-stages)
	  (loop for i from 0 to (1- no-of-gain-stages)
	       collecting
	       (mem-aref stages :string i))))))


(defcstruct bladerf_rational_rate
  (integer :uint64)
  (num :uint64)
  (den :uint64))

(defcfun ("bladerf_set_sample_rate" bladerf_set_sample_rate) :int
  (dev :pointer)
  (ch :int)
  (rate :unsigned-int)
  (actual :pointer))

;; Configure the channel's sample rate to the specified rate in Hz.
(defun set-sample-rate (device channel rate)
  (with-foreign-object (actual :pointer)
    (let ((status (bladerf_set_sample_rate (mem-ref device :pointer) channel rate actual)))
      (if (< status 0)
	  (error "Failed to set sample rate error: ~S " status)
	  (mem-aref actual :uint)))))

(defcfun ("bladerf_set_rational_sample_rate" bladerf_set_rational_sample_rate) :int
  (dev :pointer)
  (ch :int)
  (rate :pointer)
  (actual :pointer))

;;Configure the channel's sample rate as a rational fraction of Hz.
(defun set-rational-sample-rate (device channel int numerator denominator)
  (with-foreign-objects ((rate    '(:pointer (:struct bladerf_rational_rate)))
			 (actual  '(:pointer (:struct bladerf_rational_rate))))
    (setf (foreign-slot-value rate '(:struct bladerf_rational_rate) 'integer) int)
    (setf (foreign-slot-value rate '(:struct bladerf_rational_rate) 'num) numerator)
    (setf (foreign-slot-value rate '(:struct bladerf_rational_rate) 'den) denominator)
    (let ((status (bladerf_set_rational_sample_rate (mem-ref device :pointer) channel rate actual)))
      (if (< status 0)
	  (error "Failed to set sample rate as a rational fraction of Hz error ~S" status)
	  (with-foreign-slots ((integer num den) actual (:struct bladerf_rational_rate))
	    (list :integer integer :numerator num :denominator den))))))

(defcfun ("bladerf_get_sample_rate" bladerf_get_sample_rate) :int
  (dev :pointer)
  (ch :int)
  (rate :pointer))

;;Get the channel's current sample rate in Hz

(defun get-sample-rate (device channel)
  (with-foreign-object (rate :pointer)
    (let ((status (bladerf_get_sample_rate (mem-ref device :pointer) channel rate)))
      (if (< status 0)
	  (error "Failed to get channel's current sample rate in Hz error: ~S" status)
	  (with-foreign-slots ((integer num den) rate (:struct bladerf_rational_rate))
	    (list :integer integer :numerator num :denominator den))))))

(defcfun ("bladerf_get_sample_rate_range" bladerf_get_sample_rate_range) :int
  (dev :pointer)
  (ch :int)
  (range :pointer))

;; Get the channel's supported range of sample rates
(defun get-sample-rate-range (device channel)
  (with-foreign-object (range '(:pointer (:struct bladerf_range)))
    (let ((status (bladerf_get_sample_rate_range (mem-ref device :pointer) channel range)))
      (if (< status 0)
	  (error "Failed to get the channel's supported range of sample rates error: ~S" status)
	  (mem-aref (mem-ref range :pointer) '(:struct bladerf_range))))))

(defcfun ("bladerf_get_rational_sample_rate" bladerf_get_rational_sample_rate) :int
  (dev :pointer)
  (ch :int)
  (rate :pointer))

;; Get the channel's sample rate in rational Hz
(defun get-rational-sample-rate (device channel)
  (with-foreign-object (rate '(:pointer (:struct bladerf_rational_rate)))
    (let ((status (bladerf_get_rational_sample_rate (mem-ref device :pointer) channel rate)))
      (if (< status 0)
	  (error "Failed to get channel: ~S sample rate in rational Hz error ~S" channel status)
	  (mem-aref rate '(:struct bladerf_rational_rate))))))

(defcfun ("bladerf_set_bandwidth" bladerf_set_bandwidth) :int
  (dev :pointer)
  (ch :int)
  (bandwidth :unsigned-int)
  (actual :pointer))

;;This section defines functionality for configuring a channel's bandwidth. In
;;most cases, one should define the bandwidth to be less than the sample rate
;;to minimize the impact of aliasing.

;;Set the bandwidth of the channel to the specified value in Hz
(defun set-bandwidth (device channel bandwidth)
  (with-foreign-object (actual '(:pointer :uint))
    (let ((status (bladerf_set_bandwidth (mem-ref device :pointer) channel bandwidth actual)))
      (if (< status 0)
	  (error "Failed to set bandwidth of channel: ~S to ~S Hz" channel bandwidth)
	  (mem-aref actual :uint)))))

(defcfun ("bladerf_get_bandwidth" bladerf_get_bandwidth) :int
  (dev :pointer)
  (ch :int)
  (bandwidth :pointer))

;;Get the bandwidth of the channel
(defun get-bandwidth (device channel)
  (with-foreign-object (bandwidth '(:pointer :uint))
    (let ((status (bladerf_get_bandwidth (mem-ref device :pointer) channel bandwidth)))
      (if (< status 0)
	  (error "Failed to get bandwidth of channel ~S error: ~S" channel status)
	  (mem-aref bandwidth :uint)))))

(defcfun ("bladerf_get_bandwidth_range" bladerf_get_bandwidth_range) :int
  (dev :pointer)
  (ch :int)
  (range (:pointer (:struct bladerf_range))))

;;Get the supported range of bandwidths for a channel
(defun get-bandwidth-range (device channel)
  (with-foreign-object (range '(:pointer (:struct bladerf_range)))
    (let ((status (bladerf_get_bandwidth_range (mem-ref device :pointer) channel range)))
      (if (< status 0)
	  (error "Failed to get supported range of bandwidths for channel: ~S error: ~S" channel status)
	  (mem-aref range '(:struct bladerf_range))))))
			      
(defcfun ("bladerf_select_band" bladerf_select_band) :int
  (dev :pointer)
  (ch :int)
  (frequency :uint64))

;;Select the appropriate band path given a frequency in Hz.
(defun select-band (device channel frequency)
  (let ((status (bladerf_select_band (mem-ref device :pointer) channel frequency)))
    (if (< status 0)
	(error "Failed to select band path for channel: ~S error: ~S" channel status)
	t)))

(defcfun ("bladerf_set_frequency" bladerf_set_frequency) :int
  (dev :pointer)
  (ch :int)
  (frequency :uint64))

;;Set channel's frequency in Hz
;;
;;On the bladeRF1 platform, it is recommended to keep the RX and TX
;;frequencies at least 1 MHz apart, and to digitally mix on the RX side
;;if reception closer to the TX frequency is required.
;;
;;On the bladeRF2, there is one oscillator for all RX channels and one
;;oscillator for all TX channels. Therefore, changing one channel will
;;change the frequency of all channels in that direction.

(defun set-frequency (device channel frequency)
  (let ((status (bladerf_set_frequency (mem-ref device :pointer) channel  frequency)))
    (if (< status 0)
	(error "Failed to set channel ~S frequency to ~S Hz error: ~S" channel frequency status)
	t)))


(defcfun ("bladerf_get_frequency" bladerf_get_frequency) :int
  (dev :pointer)
  (ch :int)
  (frequency (:pointer :uint64)))

;;Get channel's current frequency in Hz
(defun get-frequency (device channel)
  (with-foreign-object (frequency '(:pointer :uint64))
    (let ((status (bladerf_get_frequency (mem-ref device :pointer) channel frequency)))
      (if (< status 0)
	  (error "Failled to get channel: ~S frequency error: ~S" channel status)
	  (mem-aref frequency :uint64)))))

(defcfun ("bladerf_get_frequency_range" bladerf_get_frequency_range) :int
  (dev :pointer)
  (ch :int)
  (range (:pointer (:struct bladerf_range))))

;;Get the supported range of frequencies for a channel
(defun get-frequency-range (device channel)
  (with-foreign-object (range '(:pointer (:struct bladerf_range)))
    (let ((status (bladerf_get_frequency_range (mem-ref device :pointer) channel range)))
      (if (< status 0)
	  (error "Failed to get supported range of frequencies for channel ~S error: ~S" channel status)
	  (mem-aref (mem-ref range :pointer) '(:struct bladerf_range))))))

(defcenum bladerf_loopback
  (:BLADERF_LB_NONE #.0)
  :BLADERF_LB_FIRMWARE
  :BLADERF_LB_BB_TXLPF_RXVGA2
  :BLADERF_LB_BB_TXVGA1_RXVGA2
  :BLADERF_LB_BB_TXLPF_RXLPF
  :BLADERF_LB_BB_TXVGA1_RXLPF
  :BLADERF_LB_RF_LNA1
  :BLADERF_LB_RF_LNA2
  :BLADERF_LB_RF_LNA3
  :BLADERF_LB_RFIC_BIST)

(defcstruct bladerf_loopback_modes
  (name :string)
  (mode bladerf_loopback))

(defcfun ("bladerf_get_loopback_modes" bladerf_get_loopback_modes) :int
  (dev :pointer)
  (modes (:pointer (:struct bladerf_loopback_modes))))

;;Get loopback modes
(defun get-loopback-modes (device)
  (with-foreign-object (modes '(:pointer (:struct bladerf_loopback_modes)))
    (let ((number-of-modes (bladerf_get_loopback_modes (mem-ref device :pointer) modes)))
      (if (< number-of-modes 0)
	  (error "Failed to get loopback modes error: ~S" number-of-modes)
	  (if (> number-of-modes 0)
	      (loop for i from 0 to (1- number-of-modes)
		 collecting
		   (mem-aref (mem-ref modes :pointer) '(:struct bladerf_loopback_modes) i)))))))

(defcfun ("bladerf_is_loopback_mode_supported" bladerf_is_loopback_mode_supported) :bool
  (dev :pointer)
  (mode bladerf_loopback))

;;Test if a given loopback mode is supported on this device.
(defun is-loopback-mode-supported-p (device mode)
  (bladerf_is_loopback_mode_supported (mem-ref device :pointer) mode))
    

(defcfun ("bladerf_set_loopback" bladerf_set_loopback) :int
  (dev :pointer)
  (lb bladerf_loopback))

;;Apply specified loopback mode
;;Loopback modes should only be enabled or disabled while the RX and TX
;;channels are both disabled (and therefore, when no samples are being
;;actively streamed). Otherwise, unexpected behavior may occur.
(defun set-loopback (device loopback)
  (let ((status (bladerf_set_loopback (mem-ref device :pointer) loopback)))
    (if (< status 0)
	(error "Failed to set loopback mode to: ~S error: ~S" loopback status)
	t)))

(defcfun ("bladerf_get_loopback" bladerf_get_loopback) :int
  (dev :pointer)
  (lb (:pointer bladerf_loopback)))

;;Get current loopback mode
(defun get-loopback (device)
  (with-foreign-object (lb '(:pointer bladerf_loopback))
    (let ((status (bladerf_get_loopback (mem-ref device :pointer) lb)))
      (if (< status 0)
	  (error "Failed to get current loopback mode error: ~S" status)
	  (mem-aref lb 'bladerf_loopback)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TODO implement trigers
(defcenum bladerf_trigger_role
  (:BLADERF_TRIGGER_ROLE_INVALID #.-1)
  :BLADERF_TRIGGER_ROLE_DISABLED
  :BLADERF_TRIGGER_ROLE_MASTER
  :BLADERF_TRIGGER_ROLE_SLAVE)

(defcenum bladerf_trigger_signal
  (:BLADERF_TRIGGER_INVALID #.-1)
  :BLADERF_TRIGGER_J71_4
  :BLADERF_TRIGGER_J51_1
  :BLADERF_TRIGGER_MINI_EXP_1
  (:BLADERF_TRIGGER_USER_0 #.128)
  :BLADERF_TRIGGER_USER_1
  :BLADERF_TRIGGER_USER_2
  :BLADERF_TRIGGER_USER_3
  :BLADERF_TRIGGER_USER_4
  :BLADERF_TRIGGER_USER_5
  :BLADERF_TRIGGER_USER_6
  :BLADERF_TRIGGER_USER_7)

(defcstruct bladerf_trigger
  (channel :int)
  (role bladerf_trigger_role)
  (signal bladerf_trigger_signal)
  (options :pointer))

(defcfun ("bladerf_trigger_init" bladerf_trigger_init) :int
  (dev :pointer)
  (ch :int)
  (signal bladerf_trigger_signal)
  (trigger :pointer))

(defcfun ("bladerf_trigger_arm" bladerf_trigger_arm) :int
  (dev :pointer)
  (trigger :pointer)
  (arm :pointer)
  (resv1 :pointer)
  (resv2 :pointer))

(defcfun ("bladerf_trigger_fire" bladerf_trigger_fire) :int
  (dev :pointer)
  (trigger :pointer))

(defcfun ("bladerf_trigger_state" bladerf_trigger_state) :int
  (dev :pointer)
  (trigger :pointer)
  (is_armed :pointer)
  (has_fired :pointer)
  (fire_requested :pointer)
  (resv1 :pointer)
  (resv2 :pointer))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END TODO Trigers

(defcenum bladerf_rx_mux
  (:BLADERF_RX_MUX_INVALID #.-1)
  (:BLADERF_RX_MUX_BASEBAND #.#x0)
  (:BLADERF_RX_MUX_12BIT_COUNTER #.#x1)
  (:BLADERF_RX_MUX_32BIT_COUNTER #.#x2)
  (:BLADERF_RX_MUX_DIGITAL_LOOPBACK #.#x4))

(defcfun ("bladerf_set_rx_mux" bladerf_set_rx_mux) :int
  (dev :pointer)
  (mux bladerf_rx_mux))

;;Set the current RX Mux mode
(defun set-rx-mux (device mux)
  (let ((status (bladerf_set_rx_mux (mem-ref device :pointer) mux)))
    (if (< status 0)
	(error "Failed to set RX Mux mode to ~S  error: ~S" mux status)
	t)))

(defcfun ("bladerf_get_rx_mux" bladerf_get_rx_mux) :int
  (dev :pointer)
  (mode (:pointer bladerf_rx_mux)))

;;Get the current RX Mux mode
(defun get-rx-mux (device)
  (with-foreign-object (mode '(:pointer bladerf_rx_mux))
    (let ((status (bladerf_get_rx_mux (mem-ref device :pointer) mode)))
      (if (< status 0)
	  (error "Failed to get current RX Mux mode error: ~S" status)
	  (mem-aref mode 'bladerf_rx_mux)))))

(defcstruct bladerf_quick_tune
  (freqsel :uint8)
  (vcocap :uint8)
  (nint :uint16)
  (nfrac :uint32)
  (flags :uint8)
  (tbd :pointer))

(defcfun ("bladerf_schedule_retune" bladerf_schedule_retune) :int
  (dev :pointer)
  (ch :int)
  (timestamp :uint64)
  (frequency :uint64)
  (quick_tune (:pointer (:struct bladerf_quick_tune))))

;;Schedule a frequency retune to occur at specified sample timestamp value.
;; sync-config must have been called with the
;; BLADERF_FORMAT_SC16_Q11_META format for the associated channel in
;; order to enable timestamps. (The timestamped metadata format must be
;; enabled in order to use this function.)
(defun schedule-retune (device channel timestamp frequency quick-tune)
  (let ((status (bladerf_schedule_retune (mem-ref device :pointer) channel timestamp frequency quick-tune)))
    (if (< status 0)
	(error "Failed to schedule a frequency retune to occur at sample with timestamp: ~S error: ~S" timestamp status)
	t)))

(defcfun ("bladerf_cancel_scheduled_retunes" bladerf_cancel_scheduled_retunes) :int
  (dev :pointer)
  (ch :int))

;;Cancel all pending scheduled retune operations for the specified channel.
(defun cancel-scheduled-retunes (device channel)
  (let ((status (bladerf_cancel_scheduled_retunes (mem-ref device :pointer) channel)))
    (if (< status 0)
	(error "Failed to cancel scheduled retune operations for the channel ~S  error: ~S" channel status)
	t)))

(defcfun ("bladerf_get_quick_tune" bladerf_get_quick_tune) :int
  (dev :pointer)
  (ch :int)
  (quick_tune (:pointer (:struct bladerf_quick_tune))))
;;Fetch parameters used to tune the transceiver to the current frequency for
;; use with bladerf_schedule_retune() to perform a "quick retune."

(defun get-quick-tune (device channel)
  (with-foreign-object (quick-tune '(:pointer (:struct bladerf_quick_tune)))
    (let ((status (bladerf_get_quick_tune (mem-ref device :pointer) channel quick-tune)))
      (if (< status 0)
	  (error "Failed to fetch quick tune parameters error: ~S" status)
	  (mem-aref quick-tune '(:struct bladerf_quick_tune))))))

(defcenum bladerf_correction
  :BLADERF_CORR_DCOFF_I
  :BLADERF_CORR_DCOFF_Q
  :BLADERF_CORR_PHASE
  :BLADERF_CORR_GAIN)

(defcfun ("bladerf_set_correction" bladerf_set_correction) :int
  (dev :pointer)
  (ch :int)
  (corr bladerf_correction)
  (value :uint16))
;;Set the value of the specified configuration parameter
(defun set-correction (device channel correction value)
  (let ((status (bladerf_set_correction (mem-ref device :pointer) channel correction value)))
    (if (< status 0)
	(error "Failed to set the value of configuration parameter error: ~S" status)
	(= status 0))))
	
(defcfun ("bladerf_get_correction" bladerf_get_correction) :int
  (dev :pointer)
  (ch :int)
  (corr bladerf_correction)
  (value (:pointer :uint16)))

;;Obtain the current value of the specified configuration parameter
(defun get-correction (device channel correction)
  (with-foreign-object (value '(:pointer :uint16))
    (let ((status (bladerf_get_correction device channel correction value)))
      (if (< status 0)
	  (error "Failed to obtain current value for configuration parameter: ~S error: ~S" correction status)
	  (mem-aref value :uint16)))))

(defun minimum-buffer-size (number-of-samples number-of-channels)
  (* 2 number-of-samples number-of-channels (foreign-type-size :int16))) 

(defun buffer-length (number-of-samples number-of-channels)
  (* 2 number-of-samples number-of-channels))

(defcenum bladerf_format
  :BLADERF_FORMAT_SC16_Q11
  :BLADERF_FORMAT_SC16_Q11_META)

(defconstant BLADERF_META_STATUS_OVERRUN (ash 1 0))

(defconstant BLADERF_META_STATUS_UNDERRUN (ash 1 1))

(defconstant BLADERF_META_FLAG_TX_BURST_START (ash 1 0))

(defconstant BLADERF_META_FLAG_TX_BURST_END (ash 1 1))

(defconstant BLADERF_META_FLAG_TX_NOW (ash 1 2))

(defconstant BLADERF_META_FLAG_TX_UPDATE_TIMESTAMP (ash 1 3))

(defconstant BLADERF_META_FLAG_RX_NOW (ash 1 31))

(defcstruct bladerf_metadata
  (timestamp :uint64)
  (flags :uint32)
  (status :uint32)
  (actual_count :unsigned-int)
  (reserved :uint8 :count 32))

(defcfun ("bladerf_interleave_stream_buffer" bladerf_interleave_stream_buffer) :int
  (layout bladerf_channel_layout)
  (format bladerf_format)
  (buffer_size :unsigned-int)
  (samples :pointer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TODO Test this function
;;Interleaves contiguous blocks of samples in preparation for MIMO TX
(defun interleave-stream-buffer (layout format buffer-size samples)
  (let ((status (bladerf_interleave_stream_buffer layout format buffer-size samples)))
    (if (< status 0)
	(error "Failed to interleave samples error: ~S" status)
	(= status 0))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun ("bladerf_deinterleave_stream_buffer" bladerf_deinterleave_stream_buffer) :int
  (layout bladerf_channel_layout)
  (format bladerf_format)
  (buffer_size :unsigned-int)
  (samples :pointer))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TODO test function
;; Deinterleaves samples into contiguous blocks after MIMO RX.
(defun deinterleave-stream-buffer (layout format buffer-size samples)
  (let ((status (bladerf_deinterleave_stream_buffer layout format buffer-size samples)))
    (if (< status 0)
	(error "Failed to interleave samples error: ~S" status)
	(= status 0))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcfun ("bladerf_enable_module" bladerf_enable_module) :int
  (dev :pointer)
  (ch :int)
  (enable :bool))

;;Enable or disable the RF front end of the specified direction.

(defun enable-module (device channel enable)
  (let ((status (bladerf_enable_module (mem-ref device :pointer) channel enable)))
    (if (< status 0)
	(error "Failed to enable module error: ~S" status)
	(= status 0))))

(defcfun ("bladerf_get_timestamp" bladerf_get_timestamp) :int
  (dev :pointer)
  (dir bladerf_direction)
  (timestamp (:pointer :uint64)))
;; Retrieve the specified stream's current timestamp counter value from the
;; FPGA.

(defun get-timestamp (device direction)
  (with-foreign-object (timestamp '(:pointer :uint64))
    (let ((status (bladerf_get_timestamp (mem-ref device :pointer) direction timestamp)))
      (if (< status 0)
	  (error "Failed to get timestamp error: ~S" status)
	  (mem-aref timestamp :uint64)))))

(defcfun ("bladerf_sync_config" bladerf_sync_config) :int
  (dev :pointer)
  (layout bladerf_channel_layout)
  (format bladerf_format)
  (num_buffers :unsigned-int)
  (buffer_size :unsigned-int)
  (num_transfers :unsigned-int)
  (stream_timeout :unsigned-int))

;;(Re)Configure a device for synchronous transmission or reception
(defun sync-config (device layout format num-buffers buffer-size num-transfers stream-timeout)
  (let ((status (bladerf_sync_config (mem-ref device :pointer) layout format num-buffers buffer-size num-transfers stream-timeout)))
    (if (< status 0)
	(error "Failed to (Re)Configure device error: ~S" status)
	(= status 0))))

(defcfun ("bladerf_sync_tx" bladerf_sync_tx) :int
  (dev :pointer)
  (samples (:pointer :void))
  (num_samples :unsigned-int)
  (metadata (:pointer (:struct bladerf_metadata)))
  (timeout_ms :unsigned-int))

;; Transmit IQ samples.
(defun sync-tx (device samples num-samples metadata timeout-ms)
  (let ((status (bladerf_sync_tx (mem-ref device :pointer) samples num-samples metadata timeout-ms)))
    (if (< status 0)
	(error "Failed to transmit IQ samples error: ~S" status)
	(= status 0))))

(defcfun ("bladerf_sync_rx" bladerf_sync_rx) :int
  (dev :pointer)
  (samples (:pointer :void))
  (num_samples :unsigned-int)
  (metadata (:pointer (:struct bladerf_metadata)))
  (timeout_ms :unsigned-int))

;;Receive IQ samples.
;; Under the hood, this call starts up an underlying asynchronous stream as
;; needed. This stream can be stopped by disabling the RX channel. (See
;; bladerf_enable_module for more details.)
;;
;; @pre A bladerf_sync_config() call has been to configure the device for
;;      synchronous data transfer.
;;
;; @note A call to bladerf_enable_module() should be made before attempting to
;;       receive samples. Failing to do this may result in timeouts and other
;;       errors.

(defun sync-rx (device number-of-samples timeout-ms)
  (let ((samples-to-allocate (* number-of-samples 2)))
    (with-foreign-objects ((rx-samples :uint16 samples-to-allocate)
			   (metadata :pointer))
      (let ((status (bladerf_sync_rx (mem-ref device :pointer) rx-samples number-of-samples metadata timeout-ms))
	    (samples-array (make-array samples-to-allocate :element-type '(complex float) :initial-element (complex 0.0 0.0) :fill-pointer 0)))
	(if (< status 0)
	    (error "Failed to recieve IQ samples error: ~S" status)
	    (progn
	      (loop for i below samples-to-allocate by 2
		 do
		   (vector-push  
		    (complex
		     (mem-aref rx-samples :int16 (1+ i))
		     (mem-aref rx-samples :int16 i))
		    samples-array))
	      samples-array))))))



(defun sync-rx-complex-double-float (device number-of-samples timeout-ms)
  (let ((samples-to-allocate (* number-of-samples 2)))
    (with-foreign-objects ((rx-samples :uint16 samples-to-allocate)
			   (metadata :pointer))
      (let ((status (bladerf_sync_rx (mem-ref device :pointer) rx-samples number-of-samples metadata timeout-ms))
	    (samples-array (make-array samples-to-allocate :element-type '(complex double-float))))
	(if (< status 0)
	    (error "Failed to recieve IQ samples error: ~S" status)
	    (progn
	      (loop for i below samples-to-allocate by 2
		 do
		   (vector-push  
		    (complex
		     (coerce (mem-aref rx-samples :int16 (1+ i)) 'double-float)
		     (coerce (mem-aref rx-samples :int16 i) 'double-float))
		    samples-array))
	      samples-array))))))

(defcfun ("bladerf_init_stream" bladerf_init_stream) :int
  (stream :pointer)
  (dev :pointer)
  (callback :pointer)
  (buffers :pointer)
  (num_buffers :pointer)
  (format bladerf_format)
  (samples_per_buffer :pointer)
  (num_transfers :pointer)
  (user_data :pointer))

(defcfun ("bladerf_stream" bladerf_stream) :int
  (stream :pointer)
  (layout bladerf_channel_layout))

(defcfun ("bladerf_submit_stream_buffer" bladerf_submit_stream_buffer) :int
  (stream :pointer)
  (buffer :pointer)
  (timeout_ms :unsigned-int))

(defcfun ("bladerf_submit_stream_buffer_nb" bladerf_submit_stream_buffer_nb) :int
  (stream :pointer)
  (buffer :pointer))

(defcfun ("bladerf_deinit_stream" bladerf_deinit_stream) :void
  (stream :pointer))

(defcfun ("bladerf_set_stream_timeout" bladerf_set_stream_timeout) :int
  (dev :pointer)
  (dir bladerf_direction)
  (timeout :unsigned-int))

(defcfun ("bladerf_get_stream_timeout" bladerf_get_stream_timeout) :int
  (dev :pointer)
  (dir bladerf_direction)
  (timeout :pointer))

(defcfun ("bladerf_flash_firmware" bladerf_flash_firmware) :int
  (dev :pointer)
  (firmware :string))

(defcfun ("bladerf_load_fpga" bladerf_load_fpga) :int
  (dev :pointer)
  (fpga :string))

(defcfun ("bladerf_flash_fpga" bladerf_flash_fpga) :int
  (dev :pointer)
  (fpga_image :string))

(defcfun ("bladerf_erase_stored_fpga" bladerf_erase_stored_fpga) :int
  (dev :pointer))

(defcfun ("bladerf_device_reset" bladerf_device_reset) :int
  (dev :pointer))

(defcfun ("bladerf_get_fw_log" bladerf_get_fw_log) :int
  (dev :pointer)
  (filename :string))

(defcfun ("bladerf_jump_to_bootloader" bladerf_jump_to_bootloader) :int
  (dev :pointer))

(defcfun ("bladerf_get_bootloader_list" bladerf_get_bootloader_list) :int
  (list :pointer))

(defcfun ("bladerf_load_fw_from_bootloader" bladerf_load_fw_from_bootloader) :int
  (device_identifier :string)
  (backend bladerf_backend)
  (bus :pointer)
  (addr :pointer)
  (file :string))

(defcenum bladerf_image_type
	(:BLADERF_IMAGE_TYPE_INVALID #.-1)
	:BLADERF_IMAGE_TYPE_RAW
	:BLADERF_IMAGE_TYPE_FIRMWARE
	:BLADERF_IMAGE_TYPE_FPGA_40KLE
	:BLADERF_IMAGE_TYPE_FPGA_115KLE
	:BLADERF_IMAGE_TYPE_CALIBRATION
	:BLADERF_IMAGE_TYPE_RX_DC_CAL
	:BLADERF_IMAGE_TYPE_TX_DC_CAL
	:BLADERF_IMAGE_TYPE_RX_IQ_CAL
	:BLADERF_IMAGE_TYPE_TX_IQ_CAL)

(defconstant BLADERF_IMAGE_MAGIC_LEN 7)

(defconstant BLADERF_IMAGE_CHECKSUM_LEN 32)

(defconstant BLADERF_IMAGE_RESERVED_LEN 128)

(defcstruct bladerf_image
  (magic :char :count 8)
  (checksum :uint8 :count 32)
  (version (:struct bladerf_version))
  (timestamp :uint64)
  (serial :char :count 34)
  (reserved :char :count 128)
  (type bladerf_image_type)
  (address :uint32)
  (length :uint32)
  (data :pointer))

(defcfun ("bladerf_alloc_image" bladerf_alloc_image) :pointer
  (type bladerf_image_type)
  (address :pointer)
  (length :pointer))

(defcfun ("bladerf_alloc_cal_image" bladerf_alloc_cal_image) :pointer
  (fpga_size bladerf_fpga_size)
  (vctcxo_trim :pointer))

(defcfun ("bladerf_free_image" bladerf_free_image) :void
  (image :pointer))

(defcfun ("bladerf_image_write" bladerf_image_write) :int
  (image :pointer)
  (file :string))

(defcfun ("bladerf_image_read" bladerf_image_read) :int
  (image :pointer)
  (file :string))

(defcenum bladerf_vctcxo_tamer_mode
	(:BLADERF_VCTCXO_TAMER_INVALID #.-1)
	(:BLADERF_VCTCXO_TAMER_DISABLED #.0)
	(:BLADERF_VCTCXO_TAMER_1_PPS #.1)
	(:BLADERF_VCTCXO_TAMER_10_MHZ #.2))

(defcfun ("bladerf_set_vctcxo_tamer_mode" bladerf_set_vctcxo_tamer_mode) :int
  (dev :pointer)
  (mode bladerf_vctcxo_tamer_mode))

(defcfun ("bladerf_get_vctcxo_tamer_mode" bladerf_get_vctcxo_tamer_mode) :int
  (dev :pointer)
  (mode :pointer))

(defcfun ("bladerf_get_vctcxo_trim" bladerf_get_vctcxo_trim) :int
  (dev :pointer)
  (trim :pointer))

(defcfun ("bladerf_trim_dac_write" bladerf_trim_dac_write) :int
  (dev :pointer)
  (val :pointer))

(defcfun ("bladerf_trim_dac_read" bladerf_trim_dac_read) :int
  (dev :pointer)
  (val :pointer))

(defcenum bladerf_tuning_mode
	(:BLADERF_TUNING_MODE_INVALID #.-1)
	:BLADERF_TUNING_MODE_HOST
	:BLADERF_TUNING_MODE_FPGA)

(defcfun ("bladerf_set_tuning_mode" bladerf_set_tuning_mode) :int
  (dev :pointer)
  (mode bladerf_tuning_mode))

(defcfun ("bladerf_get_tuning_mode" bladerf_get_tuning_mode) :int
  (dev :pointer)
  (mode :pointer))

(defcfun ("bladerf_read_trigger" bladerf_read_trigger) :int
  (dev :pointer)
  (ch :int)
  (signal bladerf_trigger_signal)
  (val :pointer))

(defcfun ("bladerf_write_trigger" bladerf_write_trigger) :int
  (dev :pointer)
  (ch :int)
  (signal bladerf_trigger_signal)
  (val :pointer))

(defcfun ("bladerf_config_gpio_read" bladerf_config_gpio_read) :int
  (dev :pointer)
  (val :pointer))

(defcfun ("bladerf_config_gpio_write" bladerf_config_gpio_write) :int
  (dev :pointer)
  (val :pointer))

(defcfun ("bladerf_erase_flash" bladerf_erase_flash) :int
  (dev :pointer)
  (erase_block :pointer)
  (count :pointer))

(defcfun ("bladerf_read_flash" bladerf_read_flash) :int
  (dev :pointer)
  (buf :pointer)
  (page :pointer)
  (count :pointer))

(defcfun ("bladerf_write_flash" bladerf_write_flash) :int
  (dev :pointer)
  (buf :pointer)
  (page :pointer)
  (count :pointer))

(defcfun ("bladerf_set_rf_port" bladerf_set_rf_port) :int
  (dev :pointer)
  (ch :int)
  (port :string))

(defcfun ("bladerf_get_rf_port" bladerf_get_rf_port) :int
  (dev :pointer)
  (ch :int)
  (port :pointer))

(defcfun ("bladerf_get_rf_ports" bladerf_get_rf_ports) :int
  (dev :pointer)
  (ch :int)
  (ports :pointer)
  (count :unsigned-int))

(defcenum bladerf_xb
	(:BLADERF_XB_NONE #.0)
	:BLADERF_XB_100
	:BLADERF_XB_200
	:BLADERF_XB_300)

(defcfun ("bladerf_expansion_attach" bladerf_expansion_attach) :int
  (dev :pointer)
  (xb bladerf_xb))

(defcfun ("bladerf_expansion_get_attached" bladerf_expansion_get_attached) :int
  (dev :pointer)
  (xb :pointer))

(defcenum bladerf_log_level
	:BLADERF_LOG_LEVEL_VERBOSE
	:BLADERF_LOG_LEVEL_DEBUG
	:BLADERF_LOG_LEVEL_INFO
	:BLADERF_LOG_LEVEL_WARNING
	:BLADERF_LOG_LEVEL_ERROR
	:BLADERF_LOG_LEVEL_CRITICAL
	:BLADERF_LOG_LEVEL_SILENT)

(defcfun ("bladerf_log_set_verbosity" bladerf_log_set_verbosity) :void
  (level bladerf_log_level))

(defconstant BLADERF_ERR_UNEXPECTED -1)

(defconstant BLADERF_ERR_RANGE -2)

(defconstant BLADERF_ERR_INVAL -3)

(defconstant BLADERF_ERR_MEM -4)

(defconstant BLADERF_ERR_IO -5)

(defconstant BLADERF_ERR_TIMEOUT -6)

(defconstant BLADERF_ERR_NODEV -7)

(defconstant BLADERF_ERR_UNSUPPORTED -8)

(defconstant BLADERF_ERR_MISALIGNED -9)

(defconstant BLADERF_ERR_CHECKSUM -10)

(defconstant BLADERF_ERR_NO_FILE -11)

(defconstant BLADERF_ERR_UPDATE_FPGA -12)

(defconstant BLADERF_ERR_UPDATE_FW -13)

(defconstant BLADERF_ERR_TIME_PAST -14)

(defconstant BLADERF_ERR_QUEUE_FULL -15)

(defconstant BLADERF_ERR_FPGA_OP -16)

(defconstant BLADERF_ERR_PERMISSION -17)

(defconstant BLADERF_ERR_WOULD_BLOCK -18)

(defconstant BLADERF_ERR_NOT_INIT -19)

(defcfun ("bladerf_strerror" bladerf_strerror) :string
  (error :int))
