;;;; cl-bladeRF.lisp
;;
;;;; Copyright (c) 2021 Tichaona Kadzinga <tichaona@kadzinga.com>

(in-package #:cl-bladerf)

(define-foreign-library libbladerf
    (:unix (:or "libbladeRF.so.2" "libbladeRF.so" "libbladeRF.2.dylib" "libbladeRF.dylib"))
    (t (:default "libbladeRF")))

(use-foreign-library libbladerf)

;; This is an opaque structure
(defcstruct (bladerf-device))

;; API Version constant
(defconstant LIBBLADERF_API_VERSION #x02050000)

;; Backend types
(defcenum bladerf_backend
	:BLADERF_BACKEND_ANY
	:BLADERF_BACKEND_LINUX
	:BLADERF_BACKEND_LIBUSB
	:BLADERF_BACKEND_CYPRESS
	(:BLADERF_BACKEND_DUMMY #.100))

(defconstant BLADERF_DESCRIPTION_LENGTH 33)
(defconstant BLADERF_SERIAL_LENGTH 33)

;; Device info structure
(defcstruct bladerf_devinfo
	(backend bladerf_backend)
	(serial :char :count 33)
	(usb_bus :uint8)
	(usb_addr :uint8)
	(instance :unsigned-int)
	(manufacturer :char :count 33)
	(product :char :count 33))

;; Backend info structure
(defcstruct bladerf_backendinfo
	(handle_count :int)
	(handle :pointer)
	(lock_count :int)
	(lock :pointer))

;; Core device functions
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

(defcfun ("bladerf_get_backendinfo" bladerf_get_backendinfo) :int
  (dev :pointer)
  (info :pointer))

(defcfun ("bladerf_get_devinfo_from_str" bladerf_get_devinfo_from_str) :int
  (devstr :string)
  (info :pointer))

(defcfun ("bladerf_devinfo_matches" bladerf_devinfo_matches) :boolean
  (a :pointer)
  (b :pointer))

(defcfun ("bladerf_devstr_matches" bladerf_devstr_matches) :boolean
  (dev_str :string)
  (info :pointer))

(defcfun ("bladerf_backend_str" bladerf_backend_str) :string
  (backend bladerf_backend))

(defcfun ("bladerf_set_usb_reset_on_open" bladerf_set_usb_reset_on_open) :void
  (enabled :boolean))

(defun bladerf-open-device (ptr->device &optional (device-identifier ""))
  (bladerf_set_usb_reset_on_open t)
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

;; Range structure
(defcstruct bladerf_range
  (min :int64)
  (max :int64)
  (step :int64)
  (scale :float))

;; Serial structure
(defcstruct bladerf_serial
  (serial :char :count 33))

;; Version structure
(defcstruct bladerf_version
  (major :uint16)
  (minor :uint16)
  (patch :uint16)
  (describe :string))

;; FPGA size enum
(defcenum bladerf_fpga_size
	(:BLADERF_FPGA_UNKNOWN #.0)
	(:BLADERF_FPGA_40KLE #.40)
	(:BLADERF_FPGA_115KLE #.115)
	(:BLADERF_FPGA_A4 #.49)
	(:BLADERF_FPGA_A5 #.77)
	(:BLADERF_FPGA_A9 #.301))

;; Device speed enum
(defcenum bladerf_dev_speed
	:BLADERF_DEVICE_SPEED_UNKNOWN
	:BLADERF_DEVICE_SPEED_HIGH
	:BLADERF_DEVICE_SPEED_SUPER)

;; FPGA source enum
(defcenum bladerf_fpga_source
	(:BLADERF_FPGA_SOURCE_UNKNOWN #.0)
	(:BLADERF_FPGA_SOURCE_FLASH #.1)
	(:BLADERF_FPGA_SOURCE_HOST #.2))

;; Device info functions
(defcfun ("bladerf_get_serial" bladerf_get_serial) :int
  (dev :pointer)
  (serial :string))

(defcfun ("bladerf_get_serial_struct" bladerf_get_serial_struct) :int
  (dev :pointer)
  (serial :pointer))

(defun get-serial-struct (device)
  (with-foreign-object (serial '(:struct bladerf_serial))
    (let ((status (bladerf_get_serial_struct (mem-ref device :pointer) serial)))
      (if (< status 0)
	  (error "Failed to get serial number error: ~S" status)
	  (foreign-string-to-lisp (foreign-slot-pointer serial '(:struct bladerf_serial) 'serial))))))

(defcfun ("bladerf_get_fpga_size" bladerf_get_fpga_size) :int
  (dev :pointer)
  (size :pointer))

(defun get-fpga-size (ptr->device)
  (with-foreign-object (size 'bladerf_fpga_size)
    (let ((status (bladerf_get_fpga_size (mem-ref ptr->device :pointer) size)))
      (if (< status 0)
	  (error "Failed to get on-board FPGA size")
	  (mem-ref size 'bladerf_fpga_size)))))

(defcfun ("bladerf_get_fpga_bytes" bladerf_get_fpga_bytes) :int
  (dev :pointer)
  (size :pointer))

(defcfun ("bladerf_get_flash_size" bladerf_get_flash_size) :int
  (dev :pointer)
  (size :pointer)
  (is_guess :pointer))

(defun get-flash-size (device)
  (with-foreign-objects ((size :uint32) (is-guess :boolean))
    (let ((status (bladerf_get_flash_size (mem-ref device :pointer) size is-guess)))
      (if (< status 0)
	  (error "Failed to get on-board flash")
	  (if (mem-ref is-guess :boolean)
	      (format t "The guessed flash size is ~a bytes" (mem-ref size :uint32))
	      (format t "The flash size is ~a bytes" (mem-ref size :uint32)))))))

(defcfun ("bladerf_fw_version" bladerf_fw_version) :int
  (dev :pointer)
  (version :pointer))

(defun get-firmware-version (device)
  (with-foreign-object (version '(:struct bladerf_version))
    (let ((status (bladerf_fw_version (mem-ref device :pointer) version)))
      (if (< status 0)
	  (error "Failed to get firmware version")
	  (with-foreign-slots ((describe) version (:struct bladerf_version))
	    describe)))))

(defcfun ("bladerf_is_fpga_configured" bladerf_is_fpga_configured) :int
  (dev :pointer))

(defun fpga-configured-p (device)
  (let ((status (bladerf_is_fpga_configured (mem-ref device :pointer))))
    (case status
      (0 nil)
      (1 t)
      (otherwise (error "Failed to check FPGA configuration status")))))

(defcfun ("bladerf_fpga_version" bladerf_fpga_version) :int
  (dev :pointer)
  (version :pointer))

(defun get-fpga-version (device)
  (with-foreign-object (version '(:struct bladerf_version))
    (let ((status (bladerf_fpga_version (mem-ref device :pointer) version)))
      (if (< status 0)
	  (error "Failed to get FPGA version")
	  (with-foreign-slots ((describe) version (:struct bladerf_version))
	    describe)))))

(defcfun ("bladerf_get_fpga_source" bladerf_get_fpga_source) :int
  (dev :pointer)
  (source :pointer))

(defun get-fpga-source (device)
  (with-foreign-object (source 'bladerf_fpga_source)
    (let ((status (bladerf_get_fpga_source (mem-ref device :pointer) source)))
      (if (< status 0)
	  (error "Failed to get FPGA source")
	  (mem-ref source 'bladerf_fpga_source)))))

(defcfun ("bladerf_device_speed" bladerf_device_speed) bladerf_dev_speed
  (dev :pointer))

(defun get-device-speed (device)
  (bladerf_device_speed (mem-ref device :pointer)))

(defcfun ("bladerf_get_board_name" bladerf_get_board_name) :string
  (dev :pointer))

(defun get-board-name (device)
  (bladerf_get_board_name (mem-ref device :pointer)))

;; Channel macros and functions
(defun channel-rx (channel)
  (logior (ash channel 1) #x0))

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

(defun get-channel-count (device direction)
  (bladerf_get_channel_count (mem-ref device :pointer) direction))

;; Gain control
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

(defun set-gain (device channel gain)
  (let ((status (bladerf_set_gain (mem-ref device :pointer) channel gain)))
    (if (< status 0)
	(error "Failed to set gain error: ~S " status)
	t)))

(defcfun ("bladerf_get_gain" bladerf_get_gain) :int
  (dev :pointer)
  (ch :int)
  (gain :pointer))

(defun get-gain (device channel)
  (with-foreign-object (gain :int)
    (let ((status (bladerf_get_gain (mem-ref device :pointer) channel gain)))
      (if (< status 0)
	  (error "Failed to get gain")
	  (mem-ref gain :int)))))

(defcfun ("bladerf_set_gain_mode" bladerf_set_gain_mode) :int
  (dev :pointer)
  (ch :int)
  (mode bladerf_gain_mode))

(defun set-gain-mode (device channel mode)
  (bladerf_set_gain_mode (mem-ref device :pointer) channel mode))

(defcfun ("bladerf_get_gain_mode" bladerf_get_gain_mode) :int
  (dev :pointer)
  (ch :int)
  (mode :pointer))

(defun get-gain-mode (device channel)
  (with-foreign-object (mode 'bladerf_gain_mode)
    (let ((status (bladerf_get_gain_mode (mem-ref device :pointer) channel mode)))
      (if (< status 0)
	  (error "Failed to get gain control mode ~S" status)
	 (mem-ref mode 'bladerf_gain_mode)))))

(defcfun ("bladerf_get_gain_modes" bladerf_get_gain_modes) :int
  (dev :pointer)
  (ch :int)
  (modes :pointer))

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

(defun get-gain-range (device channel)
  (with-foreign-object (range :pointer)
    (let ((status (bladerf_get_gain_range (mem-ref device :pointer) channel range)))
      (if (< status 0)
	  (error "Failed to get range of system gain error: ~S" status)
	  (mem-ref (mem-ref range :pointer) '(:struct bladerf_range))))))

(defcfun ("bladerf_set_gain_stage" bladerf_set_gain_stage) :int
  (dev :pointer)
  (ch :int)
  (stage :string)
  (gain :int))

(defun set-gain-stage (device channel stage gain)
  (let ((status (bladerf_set_gain_stage (mem-ref device :pointer) channel stage gain)))
    (if (< status 0)
	(error "Failed to set gain for specific gain stage ~S" status)
	t)))

(defcfun ("bladerf_get_gain_stage" bladerf_get_gain_stage) :int
  (dev :pointer)
  (ch :int)
  (stage :string)
  (gain :pointer))

(defun get-gain-stage (device channel stage)
  (with-foreign-object (gain :int)
    (let ((status (bladerf_get_gain_stage (mem-ref device :pointer) channel stage gain)))
      (if (< status 0)
	  (error "Failed to get gain for stage ~S error code: ~S" stage status)
	  (mem-ref gain :int)))))

(defcfun ("bladerf_get_gain_stage_range" bladerf_get_gain_stage_range) :int
  (dev :pointer)
  (ch :int)
  (stage :string)
  (range :pointer))

(defun get-gain-stage-range (device channel stage)
  (with-foreign-object (range :pointer)
    (let ((status (bladerf_get_gain_stage_range (mem-ref device :pointer) channel stage range)))
      (if (< status 0)
	  (error "Failed to get gain range of stage: ~S error: ~S" stage status)
	  (mem-ref (mem-ref range :pointer) '(:struct bladerf_range))))))

(defcfun ("bladerf_get_gain_stages" bladerf_get_gain_stages) :int
  (dev :pointer)
  (ch :int)
  (stages :pointer)
  (count :pointer))

(defun get-gain-stages (device channel)
  (with-foreign-objects ((stages :pointer) (count :uint))
    (let ((no-of-gain-stages (bladerf_get_gain_stages (mem-ref device :pointer) channel stages count)))
      (if (< no-of-gain-stages 0)
	  (error "Failed to get list of available gain stages error: ~S" no-of-gain-stages)
	  (loop for i from 0 to (1- no-of-gain-stages)
	       collecting
	       (mem-aref (mem-ref stages :pointer) :string i))))))

;; Sample rate
(defcstruct bladerf_rational_rate
  (integer :uint64)
  (num :uint64)
  (den :uint64))

(defcfun ("bladerf_set_sample_rate" bladerf_set_sample_rate) :int
  (dev :pointer)
  (ch :int)
  (rate :unsigned-int)
  (actual :pointer))

(defun set-sample-rate (device channel rate)
  (with-foreign-object (actual :unsigned-int)
    (let ((status (bladerf_set_sample_rate (mem-ref device :pointer) channel rate actual)))
      (if (< status 0)
	  (error "Failed to set sample rate error: ~S " status)
	  (mem-ref actual :unsigned-int)))))

(defcfun ("bladerf_set_rational_sample_rate" bladerf_set_rational_sample_rate) :int
  (dev :pointer)
  (ch :int)
  (rate :pointer)
  (actual :pointer))

(defun set-rational-sample-rate (device channel int numerator denominator)
  (with-foreign-objects ((rate '(:struct bladerf_rational_rate))
			 (actual '(:struct bladerf_rational_rate)))
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

(defun get-sample-rate (device channel)
  (with-foreign-object (rate :unsigned-int)
    (let ((status (bladerf_get_sample_rate (mem-ref device :pointer) channel rate)))
      (if (< status 0)
	  (error "Failed to get channel's current sample rate in Hz error: ~S" status)
	  (mem-ref rate :unsigned-int)))))

(defcfun ("bladerf_get_sample_rate_range" bladerf_get_sample_rate_range) :int
  (dev :pointer)
  (ch :int)
  (range :pointer))

(defun get-sample-rate-range (device channel)
  (with-foreign-object (range :pointer)
    (let ((status (bladerf_get_sample_rate_range (mem-ref device :pointer) channel range)))
      (if (< status 0)
	  (error "Failed to get the channel's supported range of sample rates error: ~S" status)
	  (mem-ref (mem-ref range :pointer) '(:struct bladerf_range))))))

(defcfun ("bladerf_get_rational_sample_rate" bladerf_get_rational_sample_rate) :int
  (dev :pointer)
  (ch :int)
  (rate :pointer))

(defun get-rational-sample-rate (device channel)
  (with-foreign-object (rate '(:struct bladerf_rational_rate))
    (let ((status (bladerf_get_rational_sample_rate (mem-ref device :pointer) channel rate)))
      (if (< status 0)
	  (error "Failed to get channel: ~S sample rate in rational Hz error ~S" channel status)
	  (with-foreign-slots ((integer num den) rate (:struct bladerf_rational_rate))
	    (list :integer integer :numerator num :denominator den))))))

;; Bandwidth
(defcfun ("bladerf_set_bandwidth" bladerf_set_bandwidth) :int
  (dev :pointer)
  (ch :int)
  (bandwidth :unsigned-int)
  (actual :pointer))

(defun set-bandwidth (device channel bandwidth)
  (with-foreign-object (actual :unsigned-int)
    (let ((status (bladerf_set_bandwidth (mem-ref device :pointer) channel bandwidth actual)))
      (if (< status 0)
	  (error "Failed to set bandwidth of channel: ~S to ~S Hz" channel bandwidth)
	  (mem-ref actual :unsigned-int)))))

(defcfun ("bladerf_get_bandwidth" bladerf_get_bandwidth) :int
  (dev :pointer)
  (ch :int)
  (bandwidth :pointer))

(defun get-bandwidth (device channel)
  (with-foreign-object (bandwidth :unsigned-int)
    (let ((status (bladerf_get_bandwidth (mem-ref device :pointer) channel bandwidth)))
      (if (< status 0)
	  (error "Failed to get bandwidth of channel ~S error: ~S" channel status)
	  (mem-ref bandwidth :unsigned-int)))))

(defcfun ("bladerf_get_bandwidth_range" bladerf_get_bandwidth_range) :int
  (dev :pointer)
  (ch :int)
  (range :pointer))

(defun get-bandwidth-range (device channel)
  (with-foreign-object (range :pointer)
    (let ((status (bladerf_get_bandwidth_range (mem-ref device :pointer) channel range)))
      (if (< status 0)
	  (error "Failed to get supported range of bandwidths for channel: ~S error: ~S" channel status)
	  (mem-ref (mem-ref range :pointer) '(:struct bladerf_range))))))

;; Frequency tuning
(defcfun ("bladerf_select_band" bladerf_select_band) :int
  (dev :pointer)
  (ch :int)
  (frequency :uint64))

(defun select-band (device channel frequency)
  (let ((status (bladerf_select_band (mem-ref device :pointer) channel frequency)))
    (if (< status 0)
	(error "Failed to select band path for channel: ~S error: ~S" channel status)
	t)))

(defcfun ("bladerf_set_frequency" bladerf_set_frequency) :int
  (dev :pointer)
  (ch :int)
  (frequency :uint64))

(defun set-frequency (device channel frequency)
  (let ((status (bladerf_set_frequency (mem-ref device :pointer) channel frequency)))
    (if (< status 0)
	(error "Failed to set channel ~S frequency to ~S Hz error: ~S" channel frequency status)
	t)))

(defcfun ("bladerf_get_frequency" bladerf_get_frequency) :int
  (dev :pointer)
  (ch :int)
  (frequency :pointer))

(defun get-frequency (device channel)
  (with-foreign-object (frequency :uint64)
    (let ((status (bladerf_get_frequency (mem-ref device :pointer) channel frequency)))
      (if (< status 0)
	  (error "Failed to get channel: ~S frequency error: ~S" channel status)
	  (mem-ref frequency :uint64)))))

(defcfun ("bladerf_get_frequency_range" bladerf_get_frequency_range) :int
  (dev :pointer)
  (ch :int)
  (range :pointer))

(defun get-frequency-range (device channel)
  (with-foreign-object (range :pointer)
    (let ((status (bladerf_get_frequency_range (mem-ref device :pointer) channel range)))
      (if (< status 0)
	  (error "Failed to get supported range of frequencies for channel ~S error: ~S" channel status)
	  (mem-ref (mem-ref range :pointer) '(:struct bladerf_range))))))

;; Loopback
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
  (modes :pointer))

(defun get-loopback-modes (device)
  (with-foreign-object (modes :pointer)
    (let ((number-of-modes (bladerf_get_loopback_modes (mem-ref device :pointer) modes)))
      (if (< number-of-modes 0)
	  (error "Failed to get loopback modes error: ~S" number-of-modes)
	  (if (> number-of-modes 0)
	      (loop for i from 0 to (1- number-of-modes)
		 collecting
		   (mem-ref (mem-aref (mem-ref modes :pointer) :pointer i) '(:struct bladerf_loopback_modes))))))))

(defcfun ("bladerf_is_loopback_mode_supported" bladerf_is_loopback_mode_supported) :boolean
  (dev :pointer)
  (mode bladerf_loopback))

(defun is-loopback-mode-supported-p (device mode)
  (bladerf_is_loopback_mode_supported (mem-ref device :pointer) mode))

(defcfun ("bladerf_set_loopback" bladerf_set_loopback) :int
  (dev :pointer)
  (lb bladerf_loopback))

(defun set-loopback (device loopback)
  (let ((status (bladerf_set_loopback (mem-ref device :pointer) loopback)))
    (if (< status 0)
	(error "Failed to set loopback mode to: ~S error: ~S" loopback status)
	t)))

(defcfun ("bladerf_get_loopback" bladerf_get_loopback) :int
  (dev :pointer)
  (lb :pointer))

(defun get-loopback (device)
  (with-foreign-object (lb 'bladerf_loopback)
    (let ((status (bladerf_get_loopback (mem-ref device :pointer) lb)))
      (if (< status 0)
	  (error "Failed to get current loopback mode error: ~S" status)
	  (mem-ref lb 'bladerf_loopback)))))

;; Triggers
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
  (options :uint64))

(defcfun ("bladerf_trigger_init" bladerf_trigger_init) :int
  (dev :pointer)
  (ch :int)
  (signal bladerf_trigger_signal)
  (trigger :pointer))

(defcfun ("bladerf_trigger_arm" bladerf_trigger_arm) :int
  (dev :pointer)
  (trigger :pointer)
  (arm :boolean)
  (resv1 :uint64)
  (resv2 :uint64))

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

;; RX Mux
(defcenum bladerf_rx_mux
  (:BLADERF_RX_MUX_INVALID #.-1)
  (:BLADERF_RX_MUX_BASEBAND #.#x0)
  (:BLADERF_RX_MUX_12BIT_COUNTER #.#x1)
  (:BLADERF_RX_MUX_32BIT_COUNTER #.#x2)
  (:BLADERF_RX_MUX_DIGITAL_LOOPBACK #.#x4))

(defcfun ("bladerf_set_rx_mux" bladerf_set_rx_mux) :int
  (dev :pointer)
  (mux bladerf_rx_mux))

(defun set-rx-mux (device mux)
  (let ((status (bladerf_set_rx_mux (mem-ref device :pointer) mux)))
    (if (< status 0)
	(error "Failed to set RX Mux mode to ~S  error: ~S" mux status)
	t)))

(defcfun ("bladerf_get_rx_mux" bladerf_get_rx_mux) :int
  (dev :pointer)
  (mode :pointer))

(defun get-rx-mux (device)
  (with-foreign-object (mode 'bladerf_rx_mux)
    (let ((status (bladerf_get_rx_mux (mem-ref device :pointer) mode)))
      (if (< status 0)
	  (error "Failed to get current RX Mux mode error: ~S" status)
	  (mem-ref mode 'bladerf_rx_mux)))))

;; Scheduled tuning
(defconstant BLADERF_RETUNE_NOW 0)

(defcstruct bladerf_quick_tune
  ;; Union of bladeRF1 and bladeRF2 parameters
  ;; For bladeRF1:
  (freqsel :uint8)
  (vcocap :uint8)
  (nint :uint16)
  (nfrac :uint32)
  (flags :uint8)
  (xb_gpio :uint8)
  ;; The following are actually for bladeRF2 and overlay the same memory:
  ;; (nios_profile :uint16)
  ;; (rffe_profile :uint8) 
  ;; (port :uint8)
  ;; (spdt :uint8)
  )

(defcfun ("bladerf_schedule_retune" bladerf_schedule_retune) :int
  (dev :pointer)
  (ch :int)
  (timestamp :uint64)
  (frequency :uint64)
  (quick_tune :pointer))

(defun schedule-retune (device channel timestamp frequency quick-tune)
  (let ((status (bladerf_schedule_retune (mem-ref device :pointer) channel timestamp frequency quick-tune)))
    (if (< status 0)
	(error "Failed to schedule a frequency retune to occur at sample with timestamp: ~S error: ~S" timestamp status)
	t)))

(defcfun ("bladerf_cancel_scheduled_retunes" bladerf_cancel_scheduled_retunes) :int
  (dev :pointer)
  (ch :int))

(defun cancel-scheduled-retunes (device channel)
  (let ((status (bladerf_cancel_scheduled_retunes (mem-ref device :pointer) channel)))
    (if (< status 0)
	(error "Failed to cancel scheduled retune operations for the channel ~S  error: ~S" channel status)
	t)))

(defcfun ("bladerf_get_quick_tune" bladerf_get_quick_tune) :int
  (dev :pointer)
  (ch :int)
  (quick_tune :pointer))

(defun get-quick-tune (device channel)
  (with-foreign-object (quick-tune '(:struct bladerf_quick_tune))
    (let ((status (bladerf_get_quick_tune (mem-ref device :pointer) channel quick-tune)))
      (if (< status 0)
	  (error "Failed to fetch quick tune parameters error: ~S" status)
	  quick-tune))))

;; Correction
(defcenum bladerf_correction
  :BLADERF_CORR_DCOFF_I
  :BLADERF_CORR_DCOFF_Q
  :BLADERF_CORR_PHASE
  :BLADERF_CORR_GAIN)

(defcfun ("bladerf_set_correction" bladerf_set_correction) :int
  (dev :pointer)
  (ch :int)
  (corr bladerf_correction)
  (value :int16))

(defun set-correction (device channel correction value)
  (let ((status (bladerf_set_correction (mem-ref device :pointer) channel correction value)))
    (if (< status 0)
	(error "Failed to set the value of configuration parameter error: ~S" status)
	(= status 0))))

(defcfun ("bladerf_get_correction" bladerf_get_correction) :int
  (dev :pointer)
  (ch :int)
  (corr bladerf_correction)
  (value :pointer))

(defun get-correction (device channel correction)
  (with-foreign-object (value :int16)
    (let ((status (bladerf_get_correction (mem-ref device :pointer) channel correction value)))
      (if (< status 0)
	  (error "Failed to obtain current value for configuration parameter: ~S error: ~S" correction status)
	  (mem-ref value :int16)))))

;; Streaming format
(defcenum bladerf_format
  :BLADERF_FORMAT_SC16_Q11
  :BLADERF_FORMAT_SC16_Q11_META
  :BLADERF_FORMAT_PACKET_META
  :BLADERF_FORMAT_SC8_Q7
  :BLADERF_FORMAT_SC8_Q7_META)

;; Metadata flags
(defconstant BLADERF_META_STATUS_OVERRUN (ash 1 0))
(defconstant BLADERF_META_STATUS_UNDERRUN (ash 1 1))
(defconstant BLADERF_META_FLAG_TX_BURST_START (ash 1 0))
(defconstant BLADERF_META_FLAG_TX_BURST_END (ash 1 1))
(defconstant BLADERF_META_FLAG_TX_NOW (ash 1 2))
(defconstant BLADERF_META_FLAG_TX_UPDATE_TIMESTAMP (ash 1 3))
(defconstant BLADERF_META_FLAG_RX_NOW (ash 1 31))
(defconstant BLADERF_META_FLAG_RX_HW_UNDERFLOW (ash 1 0))
(defconstant BLADERF_META_FLAG_RX_HW_MINIEXP1 (ash 1 16))
(defconstant BLADERF_META_FLAG_RX_HW_MINIEXP2 (ash 1 17))

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

(defun interleave-stream-buffer (layout format buffer-size samples)
  (let ((status (bladerf_interleave_stream_buffer layout format buffer-size samples)))
    (if (< status 0)
	(error "Failed to interleave samples error: ~S" status)
	(= status 0))))

(defcfun ("bladerf_deinterleave_stream_buffer" bladerf_deinterleave_stream_buffer) :int
  (layout bladerf_channel_layout)
  (format bladerf_format)
  (buffer_size :unsigned-int)
  (samples :pointer))

(defun deinterleave-stream-buffer (layout format buffer-size samples)
  (let ((status (bladerf_deinterleave_stream_buffer layout format buffer-size samples)))
    (if (< status 0)
	(error "Failed to deinterleave samples error: ~S" status)
	(= status 0))))

(defcfun ("bladerf_enable_module" bladerf_enable_module) :int
  (dev :pointer)
  (ch :int)
  (enable :boolean))

(defun enable-module (device channel enable)
  (let ((status (bladerf_enable_module (mem-ref device :pointer) channel enable)))
    (if (< status 0)
	(error "Failed to enable module error: ~S" status)
	(= status 0))))

(defcfun ("bladerf_get_timestamp" bladerf_get_timestamp) :int
  (dev :pointer)
  (dir bladerf_direction)
  (timestamp :pointer))

(defun get-timestamp (device direction)
  (with-foreign-object (timestamp :uint64)
    (let ((status (bladerf_get_timestamp (mem-ref device :pointer) direction timestamp)))
      (if (< status 0)
	  (error "Failed to get timestamp error: ~S" status)
	  (mem-ref timestamp :uint64)))))

(defcfun ("bladerf_sync_config" bladerf_sync_config) :int
  (dev :pointer)
  (layout bladerf_channel_layout)
  (format bladerf_format)
  (num_buffers :unsigned-int)
  (buffer_size :unsigned-int)
  (num_transfers :unsigned-int)
  (stream_timeout :unsigned-int))

(defun sync-config (device layout format num-buffers buffer-size num-transfers stream-timeout)
  (let ((status (bladerf_sync_config (mem-ref device :pointer) layout format num-buffers buffer-size num-transfers stream-timeout)))
    (if (< status 0)
	(error "Failed to (Re)Configure device error: ~S" status)
	(= status 0))))

(defcfun ("bladerf_sync_tx" bladerf_sync_tx) :int
  (dev :pointer)
  (samples :pointer)
  (num_samples :unsigned-int)
  (metadata :pointer)
  (timeout_ms :unsigned-int))

(defun sync-tx (device samples num-samples metadata timeout-ms)
  (let ((status (bladerf_sync_tx (mem-ref device :pointer) samples num-samples metadata timeout-ms)))
    (if (< status 0)
	(error "Failed to transmit IQ samples error: ~S" status)
	(= status 0))))

(defcfun ("bladerf_sync_rx" bladerf_sync_rx) :int
  (dev :pointer)
  (samples :pointer)
  (num_samples :unsigned-int)
  (metadata :pointer)
  (timeout_ms :unsigned-int))

(defun sync-rx (device number-of-samples timeout-ms)
  (let ((samples-to-allocate (* number-of-samples 2)))
    (with-foreign-objects ((rx-samples :int16 samples-to-allocate)
			   (metadata '(:struct bladerf_metadata)))
      (let ((status (bladerf_sync_rx (mem-ref device :pointer) rx-samples number-of-samples metadata timeout-ms))
	    (samples-array (make-array number-of-samples :element-type '(complex float))))
	(if (< status 0)
	    (error "Failed to receive IQ samples error: ~S" status)
	    (progn
	      (loop for i from 0 below number-of-samples
		 do (setf (aref samples-array i)
			  (complex
			   (mem-aref rx-samples :int16 (* i 2))
			   (mem-aref rx-samples :int16 (1+ (* i 2))))))
	      samples-array))))))

(defun sync-rx-complex-double-float (device number-of-samples timeout-ms)
  (let ((samples-to-allocate (* number-of-samples 2)))
    (with-foreign-objects ((rx-samples :int16 samples-to-allocate)
			   (metadata '(:struct bladerf_metadata)))
      (let ((status (bladerf_sync_rx (mem-ref device :pointer) rx-samples number-of-samples metadata timeout-ms))
	    (samples-array (make-array number-of-samples :element-type '(complex double-float))))
	(if (< status 0)
	    (error "Failed to receive IQ samples error: ~S" status)
	    (progn
	      (loop for i from 0 below number-of-samples
		 do (setf (aref samples-array i)
			  (complex
			   (coerce (mem-aref rx-samples :int16 (* i 2)) 'double-float)
			   (coerce (mem-aref rx-samples :int16 (1+ (* i 2))) 'double-float))))
	      samples-array))))))

;; Asynchronous streaming 
(defcstruct bladerf_stream)

(defcfun ("bladerf_init_stream" bladerf_init_stream) :int
  (stream :pointer)
  (dev :pointer)
  (callback :pointer)
  (buffers :pointer)
  (num_buffers :uint)
  (format bladerf_format)
  (samples_per_buffer :uint)
  (num_transfers :uint)
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

;; Firmware and FPGA loading
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
  (bus :uint8)
  (addr :uint8)
  (file :string))

;; Flash image types
(defcenum bladerf_image_type
	(:BLADERF_IMAGE_TYPE_INVALID #.-1)
	:BLADERF_IMAGE_TYPE_RAW
	:BLADERF_IMAGE_TYPE_FIRMWARE
	:BLADERF_IMAGE_TYPE_FPGA_40KLE
	:BLADERF_IMAGE_TYPE_FPGA_115KLE
	(:BLADERF_IMAGE_TYPE_FPGA_A4 #.6)
	(:BLADERF_IMAGE_TYPE_FPGA_A9 #.7)
	:BLADERF_IMAGE_TYPE_CALIBRATION
	:BLADERF_IMAGE_TYPE_RX_DC_CAL
	:BLADERF_IMAGE_TYPE_TX_DC_CAL
	:BLADERF_IMAGE_TYPE_RX_IQ_CAL
	:BLADERF_IMAGE_TYPE_TX_IQ_CAL
	(:BLADERF_IMAGE_TYPE_FPGA_A5 #.13))

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
  (dev :pointer)
  (type bladerf_image_type)
  (address :uint32)
  (length :uint32))

(defcfun ("bladerf_alloc_cal_image" bladerf_alloc_cal_image) :pointer
  (dev :pointer)
  (fpga_size bladerf_fpga_size)
  (vctcxo_trim :uint16))

(defcfun ("bladerf_free_image" bladerf_free_image) :void
  (image :pointer))

(defcfun ("bladerf_image_write" bladerf_image_write) :int
  (dev :pointer)
  (image :pointer)
  (file :string))

(defcfun ("bladerf_image_read" bladerf_image_read) :int
  (image :pointer)
  (file :string))

;; VCTCXO Tamer
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
  (val :uint16))

(defcfun ("bladerf_trim_dac_read" bladerf_trim_dac_read) :int
  (dev :pointer)
  (val :pointer))

;; Tuning mode
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

;; Trigger control
(defconstant BLADERF_TRIGGER_REG_ARM (ash 1 0))
(defconstant BLADERF_TRIGGER_REG_FIRE (ash 1 1))
(defconstant BLADERF_TRIGGER_REG_MASTER (ash 1 2))
(defconstant BLADERF_TRIGGER_REG_LINE (ash 1 3))

(defcfun ("bladerf_read_trigger" bladerf_read_trigger) :int
  (dev :pointer)
  (ch :int)
  (signal bladerf_trigger_signal)
  (val :pointer))

(defcfun ("bladerf_write_trigger" bladerf_write_trigger) :int
  (dev :pointer)
  (ch :int)
  (signal bladerf_trigger_signal)
  (val :uint8))

;; Wishbone Master
(defcfun ("bladerf_wishbone_master_read" bladerf_wishbone_master_read) :int
  (dev :pointer)
  (addr :uint32)
  (data :pointer))

(defcfun ("bladerf_wishbone_master_write" bladerf_wishbone_master_write) :int
  (dev :pointer)
  (addr :uint32)
  (val :uint32))

;; Config GPIO
(defcfun ("bladerf_config_gpio_read" bladerf_config_gpio_read) :int
  (dev :pointer)
  (val :pointer))

(defcfun ("bladerf_config_gpio_write" bladerf_config_gpio_write) :int
  (dev :pointer)
  (val :uint32))

;; SPI Flash
(defcfun ("bladerf_erase_flash" bladerf_erase_flash) :int
  (dev :pointer)
  (erase_block :uint32)
  (count :uint32))

(defcfun ("bladerf_erase_flash_bytes" bladerf_erase_flash_bytes) :int
  (dev :pointer)
  (address :uint32)
  (length :uint32))

(defcfun ("bladerf_read_flash" bladerf_read_flash) :int
  (dev :pointer)
  (buf :pointer)
  (page :uint32)
  (count :uint32))

(defcfun ("bladerf_read_flash_bytes" bladerf_read_flash_bytes) :int
  (dev :pointer)
  (buf :pointer)
  (address :uint32)
  (bytes :uint32))

(defcfun ("bladerf_write_flash" bladerf_write_flash) :int
  (dev :pointer)
  (buf :pointer)
  (page :uint32)
  (count :uint32))

(defcfun ("bladerf_write_flash_bytes" bladerf_write_flash_bytes) :int
  (dev :pointer)
  (buf :pointer)
  (address :uint32)
  (length :uint32))

(defcfun ("bladerf_lock_otp" bladerf_lock_otp) :int
  (dev :pointer))

(defcfun ("bladerf_read_otp" bladerf_read_otp) :int
  (dev :pointer)
  (buf :pointer))

(defcfun ("bladerf_write_otp" bladerf_write_otp) :int
  (dev :pointer)
  (buf :pointer))

;; RF Ports
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

;; Features
(defcenum bladerf_feature
  (:BLADERF_FEATURE_DEFAULT #.0)
  :BLADERF_FEATURE_OVERSAMPLE)

(defcfun ("bladerf_enable_feature" bladerf_enable_feature) :int
  (dev :pointer)
  (feature bladerf_feature)
  (enable :boolean))

(defcfun ("bladerf_get_feature" bladerf_get_feature) :int
  (dev :pointer)
  (feature :pointer))

;; Expansion boards
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

;; Logging
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

;; Library version
(defcfun ("bladerf_version" bladerf_version) :void
  (version :pointer))

;; Error codes
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

;; Helper functions
(defun minimum-buffer-size (number-of-samples number-of-channels)
  (* 2 number-of-samples number-of-channels (foreign-type-size :int16)))

(defun buffer-length (number-of-samples number-of-channels)
