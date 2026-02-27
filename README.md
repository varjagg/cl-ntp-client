# CL-NTP-CLIENT

[![Tests](https://github.com/varjagg/cl-ntp-client/actions/workflows/tests.yml/badge.svg)](https://github.com/varjagg/cl-ntp-client/actions/workflows/tests.yml)

A simple NTP (Network Time Protocol) client written in Common Lisp.

Does not adjust system clock, PLL drift or do any other platform specific work, nor does any statistical evaluation of time source or system clock jitter.

 ## Usage

Instantiate an `NTP` object:

`(defvar o (make-intance 'ntp))`

Synchronize the object with NTP server:

`(synchronize o)`

After that, `(get-adjusted-universal-time o)` will return you a pair of NTP timestamp values: seconds and second fraction.

## API

### `(synchronize ntp-instance &optional (server "pool.ntp.org"))`

Synchronizes the application clock with remote server.

### `(get-adjusted-universal-time ntp-instance)`

Returns application time as a pair of values, corresponding to universal time in seconds and NTP formatted 32-bit fraction of a second.

### `(local-stratum ntp-instance)`

Returns the stratum of application clock.
