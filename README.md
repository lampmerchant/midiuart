# midiuart

PIC12F1501 firmware to convert between MIDI (31250 baud UART) and 9600 baud UART.


## Technical Details

### Connections

```
                      .--------.
              Supply -|01 \/ 08|- Ground
    MIDI Tx <--- RA5 -|02    07|- RA0 <--- UART Rx
MIDI Rx LED <--- RA4 -|03    06|- RA1 ---> UART Rx LED
    MIDI Rx ---> RA3 -|04    05|- RA2 ---> UART Tx
                      '--------'
```

LEDs are active low.


### Building Firmware

Building the firmware requires Microchip MPASM, which is included with their development environment, MPLAB.  Note that you **must** use MPLAB X version 5.35 or earlier or MPLAB 8 as later versions of MPLAB X have removed MPASM.
