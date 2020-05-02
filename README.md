# pisend source
 The unwieldy source for pisend

pisend is an evolution of constantly trying to nail down the uart/pi0. Its not perfect at all but I'm hoping making this open source will help other people how talking to the pi0 can be achieved. 

pisend-v15.asm 
- the latest 2.20

Thanks go to Kev Brady for the UUENCODING, Tim Gillberts for the baud selection routines, 32bitdisplay by alwin henseler, div32_16 by fgsgh. 

Please dont submit corrections for things that look like redundant code, only actual areas of improvement. 

Also included in checkpi.asm which is an attempt to clean up, and checkpi-tostring.asm is an attempt to get a string back.

The checkpi ones are just examples and hopefully will help you work out how to talk with NextPi to some degree.

This source if free to be used in anyway but any diritives must also be open source. 
