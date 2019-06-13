# tanks

## Dependecies
You need to have the following libraries installed in your system:
- sdl2
- sdl2-image

## Troubleshooting
BadValue (integer parameter out of range for operation)

Add `+iglx` to `lightdm` config
https://askubuntu.com/questions/801440/login-loop-badvalue-integer-parameter-out-of-range-for-operation-16-04

## Running
```
stack build tanks
stack exec tanks-exe
```