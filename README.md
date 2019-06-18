# tanks

## Dependecies
You need to have the following libraries installed in your system:
- sdl2
- sdl2-image

## Troubleshooting
> BadValue (integer parameter out of range for operation)

Add `+iglx` to `lightdm` config
https://askubuntu.com/questions/801440/login-loop-badvalue-integer-parameter-out-of-range-for-operation-16-04

## Running
```
stack build
stack exec server-exe
stack exec client-exe <server-address>
```

Client accepts only IP as server address. Host names will not be resolved