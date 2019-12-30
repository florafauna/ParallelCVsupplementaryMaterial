#!/bin/bash

sudo -i scontrol update node=gc-compute[1-256] state=power_down
