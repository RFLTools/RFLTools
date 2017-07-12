RAMP : dialog {
                label = "RFL Exit Ramp";
                : row {
                        : image {
                                  key = "IMAGE1";
                                  width = 120;
                                  height = 24;
                                  color = 0;
                                }
                      }
                : row {
                        : boxed_column {
                                         label = "Ramp Taper";
                                         width = 20;
                                         : edit_box {
                                                      label = "Lane Offset";
                                                      key = "LANEOFFSET";
                                                      edit_width = 6;
                                                      alignment = right;
                                                    }
                                         : edit_box {
                                                      label = "Ramp Taper (X:1)";
                                                      key = "RAMPTAPER";
                                                      edit_width = 6;
                                                    }
                                         : edit_box {
                                                      label = "Ramp Verticies";
                                                      key = "RAMPVERTICIES";
                                                      edit_width = 6;
                                                    }
                                         : edit_box {
                                                      label = "Weave Offset";
                                                      key = "WEAVEOFFSET";
                                                      edit_width = 6;
                                                    }
                                       }
                        : boxed_column {
                                         label = "Inside Taper";
                                         width = 20;
                                         : edit_box {
                                                      label = "Gore Lane Width";
                                                      key = "GOREWIDTH";
                                                      edit_width = 6;
                                                    }
                                         : edit_box {
                                                      label = "Taper Offset";
                                                      key = "TAPEROFFSET";
                                                      edit_width = 6;
                                                    }
                                         : edit_box {
                                                      label = "Inside Verticies";
                                                      key = "INSIDEVERTICIES";
                                                      edit_width = 6;
                                                    }
                                       }
                        : boxed_column {
                                         label = "Nose";
                                         width = 20;
                                         : row {
                                                 : edit_box {
                                                              label = "Nose Station";
                                                              key = "NOSESTATION";
                                                              edit_width = 6;
                                                            }
                                                 : button {
                                                            label = "Pick";
                                                            key = "PICK";
                                                          }
                                               }
                                         : edit_box {
                                                      label = "Nose Offset";
                                                      key = "NOSEOFFSET";
                                                      edit_width = 6;
                                                    }
                                         : edit_box {
                                                      label = "Nose Radius";
                                                      key = "NOSERADIUS";
                                                      edit_width = 6;
                                                    }
                                         : edit_box {
                                                      label = "Inside Shoulder";
                                                      key = "INSIDESHOULDER";
                                                      edit_width = 6;
                                                    }
                                         : edit_box {
                                                      label = "Lane Width + Shldr.";
                                                      key = "LANEWIDTH";
                                                      edit_width = 6;
                                                    }
                                       }
                        : boxed_column {
                                         label = "Return Taper";
                                         width = 20;
                                         : edit_box {
                                                      label = "Return Taper (X:1)";
                                                      key = "RETURNTAPER";
                                                      edit_width = 6;
                                                    }
                                         : edit_box {
                                                      label = "Return Offset";
                                                      key = "RETURNOFFSET";
                                                      edit_width = 6;
                                                    }
                                         : edit_box {
                                                      label = "Return Verticies";
                                                      key = "RETURNVERTICIES";
                                                      edit_width = 6;
                                                    }
                                       }
                      }
                : row {
                        : ok_button {
                                      label = "OK";
                                      key = "OK";
                                      is_default = true;
                                    }
                        : column {
                                   : radio_button {
                                                    label = "Single Lane";
                                                    key = "SINGLELANE";
                                                    value = "1";
                                                  }
                                   : radio_button {
                                                    label = "Dual Lane";
                                                    key = "DUALLANE";
                                                  }
                                            }
                        : column {
                                   : radio_button {
                                                    label = "Up Chainage";
                                                    key = "UPCHAINAGE";
                                                    value = "1";
                                                  }
                                   : radio_button {
                                                    label = "Down Chainage";
                                                    key = "DOWNCHAINAGE";
                                                  }
                                            }
                        : column {
                                   : radio_button {
                                                    label = "Exit Terminal";
                                                    key = "EXITTERMINAL";
                                                    value = "1";
                                                  }
                                   : radio_button {
                                                    label = "Entrance Terminal";
                                                    key = "ENTRANCETERMINAL";
                                                  }
                                 }
                        : column {
                                   : radio_button {
                                                    label = "Right Side";
                                                    key = "RIGHTSIDE";
                                                    value = "1";
                                                  }
                                   : radio_button {
                                                    label = "Left Side";
                                                    key = "LEFTSIDE";
                                                  }
                                 }
                        : cancel_button {
                                          label = "Cancel";
                                          key = "CANCEL";
                                        }
                      }
              }