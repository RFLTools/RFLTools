CORNER : dialog {
                  label = "RFL Corner Creator";
                  initial_focus = "STATION";
                  : column {
                             alignment = centered;
                             : row {
                                     : boxed_column {
                                                      : row {
                                                              : radio_column {
                                                                               : radio_button {
                                                                                                label = "Spiral";
                                                                                                key = "S";
                                                                                                value = "0";
                                                                                                is_enabled = "0";
                                                                                              }
                                                                               : radio_button {
                                                                                                label = "Arc";
                                                                                                key = "A";
                                                                                                value = "1";
                                                                                              }
                                                                             }
                                                              : column {
                                                                         : edit_box {
                                                                                      label = "Radius in:";
                                                                                      key = "RIN";
                                                                                      value = "0.0";
                                                                                      edit_width = 10;
                                                                                      width = 15;
                                                                                    }
                                                                         : edit_box {
                                                                                      label = "Radius out:";
                                                                                      key = "ROUT";
                                                                                      value = "0.0";
                                                                                      edit_width = 10;
                                                                                      width = 15;
                                                                                      is_enabled = "0";
                                                                                    }
                                                                       }
                                                            }
                                                    }
                                     : boxed_column {
                                                      : row {
                                                              : radio_column {
                                                                               : radio_button {
                                                                                                label = "Length";
                                                                                                key = "L";
                                                                                                value = "0";
                                                                                              }
                                                                               : radio_button {
                                                                                                label = "Delta";
                                                                                                key = "D";
                                                                                                value = "1";
                                                                                              }
                                                                             }
                                                              : edit_box {
                                                                           label = "";
                                                                           key = "V";
                                                                           value = "0.0";
                                                                           edit_width = 10;
                                                                         }
                                                              : toggle {
                                                                         label = "Calc";
                                                                         key = "CALC";
                                                                         value = "1";
                                                                       }
                                                            }
                                                    }
                                   }
                             : row {
                                     : button {
                                                key = "SET";
                                                label = "Set";
                                                width = 5;
                                              }
                                     : popup_list {
                                                    key = "STANDARD";
                                                    label = "";
                                                    list = "Last\n440 - 50 - 15 (calc) - 50\n50 - 15 (calc) - 50 - 440\n36 - 12 (calc) - 36\n50 - 15 (calc) - 50\n55 - 18 (calc) - 55";
                                                    value = "0";
                                                    width = 50;
                                                  }
                                   }
                             : row {
                                     : cancel_button {
                                                       key = "CANCEL";
                                                       label = "Cancel";
                                                       alignment = left;
                                                       width = 12;
                                                     }
                                     : button {
                                                key = "DOWN";
                                                label = "<";
                                                is_enabled = "0";
                                              }
                                     : text {
                                              label = "";
                                              key = "NODE";
                                              value = "1";
                                              width = 3;
                                            }
                                     : button {
                                                key = "UP";
                                                label = ">";
                                              }
                                     : text {
                                              key = "DELTA";
                                              value = "Delta : 123d12'12.45\"";
                                              width = 18;
                                            }
                                     : button {
                                                key = "DELETE";
                                                label = "Delete";
                                                alignment = right;
                                                width = 12;
                                                is_enabled = "0";
                                              }
                                     : ok_button {
                                                   key = "OK";
                                                   label = "OK";
                                                   alignment = right;
                                                   width = 12;
                                                 }
                                   }
                           }
                }
