XYP : dialog {
               label = "RFL X-Y Locator";
               initial_focus = "STATION";
               : column {
                          alignment = centered;
                          : boxed_row {
                                        label = "Draw single-point:";
                                        : column {
                                                   : row {
                                                           : edit_box {
                                                                        label = "Sta:";
                                                                        key = "STATION";
                                                                        value = "";
                                                                      }
                                                           : edit_box {
                                                                        label = "O/S:";
                                                                        key = "OFFSET";
                                                                        value = "";
                                                                      }
                                                         }
                                                   : row {
                                                           : edit_box {
                                                                        label = "  N:";
                                                                        key = "NORTHING";
                                                                        value = "Northing";
                                                                      }
                                                           : edit_box {
                                                                        label = "  E:";
                                                                        key = "EASTING";
                                                                        value = "Easting";
                                                                      }
                                                         }
                                                   : row {
                                                           : button {
                                                                      label = "Pick";
                                                                      key = "PICK";
                                                                      width = 5;
                                                                    }
                                                           : button {
                                                                      key = "OK";
                                                                      label = "Draw";
                                                                      width = 35;
                                                                    }
                                                         }
                                                 }
                                      }
                          : boxed_row {
                                        label = "Draw multi-point:";
                                        : column {
                                                   : row {
                                                           : edit_box {
                                                                        label = "Steps:";
                                                                        key = "STEP";
                                                                        edit_width = 3;
                                                                        value = "";
                                                                        width = 10;
                                                                      }
                                                           : spacer {
                                                                      width = 30;
                                                                    }
                                                         }
                                                   : row {
                                                           : edit_box {
                                                                        label = "Sta:";
                                                                        key = "FROMSTATION";
                                                                        value = "";
                                                                      }
                                                           : edit_box {
                                                                        label = "O/S:";
                                                                        key = "FROMOFFSET";
                                                                        value = "";
                                                                      }
                                                         }
                                                   : row {
                                                           : edit_box {
                                                                        label = "Sta:";
                                                                        key = "TOSTATION";
                                                                        value = "";
                                                                      }
                                                           : edit_box {
                                                                        label = "O/S:";
                                                                        key = "TOOFFSET";
                                                                        value = "";
                                                                      }
                                                         }
                                                   : row {
                                                           : button {
                                                                      label = "Pick";
                                                                      key = "MPICK";
                                                                      width = 5;
                                                                    }
                                                           : button {
                                                                      key = "DRAW";
                                                                      width = 30;
                                                                      label = "Draw";
                                                                      width = 35;
                                                                    }
                                                         }
                                                   : row {
                                                         }
                                                 }
                                      }
                          : boxed_row {
                                        label = "Draw points from file:";
                                        : column {
                                                   : row {
                                                           : button {
                                                                      key = "FROMFILE";
                                                                      label = "From File (Sta,O/S : Comma delim.)";
                                                                      width = 20;
                                                                    }
                                                         }
                                                 }
                                      }
                          : boxed_row {
                                        label = "Draw section lines:";
                                        : column {
                                                   : row {
                                                           : edit_box {
                                                                        label = "Sta:";
                                                                        key = "XFROMSTATION";
                                                                        value = "";
                                                                        edit_width = 15;
                                                                      }
                                                           : edit_box {
                                                                        label = " W:";
                                                                        key = "XSWATH";
                                                                        value = "";
                                                                        edit_width = 5;
                                                                      }
                                                           : toggle {
                                                                      label = "Round";
                                                                      key = "XROUND";
                                                                      value = "1";
                                                                    }
                                                           }
                                                   : row {
                                                           : edit_box {
                                                                        label = "Sta:";
                                                                        key = "XTOSTATION";
                                                                        value = "";
                                                                        edit_width = 15;
                                                                      }
                                                           : edit_box {
                                                                        label = "Inc:";
                                                                        key = "XINC";
                                                                        value = "";
                                                                        edit_width = 5;
                                                                      }
                                                           : toggle {
                                                                      label = "TS/SC";
                                                                      key = "XSPECIAL";
                                                                      value = "0";
                                                                    }
                                                         }
                                                   : row {
                                                           : button {
                                                                      label = "Pick";
                                                                      key = "XPICK";
                                                                      width = 5;
                                                                    }
                                                           : button {
                                                                      label = "Draw";
                                                                      key = "XDRAW";
                                                                      width = 35;
                                                                    }
                                                         }
                                                 }
                                      }
                                      : row {
                                              : cancel_button {
                                                                key = "CANCEL";
                                                                label = "Cancel";
                                                              }
                                              : text {
                                                       key = "STAMINMAX";
                                                       value = "Minimum < STA < Maximum";
                                                       width = 30;
                                                     }
                                            }
                        }
             }