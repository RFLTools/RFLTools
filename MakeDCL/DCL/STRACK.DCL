STRACK : dialog {
                  label = "SPECIAL TRACKWORK";
                  : column {
                             : row {
                                     : column {
                                                : boxed_radio_column {
                                                                       label = "Type :";
                                                                       : popup_list {
                                                                                      key = "TYPE";
                                                                                      list = "Left\nRight";
                                                                                    }
                                                                       : radio_button {
                                                                                        label = "Lateral";
                                                                                        key = "LATERAL";
                                                                                        value = "1";
                                                                                      }
                                                                       : radio_button {
                                                                                        label = "Equilateral";
                                                                                        key = "EQUILATERAL";
                                                                                        value = "0";
                                                                                      }
                                                                       : radio_button {
                                                                                        label = "Crossing";
                                                                                        key = "CROSSING";
                                                                                        value = "0";
                                                                                        is_enabled = "0";
                                                                                      }
                                                                     }
                                                : boxed_column {
                                                                 label = "Configuration :";
                                                                 : row {
                                                                         : column {
                                                                                    : radio_button {
                                                                                                     label = "A.R.E.A.";
                                                                                                     key = "AREA";
                                                                                                     value = "1";
                                                                                                  }
                                                                                    : radio_button {
                                                                                                     label = "European";
                                                                                                     key = "EURO";
                                                                                                     value = "0";
                                                                                                   }
                                                                                    : radio_button {
                                                                                                     label = "B.R.Std.";
                                                                                                     key = "BRIT";
                                                                                                     value = "0";
                                                                                                     is_enabled = "0";
                                                                                                   }
                                                                                  }
                                                                         : column {
                                                                                    : popup_list {
                                                                                                   key = "AREALIST";
                                                                                                   list = "No.4\nNo.5\nNo.6\nNo.7\nNo.8\nNo.9";
                                                                                                 }
                                                                                    : popup_list {
                                                                                                   key = "EUROLIST";
                                                                                                   list = "1:4\n1:5\n1:6\n1:6.25\n1:6.5\n1:7\n1:7.5\n1:8\n1:9\n1:10";
                                                                                                 }
                                                                                    : popup_list {
                                                                                                   key = "BRITLIST";
                                                                                                   is_enabled = "0";
                                                                                                 }
                                                                                  }
                                                                      }
                                                                 : button {
                                                                            label = "Set Dimensions";
                                                                            key = "DEFAULT";
                                                                            fixed_width = true;
                                                                            alignment = centered;
                                                                          }
                                                               }
                                              }
                                     : column {
                                                : boxed_column {
                                                                 label = "Dimensions (milimeters):";
                                                                 : edit_box {
                                                                              label = "T.O. Angle  :";
                                                                              key = "ANGLE";
                                                                              is_enabled = "0";
                                                                            }
                                                                 : edit_box {
                                                                              label = "Track Gauge :";
                                                                              key = "GAUGE";
                                                                              edit_width = 12;
                                                                            }
                                                                 : edit_box {
                                                                              label = "Switch Lead :";
                                                                              key = "LEAD";
                                                                              edit_width = 12;
                                                                            }
                                                                 : edit_box {
                                                                              label = "PS Position :";
                                                                              key = "POSITION";
                                                                              edit_width = 12;
                                                                            }
                                                                 : edit_box {
                                                                              label = "T.O. Radius :";
                                                                              key = "RADIUS";
                                                                              edit_width = 12;
                                                                            }
                                                                 : edit_box {
                                                                              label = "Heel Spread :";
                                                                              key = "SPREAD";
                                                                              edit_width = 12;
                                                                            }
                                                               }
                                                : boxed_row {
                                                              label = "Switch Machines :";
                                                              : column {
                                                                         : toggle {
                                                                                    label = "Switch :";
                                                                                    key = "SWITCH";
                                                                                    value = "1";
                                                                                  }
                                                                         : popup_list {
                                                                                        key = "SWITCHPOS";
                                                                                        list = "Left\nRight";
                                                                                      }
                                                                         : popup_list {
                                                                                        key = "SWITCHTYPE";
                                                                                        list = "Manual\nPowered";
                                                                                      }
                                                                       }
                                                              : column {
                                                                         : toggle {
                                                                                    label = "Frog :";
                                                                                    key = "FROG";
                                                                                    value = "1";
                                                                                  }
                                                                         : popup_list {
                                                                                        key = "FROGPOS";
                                                                                        list = "Left\nRight";
                                                                                      }
                                                                         : popup_list {
                                                                                        key = "FROGTYPE";
                                                                                        list = "Manual\nPowered";
                                                                                      }
                                                                       }
                                                            }
                                                      }
                                              }
                             : boxed_row {
                                           label = "Draw";
                                           : toggle {
                                                      label = "Lines";
                                                      key = "DRAWLINES";
                                                      value = "1";
                                                    }
                                           : toggle {
                                                      label = "P.I.s";
                                                      key = "DRAWPIS";
                                                      value = "1";
                                                    }
                                           : toggle {
                                                      label = "Circles";
                                                      key = "DRAWCIRCLES";
                                                      value = "1";
                                                    }
                                           : toggle {
                                                      label = "Arcs";
                                                      key = "DRAWARCS";
                                                      value = "1";
                                                    }
                                         }
                             : row {
                                     : ok_button {
                                                   label = "OK";
                                                   key = "ACCEPT";
                                                   is_default = true;
                                                 }
                                     : cancel_button {
                                                       label = "Cancel";
                                                       key = "CANCEL";
                                                     }
                                   }
                           }
                }
