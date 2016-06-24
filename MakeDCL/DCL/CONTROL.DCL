CONTROL : dialog {
                   label = "RFL Template Control Line Generator";
                   : row {
                           : column {
                                      : row {
                                              : edit_box {
                                                           key = "OS";
                                                           label = "O/S (+ve = right, -ve = left and mirrored)";
                                                           edit_width = 6;
                                                         }
                                            }
                                      : row {
                                              : edit_box {
                                                           key = "OS1";
                                                           label = "O/S1";
                                                           edit_width = 6;
                                                         }
                                            }
                                      : row {
                                              : edit_box {
                                                           key = "OS2";
                                                           label = "O/S2";
                                                           edit_width = 6;
                                                         }
                                            }
                                      : row {
                                              : edit_box {
                                                           key = "D1";
                                                           label = "D1 (+ve = down)";
                                                           edit_width = 6;
                                                         }
                                            }
                                      : row {
                                              : edit_box {
                                                           key = "D2";
                                                           label = "D2 (+ve = down)";
                                                           edit_width = 6;
                                                         }
                                            }
                                      : row {
                                              : text {
                                                       value = "D3";
                                                       width = 3;
                                                     }
                                              : radio_button {
                                                               key = "D3PROFILE";
                                                             }
                                              : text {
                                                       value = "Profile";
                                                       width = 5;
                                                     }
                                              : edit_box {
                                                           key = "D3PROFILEVALUE";
                                                           width_width = 4;
                                                         }
                                              : button {
                                                         key = "D3PROFILEPICK";
                                                         label = "Set";
                                                         width = 1;
                                                       }
                                              : text {
                                                       value = "Depth";
                                                       width = 5;
                                                     }
                                              : edit_box {
                                                           key = "D3DEPTHVALUE";
                                                           edit_width = 4;
                                                         }
                                              : radio_button {
                                                               key = "D3DEPTH";
                                                             }
                                            }
                                      : row {
                                              : text {
                                                       value = "D4";
                                                       width = 3;
                                                     }
                                              : radio_button {
                                                               key = "D4PROFILE";
                                                             }
                                              : text {
                                                       value = "Profile";
                                                       width = 5;
                                                     }
                                              : edit_box {
                                                           key = "D4PROFILEVALUE";
                                                           width_width = 4;
                                                         }
                                              : button {
                                                         key = "D4PROFILEPICK";
                                                         label = "Set";
                                                         width = 1;
                                                       }
                                              : text {
                                                       value = "Depth";
                                                       width = 5;
                                                     }
                                              : edit_box {
                                                           key = "D4DEPTHVALUE";
                                                           edit_width = 4;
                                                         }
                                              : radio_button {
                                                               key = "D4DEPTH";
                                                             }
                                            }
                                      : row {
                                              : text {
                                                       value = "SLOPE 1";
                                                       width = 7;
                                                     }
                                              : radio_button {
                                                               key = "SLOPE1SUPER";
                                                               label = "Super";
                                                             }
                                              : text {
                                                       value = "Fixed (%)";
                                                       width = 5;
                                                     }
                                              : edit_box {
                                                           key = "SLOPE1FIXEDVALUE";
                                                           edit_width = 6;
                                                         }
                                              : radio_button {
                                                               key = "SLOPE1FIXED";
                                                             }
                                            }
                                      : row {
                                              : text {
                                                       value = "SLOPE 2";
                                                       width = 7;
                                                     }
                                              : radio_button {
                                                               key = "SLOPE2SUPER";
                                                               label = "Super";
                                                             }
                                              : text {
                                                       value = "Fixed (%)";
                                                       width = 5;
                                                     }
                                              : edit_box {
                                                           key = "SLOPE2FIXEDVALUE";
                                                           edit_width = 6;
                                                         }
                                              : radio_button {
                                                               key = "SLOPE2FIXED";
                                                             }
                                            }
                                      : row {
                                              : text {
                                                       value = "SLOPE 3";
                                                       width = 7;
                                                     }
                                              : radio_button {
                                                               key = "SLOPE3SUPER";
                                                               label = "Super";
                                                             }
                                              : text {
                                                       value = "Fixed (%)";
                                                       width = 5;
                                                     }
                                              : edit_box {
                                                           key = "SLOPE3FIXEDVALUE";
                                                           edit_width = 6;
                                                         }
                                              : radio_button {
                                                               key = "SLOPE3FIXED";
                                                             }
                                            }
                                      : row {
                                              : text {
                                                       value = "SLOPE 4";
                                                       width = 7;
                                                     }
                                              : radio_button {
                                                               key = "SLOPE4SUPER";
                                                               label = "Super";
                                                             }
                                              : text {
                                                       value = "Fixed (%)";
                                                       width = 5;
                                                     }
                                              : edit_box {
                                                           key = "SLOPE4FIXEDVALUE";
                                                           edit_width = 6;
                                                         }
                                              : radio_button {
                                                               key = "SLOPE4FIXED";
                                                             }
                                            }
                                      : row {
                                              : text {
                                                       value = "SLOPE 5";
                                                       width = 7;
                                                     }
                                              : radio_button {
                                                               key = "SLOPE5FIXED";
                                                             }
                                              : text {
                                                       value = "Fixed";
                                                       width = 3;
                                                     }
                                              : edit_box {
                                                           key = "SLOPE5FIXEDVALUE";
                                                           edit_width = 6;
                                                         }
                                              : text {
                                                       value = "O/S";
                                                       width = 3;
                                                     }
                                              : edit_box {
                                                           key = "SLOPE5OSVALUE";
                                                           edit_width = 6;
                                                         }
                                              : radio_button {
                                                               key = "SLOPE5OS";
                                                             }
                                            }
                                      : row {
                                              : text {
                                                       value = "SLOPE 6";
                                                       width = 7;
                                                     }
                                              : radio_button {
                                                               key = "SLOPE6FIXED";
                                                             }
                                              : text {
                                                       value = "Fixed";
                                                       width = 3;
                                                     }
                                              : edit_box {
                                                           key = "SLOPE6FIXEDVALUE";
                                                           edit_width = 6;
                                                         }
                                              : text {
                                                       value = "O/S";
                                                       width = 3;
                                                     }
                                              : edit_box {
                                                           key = "SLOPE6OSVALUE";
                                                           edit_width = 6;
                                                         }
                                              : radio_button {
                                                               key = "SLOPE6OS";
                                                             }
                                            }
                                      : row {
                                              : text {
                                                       value = "SLOPE 7";
                                                       width = 7;
                                                     }
                                              : radio_button {
                                                               key = "SLOPE7FIXED";
                                                             }
                                              : text {
                                                       value = "Fixed";
                                                       width = 3;
                                                     }
                                              : edit_box {
                                                           key = "SLOPE7FIXEDVALUE";
                                                           edit_width = 6;
                                                         }
                                              : text {
                                                       value = "O/S";
                                                       width = 3;
                                                     }
                                              : edit_box {
                                                           key = "SLOPE7OSVALUE";
                                                           edit_width = 6;
                                                         }
                                              : radio_button {
                                                               key = "SLOPE7OS";
                                                             }
                                            }
                                      : row {
                                              : text {
                                                       value = "SLOPE 8";
                                                       width = 7;
                                                     }
                                              : radio_button {
                                                               key = "SLOPE8FIXED";
                                                             }
                                              : text {
                                                       value = "Fixed";
                                                       width = 3;
                                                     }
                                              : edit_box {
                                                           key = "SLOPE8FIXEDVALUE";
                                                           edit_width = 6;
                                                         }
                                              : text {
                                                       value = "O/S";
                                                       width = 3;
                                                     }
                                              : edit_box {
                                                           key = "SLOPE8OSVALUE";
                                                           edit_width = 6;
                                                         }
                                              : radio_button {
                                                               key = "SLOPE8OS";
                                                             }
                                            }
                                      : row {
                                              : text {
                                                       value = "W1";
                                                       width = 7;
                                                     }
                                              : radio_button {
                                                               key = "W1CENTER";
                                                               label = "To Center";
                                                             }
                                              : text {
                                                       value = "Fixed";
                                                       width = 3;
                                                     }
                                              : edit_box {
                                                           key = "W1FIXEDVALUE";
                                                           edit_width = 6;
                                                         }
                                              : radio_button {
                                                               key = "W1FIXED";
                                                             }
                                            }
                                      : row {
                                              : text {
                                                       value = "W2";
                                                       width = 7;
                                                     }
                                              : radio_button {
                                                               key = "W2CENTER";
                                                               label = "To Center";
                                                             }
                                              : text {
                                                       value = "Fixed";
                                                       width = 3;
                                                     }
                                              : edit_box {
                                                           key = "W2FIXEDVALUE";
                                                           edit_width = 6;
                                                         }
                                              : radio_button {
                                                               key = "W2FIXED";
                                                             }
                                            }
                                    }
                           : column {
                                      : image {
                                                key = "IMAGE";
                                                width = 99;
                                                height = 33;
                                                color = 0;
                                              }
                                    }
                         }
                   : boxed_row {
                                 label = "Draw control points";
                                 : toggle {
                                            label = "Pt. 1";
                                            key = "DRAW1";
                                            value = "1";
                                          }
                                 : toggle {
                                            label = "Pt. 2";
                                            key = "DRAW2";
                                            value = "1";
                                          }
                                 : toggle {
                                            label = "Pt. 3";
                                            key = "DRAW3";
                                            value = "1";
                                          }
                                 : toggle {
                                            label = "Pt. 4";
                                            key = "DRAW4";
                                            value = "1";
                                          }
                                 : toggle {
                                            label = "Pt. 5";
                                            key = "DRAW5";
                                            value = "1";
                                          }
                                 : toggle {
                                            label = "Pt. 6";
                                            key = "DRAW6";
                                            value = "1";
                                          }
                                 : toggle {
                                            label = "Pt. 7";
                                            key = "DRAW7";
                                            value = "1";
                                          }
                                 : toggle {
                                            label = "Pt. 8";
                                            key = "DRAW8";
                                            value = "1";
                                          }
                                 : toggle {
                                            label = "Pt. 9";
                                            key = "DRAW9";
                                            value = "1";
                                          }
                               }
                   : boxed_row {
                                 label = "Station information";
                                 : text {
                                          key = "STARTSTATIONTEXT";
                                          value = "Start station (> 0.000)";
                                          width = 10;
                                        }
                                 : edit_box {
                                              key = "STARTSTATIONVALUE";
                                              edit_width = 12;
                                            }
                                 spacer;
                                 : text {
                                          key = "ENDSTATIONTEXT";
                                          value = "End station (< 0.000)";
                                          width = 10;
                                        }
                                 : edit_box {
                                              key = "ENDSTATIONVALUE";
                                              edit_width = 12;
                                            }
                                 spacer;
                                 : edit_box {
                                              key = "STEPVALUE";
                                              label = "Step";
                                              edit_width = 6;
                                            }
                               }
                   : row {
                           : ok_button {
                                         label = "OK";
                                         key = "OK";
                                         is_default = true;
                                       }
                           : cancel_button {
                                             label = "Cancel";
                                             key = "CANCEL";
                                           }
                         }
                 }
