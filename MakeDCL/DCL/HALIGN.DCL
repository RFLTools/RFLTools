HALIGN : dialog {
                  label = "RFL Horizontal Alignment Routines";
                  initial_focus = "OK";
                  : column {
                             : boxed_row {
                                           label = "Define";
                                           : button {
                                                      label = "From Screen";
                                                      key = "GALIGN";
                                                      width = 24;
                                                    }
                                           : button {
                                                      label = "From File";
                                                      key = "RALIGN";
                                                      width = 24;
                                                    }
                                           : button {
                                                      label = "From EMXS";
                                                      key = "HOR2ALIGN";
                                                      width = 24;
                                                    }
                                         }
                             : boxed_row {
                                           label = "Save";
                                           : button {
                                                      label = "To File";
                                                      key = "WALIGN";
                                                      width = 24;
                                                    }
                                           : button {
                                                      label = "To EMXS";
                                                      key = "ALIGN2HOR";
                                                      width = 24;
                                                    }
                                           : spacer {
                                                      width = 24;
                                                    }
                                         }
                             : boxed_row {
                                           label = "Draw";
                                           : button {
                                                      label = "To Screen";
                                                      key = "DALIGN";
                                                      width = 24;
                                                    }
                                           : spacer {
                                                      width = 24;
                                                    }
                                           : spacer {
                                                      width = 24;
                                                    }
                                         }
                             : boxed_row {
                                           label = "Label";
                                           : column {
                                                      : row {
                                                              : button {
                                                                         label = "To Screen";
                                                                         key = "LALIGN";
                                                                         width = 24;
                                                                       }
                                                            }
                                                      : row {
                                                              : spacer {
                                                                         width = 7;
                                                                       }
                                                              : button {
                                                                         label = "Setup";
                                                                         key = "LALIGNSETUP";
                                                                         width = 10;
                                                                       }
                                                              : spacer {
                                                                         width = 7;
                                                                       }
                                                            }
                                                    }
                                           : column {
                                                      : row {
                                                              : button {
                                                                         label = "EMXS To Table";
                                                                         key = "HOR2TABLE";
                                                                         width = 24;
                                                                       }
                                                            }
                                                      : row {
                                                              : button {
                                                                         label = "Big";
                                                                         key = "MAKEBIG";
                                                                         width = 8;
                                                                       }
                                                              : button {
                                                                         label = "Med";
                                                                         key = "MAKEMEDIUM";
                                                                         width = 8;
                                                                       }
                                                              : button {
                                                                         label = "Sml";
                                                                         key = "MAKESMALL";
                                                                         width = 8;
                                                                       }
                                                            }
                                                    }
                                           : column {
                                                      : row {
                                                              : button {
                                                                         label = "N/E/Sta/Elev";
                                                                         key = "NE";
                                                                         width = 24;
                                                                       }
                                                            }
                                                      : row {
                                                              : spacer {
                                                                         width = 7;
                                                                       }
                                                              : button {
                                                                         label = "Angle";
                                                                         key = "NEWANG";
                                                                         width = 10;
                                                                       }
                                                              : spacer {
                                                                         width = 7;
                                                                       }
                                                            }
                                                    }
                                         }
                             : row {
                                     : cancel_button {
                                                       key = "CANCEL";
                                                       label = "Cancel";
                                                     }
                                   }
                           }
                }
