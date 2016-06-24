DAYLIGHT : dialog {
                    label = "RFL Daylight";
                    : boxed_row {
                                  label = "Sample Swath";
                                  : edit_box {
                                               key = "SWATH";
                                               label = "Swath width";
                                               edit_width = 10;
                                             }
                                }
                    : boxed_row {
                                  label = "Ditch";
                                  :column {
                                             : toggle {
                                                        key = "USEDITCH";
                                                        value = 1;
                                                        label = "Use ditch";
                                                      }
                                           }
                                  : column {
                                             : edit_box {
                                                          key = "DITCHBACK";
                                                          label = "Backslope";
                                                          edit_width = 5;
                                                        }
                                             : edit_box {
                                                          key = "DITCHWIDTH";
                                                          label = "Base width";
                                                          edit_width = 5;
                                                        }
                                             : edit_box {
                                                          key = "DITCHDEPTH";
                                                          label = "Depth";
                                                          edit_width = 5;
                                                        }
                                           }
                                }
                    : boxed_row {
                                  label = "Slopes";
                                  :column {
                                            : row {
                                                    : column {
                                                               width = 10;
                                                               spacer;
                                                               spacer;
                                                               spacer;
                                                               : text {
                                                                         value = "FILL";
                                                                      }
                                                               spacer;
                                                               spacer;
                                                               spacer;
                                                             }
                                                    : column {
                                                               width = 10;
                                                               : text {
                                                                        value = "Depth:";
                                                                        alignment = right;
                                                                      }
                                                               spacer;
                                                               : edit_box {
                                                                            key = "FD6";
                                                                            edit_width = 5;
                                                                          }
                                                               : edit_box {
                                                                            key = "FD4";
                                                                            edit_width = 5;
                                                                          }
                                                               : edit_box {
                                                                            key = "FD2";
                                                                            edit_width = 5;
                                                                          }
                                                               spacer;
                                                             }
                                                    : column {
                                                               width = 10;
                                                               : text {
                                                                        value = "Slope (X:1):";
                                                                        alignment = right;
                                                                      }
                                                               : edit_box {
                                                                            key = "FS7";
                                                                            edit_width = 5;
                                                                          }
                                                               : edit_box {
                                                                            key = "FS5";
                                                                            edit_width = 5;
                                                                          }
                                                               : edit_box {
                                                                            key = "FS3";
                                                                            edit_width = 5;
                                                                          }
                                                               : edit_box {
                                                                            key = "FS1";
                                                                            edit_width = 5;
                                                                          }
                                                             }
                                                  }
                                            : row {
                                                    : column {
                                                               width = 10;
                                                               : text {
                                                                        value = "---------------";
                                                                      }
                                                             }
                                                    : column {
                                                               width = 10;
                                                               : text {
                                                                        value = "0.000";
                                                                        alignment = right;
                                                                      }
                                                             }
                                                    : column {
                                                               width = 10;
                                                             }
                                                  }
                                            : row {
                                                    : column {
                                                               width = 10;
                                                               spacer;
                                                               : text {
                                                                         value = "CUT";
                                                                      }
                                                               spacer;
                                                             }
                                                    : column {
                                                               width = 10;
                                                               spacer;
                                                               : edit_box {
                                                                            key = "CD2";
                                                                            edit_width = 5;
                                                                          }
                                                               spacer;
                                                             }
                                                    : column {
                                                               width = 10;
                                                               : edit_box {
                                                                            key = "CS1";
                                                                            edit_width = 5;
                                                                          }
                                                               : edit_box {
                                                                            key = "CS3";
                                                                            edit_width = 5;
                                                                          }
                                                             }
                                                  }
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
