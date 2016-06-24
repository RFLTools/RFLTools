OFFSET : dialog {
                  label = "RFL 3D Polyline Offset";
                  : radio_row {
                                : radio_button {
                                                 key = "RADIO_FIXED";
                                                 label = "Fixed offset";
                                                 value = "1";
                                               }
                                : radio_button {
                                                  key = "RADIO_SLOPED";
                                                  label = "Sloped offset";
                                                  value = "0";
                                               }
                                : radio_button {
                                                  key = "RADIO_ELEV";
                                                  label = "Elev offset";
                                                  value = "0";
                                               }
                              }
                  : boxed_row {
                                label = "Fixed";
                                : column {
                                           width = 25;
                                           : edit_box {
                                                        key = "D1FIXED";
                                                        label = "D1 : ";
                                                        edit_width = 6;
                                                        is_enabled = true;
                                                      }
                                           : edit_box {
                                                        key = "D2FIXED";
                                                        label = "D2 : ";
                                                        edit_width = 6;
                                                        is_enabled = true;
                                                      }
                                           spacer;
                                           spacer;
                                           spacer;
                                           spacer;
                                           spacer;
                                         }
                                : column {
                                           : image {
                                                     key = "IMAGE1";
                                                     width = 30;
                                                     height = 10;
                                                     color = 0;
                                                   }
                                         }
                              }
                  : boxed_row {
                                label = "Sloped";
                                : column {
                                           width = 25;
                                           : edit_box {
                                                        key = "D1ASLOPED";
                                                        label = "D1a : ";
                                                        edit_width = 6;
                                                        is_enabled = false;
                                                      }
                                           : edit_box {
                                                        key = "D1BSLOPED";
                                                        label = "D1b : ";
                                                        edit_width = 6;
                                                        is_enabled = false;
                                                      }
                                           : row {
                                                   : radio_column {
                                                                    : radio_button {
                                                                                     key = "USES1SLOPED";
                                                                                     value = "1";
                                                                                   }
                                                                    : radio_button {
                                                                                     key = "USESUPER";
                                                                                     value = "0";
                                                                                   }
                                                                  }
                                                   : column {
                                                              : edit_box {
                                                                           key = "S1SLOPED";
                                                                           label = "S1 (%) : ";
                                                                           edit_width = 6;
                                                                           is_enabled = false;
                                                                         }
                                                              : edit_box {
                                                                           key = "RFLSLOPED";
                                                                           label = "Use super, CL OS : ";
                                                                           edit_width = 6;
                                                                           is_enabled = false;
                                                                         }
                                                            }
                                                 }
                                           : edit_box {
                                                        key = "D2SLOPED";
                                                               label = "D2 : ";
                                                               edit_width = 6;
                                                               is_enabled = false;
                                                      }
                                           : edit_box {
                                                        key = "S2SLOPED";
                                                        label = "S2 (X:1) : ";
                                                        edit_width = 6;
                                                        is_enabled = false;
                                                      }
                                         }
                                : column {
                                           : image {
                                                     key = "IMAGE2";
                                                     width = 30;
                                                     height = 10;
                                                     color = 0;
                                                   }
                                         }
                              }
                  : boxed_row {
                                label = "Elev";
                                : column {
                                           width = 25;
                                           : edit_box {
                                                        key = "S1ELEV";
                                                        label = "S1 (X:1) : ";
                                                        edit_width = 6;
                                                        is_enabled = false;
                                                      }
                                           : edit_box {
                                                        key = "S1ELEVF";
                                                        label = "Fixed : ";
                                                        edit_width = 6;
                                                        is_enabled = false;
                                                      }
                                           : text {
                                                    key = "S1ELEVT";
                                                    value = "Enter 'VRT' for prof elev";
                                                  }
                                           spacer;
                                           spacer;
                                           spacer;
                                           spacer;
                                           spacer;
                                         }
                                : column {
                                           : image {
                                                     key = "IMAGE3";
                                                     width = 30;
                                                     height = 10;
                                                     color = 0;
                                                   }
                                         }
                              }
                  : row {
                          : ok_button {
                                        label = "OK";
                                        key = "OK";
                                        is_default = true;
                                      }
                          : text {
                                   value = "Note: +ve = arrow direction.";
                                   alignment = centered;
                                   width = 20;
                                 }
                          : cancel_button {
                                            label = "Cancel";
                                            key = "CANCEL";
                                          }
                        }
                }
