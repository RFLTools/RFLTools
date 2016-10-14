DUALCAR : dialog {
                   label = "RFL Dual Carriageway 3DPoly Creator";
                   : boxed_row {
                                 label = "Centerline Control :";
                                 : edit_box {
                                              label = "Normal Crown (%) :";
                                              key = "NORMAL";
                                              edit_width = 5;
                                            }
                                 spacer;
                                 : edit_box {
                                              label = "Horizontal :";
                                              key = "CHOR";
                                              edit_width = 25;
                                            }
                                 : button {
                                            label = "Pick";
                                            key = "CHORPICK";
                                            width = 5;
                                          }
                                 spacer;
                                 : edit_box {
                                              label = "Vertical :";
                                              key = "CVER";
                                              edit_width = 25;
                                            }
                                 : button {
                                            label = "Pick";
                                            key = "CVERPICK";
                                            width = 5;
                                          }
                                 spacer;
                                 : edit_box {
                                              label = "Super :";
                                              key = "CSUPER";
                                              edit_width = 25;
                                            }
                                 : button {
                                            label = "Pick";
                                            key = "CSUPERPICK";
                                            width = 5;
                                          }
                         }
                   : row {
                           : image {
                                     key = "IMAGE";
                                     width = 180;
                                     height = 30;
                                     color = 0;
                                   }
                         }
                   : row {
                           : boxed_column {
                                            label = "Left Ditch :";
                                            : row {
                                                    : column {
                                                               : row {
                                                                       : radio_button {
                                                                                        key = "LDF";
                                                                                      }
                                                                       : edit_box {
                                                                                    label = "Fixed :";
                                                                                    edit_width = 4;
                                                                                    key = "LDFVAL";
                                                                                  }
                                                                       : radio_button {
                                                                                        key = "LDV";
                                                                                      }
                                                                       : edit_box {
                                                                                    label = "Variable :";
                                                                                    edit_width = 10;
                                                                                    key = "LDVVAL";
                                                                                  }
                                                                       : button {
                                                                                  label = "Pick";
                                                                                  key = "LDVPICK";
                                                                                }
                                                                     }
                                                               : row {
                                                                       : edit_box {
                                                                                    label = "Width :";
                                                                                    edit_width = 10;
                                                                                    key = "LDW";
                                                                                  }
                                                                       spacer;
                                                                       spacer;
                                                                       spacer;
                                                                       spacer;
                                                                       spacer;
                                                                       spacer;
                                                                       spacer;
                                                                       spacer;
                                                                     }
                                                             }
                                                  }
                                          }
                           : boxed_column {
                                            label = "Median Ditch :";
                                            : row {
                                                    : column {
                                                               : row {
                                                                       : radio_button {
                                                                                        key = "MDF";
                                                                                      }
                                                                       : edit_box {
                                                                                    label = "Fixed :";
                                                                                    edit_width = 4;
                                                                                    key = "MDFVAL";
                                                                                  }
                                                                       : radio_button {
                                                                                        key = "MDV";
                                                                                      }
                                                                       : edit_box {
                                                                                    label = "Variable :";
                                                                                    edit_width = 10;
                                                                                    key = "MDVVAL";
                                                                                  }
                                                                       : button {
                                                                                  label = "Pick";
                                                                                  key = "MDVPICK";
                                                                                }
                                                                     }
                                                               spacer;
                                                               spacer;
                                                               spacer;
                                                             }
                                                  }
                                          }
                           : boxed_column {
                                            label = "Right Ditch :";
                                            : row {
                                                    : column {
                                                               : row {
                                                                       : radio_button {
                                                                                        key = "RDF";
                                                                                      }
                                                                       : edit_box {
                                                                                    label = "Fixed :";
                                                                                    edit_width = 4;
                                                                                    key = "RDFVAL";
                                                                                  }
                                                                       : radio_button {
                                                                                        key = "RDV";
                                                                                      }
                                                                       : edit_box {
                                                                                    label = "Variable :";
                                                                                    edit_width = 10;
                                                                                    key = "RDVVAL";
                                                                                  }
                                                                       : button {
                                                                                  label = "Pick";
                                                                                  key = "RDVPICK";
                                                                                }
                                                                     }
                                                               : row {
                                                                       : edit_box {
                                                                                    label = "Width :";
                                                                                    edit_width = 10;
                                                                                    key = "RDW";
                                                                                  }
                                                                       spacer;
                                                                       spacer;
                                                                       spacer;
                                                                       spacer;
                                                                       spacer;
                                                                       spacer;
                                                                       spacer;
                                                                       spacer;
                                                                     }
                                                             }
                                                  }
                                          }
                         }
                   : row {
                           : boxed_column {
                                            label = "Left Carriageway :";
                                            : row {
                                                    : text {
                                                             value = "Structure Depths :";
                                                             width = 15;
                                                           }
                                                    : edit_box {
                                                                 label = "LD1 :";
                                                                 edit_width = 10;
                                                                 key = "LSD1";
                                                               }
                                                    : edit_box {
                                                                 label = "LD2 :";
                                                                 edit_width = 10;
                                                                 key = "LSD2";
                                                               }
                                                  }
                                            : row {
                                                    : text {
                                                              value = "Horizontal : L-OS-1";
                                                              width = 17;
                                                           }
                                                    : radio_button {
                                                                     key = "LCOSF";
                                                                   }
                                                    : edit_box {
                                                                 label = "Fixed :";
                                                                 edit_width = 5;
                                                                 key = "LCOSFVAL";
                                                               }
                                                    : radio_button {
                                                                     key = "LCOSV";
                                                                   }
                                                    : edit_box {
                                                                 label = "Variable :";
                                                                 edit_width = 15;
                                                                 key = "LCOSVVAL";
                                                               }
                                                    : button {
                                                               label = "Pick";
                                                               key = "LCOSVPICK";
                                                             }
                                                  }
                                            : row {
                                                    : text {
                                                             value = "Horizontal : L-SHLDR";
                                                             width = 17;
                                                           }
                                                    : radio_button {
                                                                     key = "LSOSF";
                                                                   }
                                                    : edit_box {
                                                                 label = "Fixed :";
                                                                 edit_width = 5;
                                                                 key = "LSOSFVAL";
                                                               }
                                                    : radio_button {
                                                                     key = "LSOSV";
                                                                   }
                                                    : edit_box {
                                                                 label = "Variable :";
                                                                 edit_width = 15;
                                                                 key = "LSOSVVAL";
                                                               }
                                                    : button {
                                                               label = "Pick";
                                                               key = "LSOSVPICK";
                                                             }
                                                  }
                                            : row {
                                                    : text {
                                                             value = "Horizontal : L-MDN";
                                                             width = 17;
                                                           }
                                                    : radio_button {
                                                                     key = "LMOSF";
                                                                   }
                                                    : edit_box {
                                                                 label = "Fixed :";
                                                                 edit_width = 5;
                                                                 key = "LMOSFVAL";
                                                               }
                                                    : radio_button {
                                                                     key = "LMOSV";
                                                                   }
                                                    : edit_box {
                                                                 label = "Variable :";
                                                                 edit_width = 15;
                                                                 key = "LMOSVVAL";
                                                               }
                                                    : button {
                                                               label = "Pick";
                                                               key = "LMOSVPICK";
                                                             }
                                                  }
                                            : row {
                                                    : text {
                                                             value = "Vertical :";
                                                             width = 8;
                                                           }
                                                           : radio_button {
                                                                            label = "Follow Centerline";
                                                                            key = "LVF";
                                                                          }
                                                           : radio_button {
                                                                            key = "LVV";
                                                                          }
                                                           : edit_box {
                                                                        label = "Independant :";
                                                                        edit_width = 15;
                                                                        key = "LVVVAL";
                                                                      }
                                                           : button {
                                                                      label = "Pick";
                                                                      key = "LVVPICK";
                                                                    }
                                                  }
                                            : row {
                                                    : text {
                                                             value = "Slopes (X:1) :";
                                                             width = 8;
                                                           }
                                                    : edit_box {
                                                                 label = " LS1";
                                                                 edit_width = 2;
                                                                 key = "LSLOPE1";
                                                               }
                                                    : edit_box {
                                                                 label = " LS2";
                                                                 edit_width = 2;
                                                                 key = "LSLOPE2";
                                                               }
                                                    : edit_box {
                                                                 label = " LS3";
                                                                 edit_width = 2;
                                                                 key = "LSLOPE3";
                                                               }
                                                    : edit_box {
                                                                 label = " LS4";
                                                                 edit_width = 2;
                                                                 key = "LSLOPE4";
                                                               }
                                                    : edit_box {
                                                                 label = " LS5";
                                                                 edit_width = 2;
                                                                 key = "LSLOPE5";
                                                               }
                                                    : edit_box {
                                                                 label = " LS6";
                                                                 edit_width = 2;
                                                                 key = "LSLOPE6";
                                                               }
                                                  }
                                            : row {
                                                    : text {
                                                             value = "Pivot Point :";
                                                             width = 8;
                                                           }
                                                           : radio_button {
                                                                            label = "L4";
                                                                            key = "LPIVOT4";
                                                                          }
                                                           : radio_button {
                                                                            label = "L5";
                                                                            key = "LPIVOT5";
                                                                          }
                                                           : radio_button {
                                                                            label = "L6";
                                                                            key = "LPIVOT6";
                                                                          }
                                                  }
                                          }
                           : boxed_column {
                                            label = "Right Carriageway :";
                                            : row {
                                                    : text {
                                                             value = "Structure Depths :";
                                                             width = 15;
                                                           }
                                                    : edit_box {
                                                                 label = "RD1 :";
                                                                 edit_width = 10;
                                                                 key = "RSD1";
                                                               }
                                                    : edit_box {
                                                                 label = "RD2 :";
                                                                 edit_width = 10;
                                                                 key = "RSD2";
                                                               }
                                                  }
                                            : row {
                                                    : text {
                                                              value = "Horizontal : R-OS-1";
                                                              width = 17;
                                                           }
                                                    : radio_button {
                                                                     key = "RCOSF";
                                                                   }
                                                    : edit_box {
                                                                 label = "Fixed :";
                                                                 edit_width = 5;
                                                                 key = "RCOSFVAL";
                                                               }
                                                    : radio_button {
                                                                     key = "RCOSV";
                                                                   }
                                                    : edit_box {
                                                                 label = "Variable :";
                                                                 edit_width = 15;
                                                                 key = "RCOSVVAL";
                                                               }
                                                    : button {
                                                               label = "Pick";
                                                               key = "RCOSVPICK";
                                                             }
                                                  }
                                            : row {
                                                    : text {
                                                             value = "Horizontal : R-SHLDR";
                                                             width = 17;
                                                           }
                                                    : radio_button {
                                                                     key = "RSOSF";
                                                                   }
                                                    : edit_box {
                                                                 label = "Fixed :";
                                                                 edit_width = 5;
                                                                 key = "RSOSFVAL";
                                                               }
                                                    : radio_button {
                                                                     key = "RSOSV";
                                                                   }
                                                    : edit_box {
                                                                 label = "Variable :";
                                                                 edit_width = 15;
                                                                 key = "RSOSVVAL";
                                                               }
                                                    : button {
                                                               label = "Pick";
                                                               key = "RSOSVPICK";
                                                             }
                                                  }
                                            : row {
                                                    : text {
                                                             value = "Horizontal : R-MDN";
                                                             width = 17;
                                                           }
                                                    : radio_button {
                                                                     key = "RMOSF";
                                                                   }
                                                    : edit_box {
                                                                 label = "Fixed :";
                                                                 edit_width = 5;
                                                                 key = "RMOSFVAL";
                                                               }
                                                    : radio_button {
                                                                     key = "RMOSV";
                                                                   }
                                                    : edit_box {
                                                                 label = "Variable :";
                                                                 edit_width = 15;
                                                                 key = "RMOSVVAL";
                                                               }
                                                    : button {
                                                               label = "Pick";
                                                               key = "RMOSVPICK";
                                                             }
                                                  }
                                            : row {
                                                    : text {
                                                             value = "Vertical :";
                                                             width = 8;
                                                           }
                                                           : radio_button {
                                                                            label = "Follow Centerline";
                                                                            key = "RVF";
                                                                          }
                                                           : radio_button {
                                                                            key = "RVV";
                                                                          }
                                                           : edit_box {
                                                                        label = "Independant :";
                                                                        edit_width = 15;
                                                                        key = "RVVVAL";
                                                                      }
                                                           : button {
                                                                      label = "Pick";
                                                                      key = "RVVPICK";
                                                                    }
                                                  }
                                            : row {
                                                    : text {
                                                             value = "Slopes (X:1) :";
                                                             width = 8;
                                                           }
                                                    : edit_box {
                                                                 label = " RS1";
                                                                 edit_width = 2;
                                                                 key = "RSLOPE1";
                                                               }
                                                    : edit_box {
                                                                 label = " RS2";
                                                                 edit_width = 2;
                                                                 key = "RSLOPE2";
                                                               }
                                                    : edit_box {
                                                                 label = " RS3";
                                                                 edit_width = 2;
                                                                 key = "RSLOPE3";
                                                               }
                                                    : edit_box {
                                                                 label = " RS4";
                                                                 edit_width = 2;
                                                                 key = "RSLOPE4";
                                                               }
                                                    : edit_box {
                                                                 label = " RS5";
                                                                 edit_width = 2;
                                                                 key = "RSLOPE5";
                                                               }
                                                    : edit_box {
                                                                 label = " RS6";
                                                                 edit_width = 2;
                                                                 key = "RSLOPE6";
                                                               }
                                                  }
                                            : row {
                                                    : text {
                                                             value = "Pivot Point :";
                                                             width = 8;
                                                           }
                                                           : radio_button {
                                                                            label = "R4";
                                                                            key = "RPIVOT4";
                                                                          }
                                                           : radio_button {
                                                                            label = "R5";
                                                                            key = "RPIVOT5";
                                                                          }
                                                           : radio_button {
                                                                            label = "R6";
                                                                            key = "RPIVOT6";
                                                                          }
                                                  }
                                          }
                         }
                   : row {
                           : boxed_column {
                                            label = "Draw Left :";
                                            : row {
                                                    : toggle {
                                                               label = "1";
                                                               key = "DL1";
                                                             }
                                                    : toggle {
                                                               label = "2";
                                                               key = "DL2";
                                                             }
                                                    : toggle {
                                                               label = "3";
                                                               key = "DL3";
                                                             }
                                                    : toggle {
                                                               label = "4";
                                                               key = "DL4";
                                                             }
                                                    : toggle {
                                                               label = "5";
                                                               key = "DL5";
                                                             }
                                                    : toggle {
                                                               label = "6";
                                                               key = "DL6";
                                                             }
                                                    : toggle {
                                                               label = "7";
                                                               key = "DL7";
                                                             }
                                                    : toggle {
                                                               label = "8";
                                                               key = "DL8";
                                                             }
                                                    : toggle {
                                                               label = "9";
                                                               key = "DL9";
                                                             }
                                                    : toggle {
                                                               label = "10";
                                                               key = "DL10";
                                                             }
                                                    : toggle {
                                                               label = "11";
                                                               key = "DL11";
                                                             }
                                                    : toggle {
                                                               label = "12";
                                                               key = "DL12";
                                                             }
                                                        }
                                          }
                           : boxed_column {
                                            label = "Draw Right :";
                                            : row {
                                                    : toggle {
                                                               label = "1";
                                                               key = "DR1";
                                                             }
                                                    : toggle {
                                                               label = "2";
                                                               key = "DR2";
                                                             }
                                                    : toggle {
                                                               label = "3";
                                                               key = "DR3";
                                                             }
                                                    : toggle {
                                                               label = "4";
                                                               key = "DR4";
                                                             }
                                                    : toggle {
                                                               label = "5";
                                                               key = "DR5";
                                                             }
                                                    : toggle {
                                                               label = "6";
                                                               key = "DR6";
                                                             }
                                                    : toggle {
                                                               label = "7";
                                                               key = "DR7";
                                                             }
                                                    : toggle {
                                                               label = "8";
                                                               key = "DR8";
                                                             }
                                                    : toggle {
                                                               label = "9";
                                                               key = "DR9";
                                                             }
                                                    : toggle {
                                                               label = "10";
                                                               key = "DR10";
                                                             }
                                                    : toggle {
                                                               label = "11";
                                                               key = "DR11";
                                                             }
                                                    : toggle {
                                                               label = "12";
                                                               key = "DR12";
                                                             }
                                                        }
                                          }
                         }
                   : row {
                           : boxed_row {
                                         label = "From-To :";
                                         : toggle {
                                                    label = "Begin";
                                                    key = "FROMTOBEGIN";
                                                  }
                                         : edit_box {
                                                      label = "Start Station : ";
                                                      edit_width = 10;
                                                      key = "FROMTOSTARTVAL";
                                                    }
                                         : toggle {
                                                    label = "End";
                                                    key = "FROMTOEND";
                                                  }
                                         : edit_box {
                                                      label = "End Station : ";
                                                      edit_width = 10;
                                                      key = "FROMTOENDVAL";
                                                    }
                                         : edit_box {
                                                      label = "Step size : ";
                                                      edit_width = 10;
                                                      key = "FROMTOSTEP";
                                                    }
                                         : toggle {
                                                    label = "To File";
                                                    key = "FROMTOFILE";
                                                  }
                                       }
                           : boxed_row {
                                         label = "Save Settings";
                                         : button {
                                                    label = "Write";
                                                    key = "SSW";
                                                  }
                                         : button {
                                                    label = "Read";
                                                    key = "SSR";
                                                  }
                                       }
                           : column {
                                      : cancel_button {
                                                        label = "Cancel";
                                                        key = "CANCEL";
                                                        width = 12;
                                                      }
                                      : ok_button {
                                                    label = "OK";
                                                    key = "OK";
                                                    is_default = true;
                                                    width = 12;
                                                  }
                                    }
                         }
                 }
