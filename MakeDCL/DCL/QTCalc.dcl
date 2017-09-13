QTCALC : dialog {
                  label = "QuickTurn Vehicle Turn Calculator";
                  : column {
                             alignment = centered;
                             : row {
                                     : boxed_column {
                                                      label = "Vehicle Width ";
                                                      : edit_box {
                                                                   key = "W";
                                                                   value = "2.600";
                                                                   edit_width = 5;
                                                                   width = 22;
                                                                 }
                                                    }
                                     : boxed_column {
                                                      label = "Wheel Base  ";
                                                      : edit_box {
                                                                   key = "WB";
                                                                   value = "12.000";
                                                                   edit_width = 5;
                                                                   width = 22;
                                                                 }
                                                    }
                                     :boxed_column {
                                                     label = "Lock Angle (degrees) ";
                                                     : edit_box {
                                                                  key = "L";
                                                                  value = "15";
                                                                  edit_width = 5;
                                                                  width = 22;
                                                                }
                                                   }
                                   }
                             : boxed_row {
                                           label = "Radius ";
                                           : edit_box {
                                                        label = "Inside :";
                                                        key = "RI";
                                                        value = "12.0";
                                                        edit_width = 5;
                                                      }
                                           : edit_box {
                                                        label = "Center :";
                                                        key = "RC";
                                                        value = "12.0";
                                                        edit_width = 5;
                                                      }
                                           : edit_box {
                                                        label = "Outside :";
                                                        key = "RO";
                                                        value = "12.0";
                                                        edit_width = 5;
                                                      }
                                         }
                             : row {
                                     : cancel_button {
                                                       label = "Cancel";
                                                       key = "CANCEL";
                                                     }
                                   }
                           }
                }