When UIClicker is compiled with "TestBuild" compiler directive (see Project Options -> Custom Options, the "-dTestBuild" option), various extra features are available, like extra caption for main window, or setting execution mode on startup:
--ExtraCaption <MyCaption>     - this will add <MyCaption> to the main window title. For example, running with --ExtraCaption abc  , will result in the 'UI Clicker Main - abc' window title.
--SetExecMode <Mode>           - this will set the execution mode. The Mode argument can be Local, Client or Server.
--ServerPort <Port>            - this will set a custom port, to listen to. If "--ServerPort" is not passed, the default value is used.

The test application connects to this instance (the driver instance(s)), which controls the UIClicker-under-test instances.
The UIClicker-under-test instances are started and stopped by the TestDriver instance(s).
When testing local mode, it is enough to have a single TestDriver instance. For testing client-server, two instances of TestDriver will be needed.
Both instances run initially in server mode and listens on 15444.  UIClicker-under-test listens on 5444.

1. The driver instance executes a SetVar action on UIClicker-under-test to get the window handles.  The result is a #7#8 separated list of name=value pairs, which cannot be parsed:  $frmUIClickerMainForm_Handle$=337770$frmClickerControlPreview_Handle$=272500$frmClickerActions_Handle$=336718
2. The driver instance controls the UIClicker-under-test to switch from server mode to local mode.