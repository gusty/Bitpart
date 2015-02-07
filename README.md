# Bitpart

 - [Lingo](http://en.wikipedia.org/wiki/Lingo_(programming_language)) values parser
 - Lingo values serializer
 - Socket Server compatible with Shockwave Multiuser Server Protocol + DB Access
 

Adding this Bitpart to your Movie will provide integration with other movies, SQL Server and .NET applications.

Lingo Parser and Serializer
---------------------------

Support conversion of Lingo values between these formats:

 - Binary
 - Plain text (something like JSON for Javascript)
 - .NET Lingo values AST
 - .NET Standard types

 For more information see the test files.
 

Server
------

Unlimited connections, very good performance.

The low level protocol is 100% compatible with Adobe's [Shockwave Multiuser Server](https://www.adobe.com/support/director/multiuser.html) (SMUS) protocol. 

Movies can connect using the Mutiuser Xtra, applications using the classes provided in Bitpart.dll.

At high level only the basic set of commands of the SMUS are provided, the rest are not (yet) implemented and there are additional commands for:

 - Administration
 - Message validation, version control, anti-flood filter
 - DB access

To install it build the BitPart.Server project, copy the output directory to your server, there you will find Bitpart.Server.dll which is a plug-in for [SuperSocket](http://www.supersocket.net), just run SuperSocket.exe, adjust the config and select install as a service.
You can also build the Bitpart.Client and use it to manage your server remotely.



