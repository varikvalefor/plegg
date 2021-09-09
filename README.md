# `plegg`
`plegg` is a semi-cross-platform Haskell interface for OpenBSD's `pledge(2)` and `unveil(2)`.

`plegg` should compile and run fine on all `directory`-compatible operating systems.
## "Semi-Cross-Platform"
The term "semi-cross-platform" is used because although the functions which `plegg` exports use `pledge(2)` and `unveil(2)` on OpenBSD, these functions do absolutely nothing on other systems.
## Contributing
Contributions are welcomed but must be released in accordance with Plegg's extremely permissive licence.  Contributors must understand that contributors retain no rights to the source code which is submitted to this repository.

For all good changes, the diff of a good change can be pinged to Varik Valefor \<varikvalefor@aol.com\> such that the message's subject line includes the phrase "PLEGG CONTRIBUTION", or a pull request for this good change can be submitted to this GitHub repository.

For all Plegg issues, for all men, if a man wishes to fix a Matel issue, then this man should inform Varik Valefor via e-mail or Matrix at varikvalefor@aol.com or @varikalefor:matrix.org of this man's intent to fix this Plegg issue.  If VARIK is informed of this intent via e-mail, then the subject line of this e-mail should include the phrase "PLEGG ISSUE ASSIGNMENT REQUEST".
