---
layout: markdown
---
## Security

Best Practice: Reporting a Security Issue in Erlang/OTP

## Summary

**Do not create a public github issue**.

Please create a new [Security Advisory](https://github.com/erlang/otp/security/security) for security issues. 
Alternatively send an email to erlang-security [at] erlang [dot] org.

Please follow this document in order to report security vulnerabilities in Erlang/OTP. 

## When should you report a security issue?

The risk level is often determined by a product of the impact once exploited, and the probability of exploitation occurring. In other words, if a bug can cause great damage, but it takes highest privilege to exploit the bug, then the bug is not a high risk one. Similarly, if the bug is easily exploitable, but its impact is limited, then it is not a high risk issue either.

There is not any hard and fast rule to determine if a bug is worth reporting as a security issue to [https://github.com/erlang/otp/security](https://github.com/erlang/otp/security). A general rule is that a bug which allows an unprivileged user to successfully attack the Erlang application, the Erlang runtime, or can be used as a springboard to attack other software running on the same or other machines is considered a security issue. As attacks we consider anything that affects the confidentiality, integrity and/or availability of the system.


## What happens after the report?

All security bugs in the Erlang/OTP distribution should be reported to [https://github.com/erlang/otp/security](https://github.com/erlang/otp/security). Your report will be handled by a small security team at the OTP team.

Please use a descriptive title for your report. After the initial response to your report, the security team will keep you updated on the progress and decision being made towards a fix and release announcement.

## Flagging Existing Issues as Security-related

If you believe that an existing public issue on [https://github.com/erlang/otp/issues](https://github.com/erlang/otp/issues) is security-related, we ask that you report it via [https://github.com/erlang/otp/security](https://github.com/erlang/otp/security). The title should contain the issue ID from [https://github.com/erlang/otp/issues](https://github.com/erlang/otp/issues) (e.g., flagging security issue [#7539](https://github.com/erlang/otp/issues/7539)). Please include a short description to motivate why it should be handled according to the security policy.
