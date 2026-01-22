# Hanfix-mode

## 한국어 (Korean)
Hanfix-mode는 한국어 문법 및 철자법 확인을 지원하는 Emacs 마이너
모드입니다. 내부적으로는 [hanfix](https://github.com/ntalbs/hanfix)
명령을 사용하거나 구글 제미니를
사용합니다. `hanfix-default-exec-mode`로 어느 것을 사용할 지 설정할 수
있습니다.

`hanfix` 명령어를 사용하려면 다음과 같이 `hanfix`를 설치해야 합니다.

```console
npm install hanfix -g
```

`hanfix` 명령은 내부적으로 [다음 맞춤법
검사기](https://dic.daum.net/grammar_checker.do)를
사용합니다. `hanfix` 명령을 사용하면 구글 제미니에 비해 상대적으로
빠르게 문법 및 맞춤법을 검사할 수 있지만, 검사 결과의 품질은 구글
제미니를 사용할 때보다 낮습니다.

구글 제미니를 사용하려면 [Google AI
Studio](https://aistudio.google.com/)에서 API 키를 발급 받아야 합니다.
구글 제미니를 사용하면 좀더 높은 품질의 문법 및 맞춤법 검사를 할 수
있지만, `hanfix` 명령어를 사용할 때보다 속도가 느리고, 사용상
제한(분당 최대 호출 회수, 하루 최대 호출 회수 등)이 있습니다. 자세한
사항은 [Gemini API
문서](https://ai.google.dev/gemini-api/docs/rate-limits)를 참고하시기
바랍니다.

## English

Hanfix-mode is an Emacs minor mode for checking grammar and spelling of
Korean text. It internally uses hanfix
[hanfix](https://github.com/ntalbs/hanfix) command or Google Gemini API
based on `hanfix-default-exec-mode`.

In order to use `hanfix` command, you need to install `hanfix` first.

```console
npm install hanfix -g
```

`hanfix` command relies on the [Daum Grammer
Checker](https://dic.daum.net/grammar_checker.do) for its
operations. Although it offers faster processing speeds than Google
Gemini, the output quality is not as high as Gemini's.


Accessing Google Gemini requires an API key from [Google AI
Studio](https://aistudio.google.com/). Gemini offers superior accuracy
in grammar and spell checking compared to the `hanfix` command;
however, it has slower response time and is subject to rate limits
(RPM, RPD, etc.) Please check [Gemini API
documentation](https://ai.google.dev/gemini-api/docs/rate-limits) for
the details on these limits.

![](images/hanfix-check.png?raw=true)
