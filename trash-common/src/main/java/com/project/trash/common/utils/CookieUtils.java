package com.project.trash.common.utils;

import org.springframework.http.ResponseCookie;

import java.util.Objects;

import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.experimental.UtilityClass;

@UtilityClass
public class CookieUtils {

  /**
   * 쿠키 읽기
   *
   * @param request 요청
   * @param name    쿠키 이름
   * @return 쿠키 값
   */
  public String getCookie(HttpServletRequest request, String name) {
    final Cookie[] cookies = request.getCookies();
    if (Objects.isNull(cookies)) {
      System.out.println("cookies null");
      return null;
    }

    for (Cookie cookie : cookies) {
      System.out.println("cookie name: " + cookie.getName() + ", value: " + cookie.getValue());
      if (cookie.getName().equals(name)) {
        return cookie.getValue();
      }
    }

    return null;
  }

  /**
   * 쿠키 생성
   *
   * @param name     쿠키 이름
   * @param value    쿠키 값
   * @param maxAge   저장 기간
   * @param response 응답
   */
  public void setCookie(String name, String value, Integer maxAge, HttpServletResponse response) {
    ResponseCookie cookie =
        ResponseCookie.from(name, value).path("/").sameSite("None").httpOnly(true).secure(true).maxAge(maxAge).build();

    response.addHeader("Set-Cookie", cookie.toString());
  }
}
