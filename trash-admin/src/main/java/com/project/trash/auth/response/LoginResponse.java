package com.project.trash.auth.response;

import lombok.Getter;

/**
 * 로그인 결과 반환
 */
@Getter
public class LoginResponse {

  /**
   * ID
   */
  private final String id;
  /**
   * 엑세스 토큰
   */
  private final String accessToken;
  /**
   * 엑세스 토큰 유효시간
   */
  private final Integer accessExpiration;
  /**
   * 리프레시 토큰
   */
  private final String refreshToken;
  /**
   * 리프레시 토큰 유효시간
   */
  private final Integer refreshExpiration;

  public LoginResponse(String id, String accessToken, Integer accessExpiration, String refreshToken,
      Integer refreshExpiration) {
    this.id = id;
    this.accessToken = accessToken;
    this.accessExpiration = accessExpiration;
    this.refreshToken = refreshToken;
    this.refreshExpiration = refreshExpiration;
  }
}
