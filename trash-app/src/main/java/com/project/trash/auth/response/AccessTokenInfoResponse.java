package com.project.trash.auth.response;

import lombok.Getter;

/**
 * 엑세스 토큰 정보 반환
 */
@Getter
public class AccessTokenInfoResponse {

  /**
   * 엑세스 토큰
   */
  private final String accessToken;
  /**
   * 엑세스 토큰 유효시간
   */
  private final Integer accessExpiration;

  public AccessTokenInfoResponse(String accessToken, Integer accessExpiration) {
    this.accessToken = accessToken;
    this.accessExpiration = accessExpiration;
  }
}
