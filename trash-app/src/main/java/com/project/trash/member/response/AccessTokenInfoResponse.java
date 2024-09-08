package com.project.trash.member.response;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;

/**
 * 엑세스 토큰 재발급 반환
 */
@Getter
@Schema(title = "엑세스 토큰 재발급 응답")
public class AccessTokenInfoResponse {

  /**
   * 엑세스 토큰
   */
  @Schema(description = "엑세스 토큰", example = "eyJhbGciOiJIUzUxMiJ9")
  private final String accessToken;
  /**
   * 엑세스 토큰 유효시간
   */
  @Schema(description = "엑세스 토큰 유효시간", example = "86400000")
  private final Integer accessExpiration;

  public AccessTokenInfoResponse(String accessToken, Integer accessExpiration) {
    this.accessToken = accessToken;
    this.accessExpiration = accessExpiration;
  }
}
