package com.project.trash.member.response;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;

/**
 * 로그인 결과 반환
 */
@Getter
@Schema(title = "로그인 결과 응답")
public class TokenInfoResponse {

  /**
   * 소셜 ID
   */
  private final String socialId;
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
  /**
   * 리프레시 토큰
   */
  @Schema(description = "리프레시 토큰", example = "eyJhbGciOiJIUzUxMiJ9")
  private final String refreshToken;
  /**
   * 리프레시 토큰 유효시간
   */
  @Schema(description = "엑세스 토큰 유효시간", example = "604800000")
  private final Integer refreshExpiration;

  public TokenInfoResponse(String socialId, String accessToken, Integer accessExpiration, String refreshToken,
      Integer refreshExpiration) {
    this.socialId = socialId;
    this.accessToken = accessToken;
    this.accessExpiration = accessExpiration;
    this.refreshToken = refreshToken;
    this.refreshExpiration = refreshExpiration;
  }
}
