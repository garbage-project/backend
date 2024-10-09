package com.project.trash.member.response;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;

@Getter
@Schema(title = "엑세스 토큰 재발급 응답")
public class AccessTokenInfoResponse {

  @Schema(description = "엑세스 토큰", example = "eyJhbGciOiJIUzUxMiJ9")
  private final String accessToken;

  @Schema(description = "엑세스 토큰 유효시간", example = "86400000")
  private final Long accessExpiration;

  public AccessTokenInfoResponse(String accessToken, Long accessExpiration) {
    this.accessToken = accessToken;
    this.accessExpiration = accessExpiration;
  }
}
