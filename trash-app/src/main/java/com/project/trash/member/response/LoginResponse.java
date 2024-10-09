package com.project.trash.member.response;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;

@Getter
@Schema(title = "로그인 결과 응답")
public class LoginResponse {

  @Schema(description = "소셜 ID", example = "XGJbTOt3U-Ahqghp9x61PFduxX_sGpk-M0ZdJSm5ZUc")
  private final String socialId;

  @Schema(description = "약관 동의 여부(Y/N)", example = "Y")
  private final String agreementYn;

  @Schema(description = "엑세스 토큰", example = "eyJhbGciOiJIUzUxMiJ9")
  private final String accessToken;

  @Schema(description = "엑세스 토큰 유효시간", example = "86400000")
  private final Long accessExpiration;

  @Schema(description = "리프레시 토큰", example = "eyJhbGciOiJIUzUxMiJ9")
  private final String refreshToken;

  @Schema(description = "엑세스 토큰 유효시간", example = "604800000")
  private final Long refreshExpiration;

  public LoginResponse(String socialId, String agreementYn, String accessToken, Long accessExpiration, String refreshToken,
      Long refreshExpiration) {
    this.socialId = socialId;
    this.agreementYn = agreementYn;
    this.accessToken = accessToken;
    this.accessExpiration = accessExpiration;
    this.refreshToken = refreshToken;
    this.refreshExpiration = refreshExpiration;
  }
}
