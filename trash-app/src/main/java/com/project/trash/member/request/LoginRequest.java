package com.project.trash.member.request;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;

/**
 * 로그인 요청
 */
@Getter
public class LoginRequest {

  /**
   * 소셜 타입
   */
  @Schema(description = "회원 소셜 타입 (K - KAKAO, N - NAVER)", example = "N")
  private String socialType;
  /**
   * 엑세스 토큰
   */
  @Schema(description = "엑세스 토큰", example = "AAAAOQyhkOCIHqxBRocc56YEJ5txff0nDZxWURIixTjytcKFsQcQgCv-y6MiC8ObOrmiJfWHMwFn-Y6JsXYm-oiMXrE")
  private String accessToken;
  /**
   * 소셜 ID
   */
  @Schema(description = "소셜 ID", example = "XGJbTOt3U-Ahqghp9x61PFduxX_sGpk-M0ZdJSm5ZUc")
  private String socialId;
}
