package com.project.trash.member.request;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;

@Getter
public class ReissueRequest {

  @Schema(description = "소셜 ID", example = "XGJbTOt3U-Ahqghp9x61PFduxX_sGpk-M0ZdJSm5ZUc")
  private String socialId;

  @Schema(description = "리프레시 토큰", example = "eyJhbGciOiJIUzUxMiJ9")
  private String refreshToken;
}
