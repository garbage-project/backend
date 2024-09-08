package com.project.trash.member.response;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;

/**
 * 회원 상세 조회 응답
 */
@Getter
@Schema(title = "회원 상세 조회 응답")
public class MemberDetailResponse {

  /**
   * 닉네임
   */
  @Schema(description = "회원 닉네임", example = "Kim")
  private final String nickname;
  /**
   * 소셜 ID
   */
  @Schema(description = "소셜 ID", example = "XGJbTOt3U-Ahqghp9x61PFduxX_sGpk-M0ZdJSm5ZUc")
  private final String socialId;

  public MemberDetailResponse(String nickname, String socialId) {
    this.nickname = nickname;
    this.socialId = socialId;
  }
}
