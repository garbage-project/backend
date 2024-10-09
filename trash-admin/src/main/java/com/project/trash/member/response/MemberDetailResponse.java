package com.project.trash.member.response;

import com.project.trash.common.utils.DateTimeUtils;
import com.project.trash.member.domain.Member;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Schema(title = "회원 상세 조회 응답")
public class MemberDetailResponse {

  @Schema(description = "회원 ID", example = "1")
  private Long memberId;

  @Schema(description = "이메일", example = "test1234@naver.com")
  private String email;

  @Schema(description = "회원 닉네임", example = "Kim")
  private String nickname;

  @Schema(description = "회원 성별 (M/F)", example = "M")
  private String gender;

  @Schema(description = "회원 소셜 타입 (K - KAKAO, N - NAVER)", example = "K")
  private String socialType;

  @Schema(description = "회원 등록일자", example = "2024-09-01")
  private String createdDate;

  public MemberDetailResponse(Member member) {
    this.memberId = member.getMemberId();
    this.email = member.getEmail();
    this.nickname = member.getNickname();
    this.gender = member.getGender().getCode();
    this.socialType = member.getSocialType().getCode();
    this.createdDate = DateTimeUtils.convertToString(member.getCreatedAt(), DateTimeUtils.DEFAULT_DATE);
  }
}
