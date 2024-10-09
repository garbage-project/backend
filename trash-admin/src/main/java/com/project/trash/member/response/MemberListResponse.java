package com.project.trash.member.response;

import com.project.trash.common.utils.DateTimeUtils;

import org.jooq.types.ULong;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import trash.tables.records.MemberRecord;

@Getter
@Setter
@Schema(title = "회원 목록 조회 응답")
public class MemberListResponse {

  @Schema(description = "회원 ID", example = "1")
  private ULong memberId;

  @Schema(description = "회원 닉네임", example = "Kim")
  private String nickname;

  @Schema(description = "회원 성별 (M/F)", example = "M")
  private String gender;

  @Schema(description = "회원 소셜 타입 (K - KAKAO, N - NAVER)", example = "K")
  private String socialType;

  @Schema(description = "회원 등록일자", example = "2024-09-01")
  private String createdDate;

  public MemberListResponse(MemberRecord member) {
    this.memberId = member.getMbrId();
    this.nickname = member.getMbrNckNm();
    this.gender = member.getMbrGndr();
    this.socialType = member.getMbrSclTyp();
    this.createdDate = DateTimeUtils.convertToString(member.getCreDtm(), DateTimeUtils.DEFAULT_DATE);
  }
}
