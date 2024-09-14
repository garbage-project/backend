package com.project.trash.member.response;

import com.project.trash.common.utils.DateTimeUtils;

import org.jooq.types.ULong;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import trash.tables.records.MemberRecord;

/**
 * 회원 목록 조회 응답
 */
@Getter
@Setter
@Schema(title = "회원 목록 조회 응답")
public class MemberListResponse {

  /**
   * 회원 ID
   */
  @Schema(description = "회원 ID", example = "1")
  private ULong memberId;
  /**
   * 이름
   */
  @Schema(description = "회원 이름", example = "김회원")
  private String name;
  /**
   * 닉네임
   */
  @Schema(description = "회원 닉네임", example = "Kim")
  private String nickname;
  /**
   * 생일
   */
  @Schema(description = "회원 생일 (yyyy-MM-dd)", example = "1990-01-01")
  private String birthday;
  /**
   * 성별
   */
  @Schema(description = "회원 성별 (M/F)", example = "M")
  private String gender;
  /**
   * 소셜 타입
   */
  @Schema(description = "회원 소셜 타입 (K - KAKAO, N - NAVER)", example = "K")
  private String socialType;
  /**
   * 등록일자
   */
  @Schema(description = "회원 등록일자", example = "2024-09-01")
  private String createdDate;

  public MemberListResponse(MemberRecord member) {
    this.memberId = member.getMbrId();
    this.name = member.getMbrNm();
    this.nickname = member.getMbrNckNm();
    this.birthday =
        DateTimeUtils.convertToString(DateTimeUtils.convertToDate(member.getMbrBrdt(), DateTimeUtils.DEFAULT_DAY),
            DateTimeUtils.DEFAULT_DATE);
    this.gender = member.getMbrGndr();
    this.socialType = member.getMbrSclTyp();
    this.createdDate = DateTimeUtils.convertToString(member.getCreDtm(), DateTimeUtils.DEFAULT_DATE);
  }
}
