package com.project.trash.member.response;

import com.project.trash.common.utils.DateTimeUtils;

import org.jooq.types.ULong;

import lombok.Getter;
import lombok.Setter;
import trash.tables.records.MemberRecord;

/**
 * 회원 목록 조회 응답
 */
@Getter
@Setter
public class MemberListResponse {

  /**
   * 회원 일련번호
   */
  private ULong memberSeq;
  /**
   * 이름
   */
  private String name;
  /**
   * 닉네임
   */
  private String nickname;
  /**
   * 생일
   */
  private String birthday;
  /**
   * 성별
   */
  private String gender;
  /**
   * 소셜 타입
   */
  private String socialType;
  /**
   * 등록일자
   */
  private String createdDate;

  public MemberListResponse(MemberRecord member) {
    this.memberSeq = member.getMbrSeq();
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
