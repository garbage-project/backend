package com.project.trash.member.response;

import com.project.trash.common.utils.DateTimeUtils;
import com.project.trash.member.domain.Member;

import lombok.Getter;
import lombok.Setter;

/**
 * 회원 상세 조회 응답
 */
@Getter
@Setter
public class MemberDetailResponse {

  /**
   * 회원 일련번호
   */
  private Long memberSeq;
  /**
   * 이메일
   */
  private String email;
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

  public MemberDetailResponse(Member member) {
    this.memberSeq = member.getMemberSeq();
    this.email = member.getEmail();
    this.name = member.getName();
    this.nickname = member.getNickname();
    this.birthday =
        DateTimeUtils.convertToString(DateTimeUtils.convertToDate(member.getBirthday(), DateTimeUtils.DEFAULT_DAY),
            DateTimeUtils.DEFAULT_DATE);
    this.gender = member.getGender().getCode();
    this.socialType = member.getSocialType().getCode();
    this.createdDate = DateTimeUtils.convertToString(member.getCreatedAt(), DateTimeUtils.DEFAULT_DATE);
  }
}
