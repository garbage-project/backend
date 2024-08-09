package com.project.trash.member.response;

import lombok.Getter;

/**
 * 회원 상세 조회 응답
 */
@Getter
public class MemberDetailResponse {

  /**
   * 닉네임
   */
  private final String nickname;
  /**
   * ID
   */
  private final String socialId;

  public MemberDetailResponse(String nickname, String socialId) {
    this.nickname = nickname;
    this.socialId = socialId;
  }
}
