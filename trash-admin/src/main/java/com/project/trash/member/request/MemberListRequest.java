package com.project.trash.member.request;

import com.project.trash.common.request.PageRequest;

import lombok.Getter;
import lombok.Setter;

/**
 * 회원 목록 조회 요청
 */
@Getter
@Setter
public class MemberListRequest extends PageRequest {

  /**
   * 회원 일련번호
   */
  private Long memberSeq;
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
   * 생성일 검색 시작일
   */
  private String startDate;
  /**
   * 생성일 검색 종료일
   */
  private String endDate;
}
