package com.project.trash.member.request;

import com.project.trash.common.request.PageRequest;

import io.swagger.v3.oas.annotations.Parameter;
import lombok.Getter;
import lombok.Setter;

/**
 * 회원 목록 조회 요청
 */
@Getter
@Setter
public class MemberListRequest extends PageRequest {

  /**
   * 회원 ID
   */
  @Parameter(description = "회원 ID", example = "1")
  private Long memberId;
  /**
   * 이름
   */
  @Parameter(description = "회원 이름", example = "김회원")
  private String name;
  /**
   * 닉네임
   */
  @Parameter(description = "회원 닉네임", example = "Kim")
  private String nickname;
  /**
   * 생일
   */
  @Parameter(description = "회원 생일 (yyyy-MM-dd)", example = "1990-01-01")
  private String birthday;
  /**
   * 성별
   */
  @Parameter(description = "회원 성별 (M/F)", example = "M")
  private String gender;
  /**
   * 소셜 타입
   */
  @Parameter(description = "회원 소셜 타입 (K - KAKAO, N - NAVER)", example = "K")
  private String socialType;
  /**
   * 생성일 검색 시작일
   */
  @Parameter(description = "회원 생성일 검색 시작일 (yyyy-MM-dd)", example = "2024-06-01")
  private String startDate;
  /**
   * 생성일 검색 종료일
   */
  @Parameter(description = "회원 생성일 검색 종료일 (yyyy-MM-dd)", example = "2024-09-08")
  private String endDate;
}
