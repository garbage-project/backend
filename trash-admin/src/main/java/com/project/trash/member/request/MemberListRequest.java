package com.project.trash.member.request;

import com.project.trash.common.request.PageRequest;

import io.swagger.v3.oas.annotations.Parameter;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class MemberListRequest extends PageRequest {

  @Parameter(description = "회원 ID", example = "1")
  private Long memberId;

  @Parameter(description = "회원 닉네임", example = "Kim")
  private String nickname;

  @Parameter(description = "회원 성별 (M/F)", example = "M")
  private String gender;

  @Parameter(description = "회원 소셜 타입 (K - KAKAO, N - NAVER)", example = "K")
  private String socialType;

  @Parameter(description = "회원 생성일 검색 시작일 (yyyy-MM-dd)", example = "2024-06-01")
  private String startDate;

  @Parameter(description = "회원 생성일 검색 종료일 (yyyy-MM-dd)", example = "2024-09-08")
  private String endDate;
}
