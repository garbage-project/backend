package com.project.trash.member.request;

import com.project.trash.common.request.PageRequest;

import io.swagger.v3.oas.annotations.Parameter;
import lombok.Getter;
import lombok.Setter;

/**
 * 등록한 리뷰 목록 조회
 */
@Getter
@Setter
public class MemberReviewListRequest extends PageRequest {

  /**
   * 회원 ID
   */
  @Parameter(description = "회원 ID", example = "1")
  private Long memberId;
}
