package com.project.trash.member.request;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;

/**
 * 회원 리뷰 수정 요청
 */
@Getter
public class MemberReviewModifyRequest {

  /**
   * 리뷰 ID
   */
  @Schema(description = "리뷰 ID", example = "1")
  private Long reviewId;
  /**
   * 리뷰 내용
   */
  @Schema(description = "리뷰 내용", example = "시설물이 청결합니다~")
  private String content;
}
