package com.project.trash.member.request;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;

@Getter
public class MemberReviewModifyRequest {

  @Schema(description = "리뷰 ID", example = "1")
  private Long reviewId;

  @Schema(description = "리뷰 내용", example = "시설물이 청결합니다~")
  private String content;
}
