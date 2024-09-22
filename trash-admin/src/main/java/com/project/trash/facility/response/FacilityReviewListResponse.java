package com.project.trash.facility.response;

import com.project.trash.common.utils.DateTimeUtils;

import org.jooq.types.ULong;

import java.time.LocalDateTime;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;

/**
 * 시설물 리뷰 목록 응답
 */
@Getter
@Setter
@Schema(title = "시설물의 리뷰 목록 조회 응답")
public class FacilityReviewListResponse {

  @Schema(description = "리뷰 ID", example = "1")
  private ULong reviewId;

  @Schema(description = "리뷰 내용", example = "시설물이 청결합니다~")
  private String content;

  @Schema(description = "리뷰 등록일자", example = "2024-09-01")
  private String createdDate;

  @Schema(description = "등록자 ID", example = "1")
  private ULong memberId;

  @Schema(description = "닉네임", example = "SBS")
  private String nickname;

  public FacilityReviewListResponse(ULong reviewId, String content, LocalDateTime createdAt, ULong memberId, String nickname) {
    this.reviewId = reviewId;
    this.content = content;
    this.createdDate = DateTimeUtils.convertToString(createdAt, DateTimeUtils.DEFAULT_DATE);
    this.memberId = memberId;
    this.nickname = nickname;
  }
}
