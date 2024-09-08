package com.project.trash.member.response;

import com.project.trash.common.utils.DateTimeUtils;

import java.time.LocalDateTime;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;

/**
 * 등록한 시설물 목록 응답
 */
@Getter
@Setter
@Schema(title = "로그인 회원이 등록한 리뷰 목록 조회 응답")
public class MyReviewListResponse {

  /**
   * 리뷰 일련번호
   */
  @Schema(description = "리뷰 일련번호", example = "1")
  private Long reviewSeq;
  /**
   * 시설물명
   */
  @Schema(description = "시설물명", example = "쌍문역 내 화장실")
  private String facilityName;
  /**
   * 리뷰 내용
   */
  @Schema(description = "리뷰 내용", example = "시설물이 청결합니다~")
  private String content;
  /**
   * 등록일자
   */
  @Schema(description = "리뷰 등록일자", example = "2024-09-01")
  private String createdDate;

  public MyReviewListResponse(Long reviewSeq, String facilityName, String content, LocalDateTime createdAt) {
    this.reviewSeq = reviewSeq;
    this.facilityName = facilityName;
    this.content = content;
    this.createdDate = DateTimeUtils.convertToString(createdAt, DateTimeUtils.DEFAULT_DATE);
  }
}
