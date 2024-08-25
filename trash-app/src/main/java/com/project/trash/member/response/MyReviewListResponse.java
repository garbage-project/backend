package com.project.trash.member.response;

import com.project.trash.common.utils.DateTimeUtils;

import java.time.LocalDateTime;

import lombok.Getter;
import lombok.Setter;

/**
 * 등록한 시설물 목록 응답
 */
@Getter
@Setter
public class MyReviewListResponse {

  /**
   * 리뷰 일련번호
   */
  private Long reviewSeq;
  /**
   * 시설물명
   */
  private String facilityName;
  /**
   * 리뷰 내용
   */
  private String content;
  /**
   * 등록일자
   */
  private String createdDate;

  public MyReviewListResponse(Long reviewSeq, String facilityName, String content, LocalDateTime createdAt) {
    this.reviewSeq = reviewSeq;
    this.facilityName = facilityName;
    this.content = content;
    this.createdDate = DateTimeUtils.convertToString(createdAt, DateTimeUtils.DEFAULT_DATE);
  }
}
