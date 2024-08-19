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
   * 시설물 위치
   */
  private String location;
  /**
   * 리뷰 내용
   */
  private String content;
  /**
   * 등록일자
   */
  private String createdDate;

  public MyReviewListResponse(Long reviewSeq, String location, String content, LocalDateTime createdDate) {
    this.reviewSeq = reviewSeq;
    this.location = location;
    this.content = content;
    this.createdDate = DateTimeUtils.convertToString(createdDate, DateTimeUtils.DEFAULT_DATE);
  }
}
