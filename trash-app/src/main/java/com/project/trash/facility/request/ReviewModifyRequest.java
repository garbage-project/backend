package com.project.trash.facility.request;

import lombok.Getter;

/**
 * 라뷰 수정 요청
 */
@Getter
public class ReviewModifyRequest {

  /**
   * 리뷰 일련번호
   */
  private Long reviewSeq;
  /**
   * 리뷰 내용
   */
  private String content;
}
