package com.project.trash.facility.request;

import lombok.Getter;

/**
 * 라뷰 등록 요청
 */
@Getter
public class ReviewEntryRequest {

  /**
   * 시설물 ID
   */
  private String facilityId;
  /**
   * 리뷰 내용
   */
  private String content;
}
