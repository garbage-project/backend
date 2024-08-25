package com.project.trash.facility.request;

import lombok.Getter;

/**
 * 신고 등록 요청
 */
@Getter
public class ReportEntryRequest {

  /**
   * 시설물 ID
   */
  private String facilityId;
  /**
   * 신고 내용
   */
  private String content;
}
