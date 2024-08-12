package com.project.trash.facility.request;

import java.math.BigDecimal;

import lombok.Getter;

/**
 * 시설물 등록 요청
 */
@Getter
public class FacilityEntryRequest {

  /**
   * 시설물 종류
   */
  private String type;
  /**
   * 위치
   */
  private String location;
  /**
   * 상세 위치
   */
  private String detailLocation;
  /**
   * 위도
   */
  private BigDecimal latitude;
  /**
   * 경도
   */
  private BigDecimal longitude;
  /**
   * 정보
   */
  private String information;
}
