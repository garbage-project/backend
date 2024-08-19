package com.project.trash.facility.request;

import java.math.BigDecimal;
import java.util.List;

import lombok.Getter;

/**
 * 시설물 수정 요청
 */
@Getter
public class FacilityModifyRequest {

  /**
   * 시설물 ID
   */
  private String facilityId;
  /**
   * 시설물 종류
   */
  private String type;
  /**
   * 시설물명
   */
  private String name;
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
  /**
   * 이미지 인덱스 목록
   */
  private List<Integer> imageIndexes;
}
