package com.project.trash.member.response;

import org.springframework.data.annotation.Id;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

/**
 * 등록한 시설물 목록 응답
 */
@Getter
@Setter
@NoArgsConstructor
public class MyFacilityListResponse {

  /**
   * 시설물 ID
   */
  @Id
  private String facilityId;
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
   * 설명
   */
  private String information;

  public MyFacilityListResponse(String facilityId, String type, String location, String detailLocation,
      String information) {
    this.facilityId = facilityId;
    this.type = type;
    this.location = location;
    this.detailLocation = detailLocation;
    this.information = information;
  }
}
