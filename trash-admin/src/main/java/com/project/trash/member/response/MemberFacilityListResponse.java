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
public class MemberFacilityListResponse {

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
   * 설명
   */
  private String information;
  /**
   * 승인 상태
   */
  private String approvalStatus;
  /**
   * 승인일자
   */
  private String approvalDate;

  public MemberFacilityListResponse(String facilityId, String type, String name, String location, String detailLocation,
      String information, String approvalStatus) {
    this.facilityId = facilityId;
    this.type = type;
    this.name = name;
    this.location = location;
    this.detailLocation = detailLocation;
    this.information = information;
    this.approvalStatus = approvalStatus;
  }
}
